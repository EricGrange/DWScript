{**************************************************************************}
{                                                                          }
{    This Source Code Form is subject to the terms of the Mozilla Public   }
{    License, v. 2.0. If a copy of the MPL was not distributed with this   }
{     file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                          }
{    Software distributed under the License is distributed on an           }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express           }
{    or implied. See the License for the specific language                 }
{    governing rights and limitations under the License.                   }
{                                                                          }
{    Copyright Eric Grange / Creative IT                                   }
{                                                                          }
{**************************************************************************}
unit dwsJITx86_64;

{$I ../dws.inc}

interface
{$ifdef WIN64}

{.$define ALIGN_JUMP_TARGETS}

uses
   Classes, SysUtils, Math, Windows,
   dwsExprs, dwsSymbols, dwsErrors, dwsUtils, dwsExprList, dwsXPlatform,
   dwsCoreExprs, dwsRelExprs, dwsMagicExprs, dwsConstExprs,
   dwsMathFunctions, dwsDataContext, dwsConvExprs, dwsSetOfExprs, dwsMethodExprs,
   dwsJIT, dwsJITFixups, dwsJITAllocatorWin, dwsJITx86Intrinsics, dwsVMTOffsets,
   dwsWin64FunctionTables;

type

   TRegisterStatus = record
      Contains : TObject;
      Lock : Integer;
      procedure Flush; inline;
      function ContainsDataSymbol : TDataSymbol;
   end;

   TFixupAlignedTarget = class(TFixupTarget)
      public
         function  GetSize : Integer; override;
         procedure Write(output : TWriteOnlyBlockStream); override;
         function JumpLocation : Integer; override;
   end;

   TFixupJump = class(TFixupTargeting)
      private
         FFlags : TboolFlags;
         FLongJump : Boolean;

      protected
         function NearJump : Boolean;

      public
         constructor Create(flags : TboolFlags);

         function Optimize : TFixupOptimizeAction; override;

         function  GetSize : Integer; override;
         procedure Write(output : TWriteOnlyBlockStream); override;
   end;

   TFixupPreamble = class(TFixup)
      private
         FAllocatedStackSpace : Integer;
         FPreserveExec : Boolean;
         FHasCalls : Boolean;
         FUnwind : TUnwindCodeBuilder;
         FAlteredXMM : Cardinal;

         procedure Prepare;

      public
         constructor Create;
         destructor Destroy; override;

         function Optimize : TFixupOptimizeAction; override;
         function  GetSize : Integer; override;
         procedure Write(output : TWriteOnlyBlockStream); override;

         function AllocateStackSpace(bytes : Integer) : Integer;
         procedure NotifyAlteration(xmm : TxmmRegister);

         function StackTotalSpace : Integer;
         function NeedRBP : Boolean; inline;

         property AllocatedStackSpace : Integer read FAllocatedStackSpace write FAllocatedStackSpace;
         property PreserveExec : Boolean read FPreserveExec write FPreserveExec;
         property HasCalls : Boolean read FHasCalls write FHasCalls;

         property Unwind : TUnwindCodeBuilder read FUnwind;
   end;

   TFixupPostamble = class(TFixup)
      private
         FPreamble : TFixupPreamble;

      public
         constructor Create(preamble : TFixupPreamble);

         function  GetSize : Integer; override;
         procedure Write(output : TWriteOnlyBlockStream); override;
   end;

   TStaticDataFixup = class(TFixup)
      private
         FPrefixBytes : TBytes;
         FDataIndex : Integer;

      public
         constructor Create(const aPrefixBytes : TBytes);

         function GetSize : Integer; override;
         procedure Write(output : TWriteOnlyBlockStream); override;

         property DataIndex : Integer read FDataIndex write FDataIndex;
         property PrefixBytes : TBytes read FPrefixBytes;
         function RelativeOffset : Integer;
   end;

   Tx86_64FixupLogic = class (TFixupLogic)
      private
         FOptions : TdwsJITOptions;
         FStaticData : array of UInt64;
         FStaticDataBase : Integer;
         FStaticDataFree8BytesSlot : Integer;

      protected
         procedure AfterResolve; override;

      public
         procedure ClearFixups; override;

         function NewHangingTarget(align : Boolean) : TFixupTarget; override;

         property Options : TdwsJITOptions read FOptions write FOptions;

         function AddStaticData(const data : UInt64) : Integer;
         function AddStaticData128(const data1, data2 : UInt64) : Integer;
         property StaticDataBase : Integer read FStaticDataBase write FStaticDataBase;
         function CompileStaticData : TBytes;
   end;

   TxmmFlushOption = (xmmFlushAll, xmmFlushVolatile, xmmFlushIntermediateExprs);

   Tx86_64FixupLogicHelper = class helper for TFixupLogic
      function NewJump(flags : TboolFlags) : TFixupJump; overload;
      function NewJump(flags : TboolFlags; target : TFixup) : TFixupJump; overload;
      function NewJump(target : TFixup) : TFixupJump; overload;
      procedure NewConditionalJumps(flagsTrue : TboolFlags; targetTrue, targetFalse : TFixup);

      function NewMovsdRegImm(reg : TxmmRegister; const value : Double) : TStaticDataFixup;
      function NewComisdRegImm(reg : TxmmRegister; const value : Double) : TStaticDataFixup;
      function NewNegRegImm(reg : TxmmRegister) : TStaticDataFixup;
      function NewOpRegImm(op : TxmmOp; reg : TxmmRegister; const value : Double) : TStaticDataFixup;

      function AddStaticData(const data : UInt64) : Integer;
      function AddStaticData128(const data1, data2 : UInt64) : Integer;
      function StaticData : TBytes;

      function GetStaticDataBase : Integer;
      procedure SetStaticDataBase(v : Integer);
      property StaticDataBase : Integer read GetStaticDataBase write SetStaticDataBase;
   end;

   TdwsJITx86_64 = class (TdwsJIT)
      private
         FRegs : array [TxmmRegister] of TRegisterStatus;
         FRegsStackAddr : array [TxmmRegister] of Integer;
         FSavedXMM : TxmmRegisters;

         FPreamble : TFixupPreamble;
         FPostamble : TFixupPostamble;
         x86 : Tx86_64_WriteOnlyStream;

         FAllocator : TdwsJITAllocatorWin;

         FInterpretedJITter : TdwsJITter;

      protected
         function CreateOutput : Tx86BaseWriteOnlyStream; override;
         function CreateFixupLogic : TFixupLogic; override;

         procedure ResetXMMReg;

         procedure StartJIT(expr : TExprBase; exitable : Boolean); override;
         procedure EndJIT; override;
         procedure EndFloatJIT(resultHandle : Integer); override;
         procedure EndIntegerJIT(resultHandle : Integer); override;

      public
         constructor Create; override;
         destructor Destroy; override;

         function  AllocXMMReg(expr : TExprBase) : TxmmRegister;
         procedure ReleaseXMMReg(reg : TxmmRegister);
         procedure FlushXMMRegs(option : TxmmFlushOption);
         function  CurrentXMMReg(expr : TExprBase) : TxmmRegister;
         procedure SetContainsXMMReg(reg : TxmmRegister; expr : TExprBase);
         procedure SaveXMMRegs(firstReg : TxmmRegister = xmm0);
         procedure RestoreXMMRegs(firstReg : TxmmRegister = xmm0);

         function StackAddrOfFloat(expr : TTypedExpr) : Integer;

         property Allocator : TdwsJITAllocatorWin read FAllocator write FAllocator;

         function CompileFloat(expr : TTypedExpr) : TxmmRegister; inline;
         procedure CompileAssignFloat(expr : TTypedExpr; source : TxmmRegister); inline;

         procedure CompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);

         function CompiledOutput : TdwsJITCodeBlock; override;

         procedure _xmm_reg_expr(op : TxmmOp; dest : TxmmRegister; expr : TTypedExpr);
         procedure _comisd_reg_expr(dest : TxmmRegister; expr : TTypedExpr);

         procedure _movsd_reg_imm(dest : TxmmRegister; const value : Double);

         function _store_rax : Integer;
         procedure _restore_rax(addr : Integer);

         procedure _mov_reg_execInstance(reg : TgpRegister64);
         procedure _mov_reg_execStatus(reg : TgpRegister64);
         procedure _mov_execStatus_imm(value : Int32);
         procedure _DoStep(expr : TExprBase);
//         procedure _RangeCheck(expr : TExprBase; reg : TgpRegister64;
//                               delta, miniInclusive, maxiExclusive : Integer);
   end;

   TdwsJITCodeBlock86_64 = class (TdwsJITCodeBlock)
      private
         FRuntimeFunction : PRUNTIME_FUNCTION;
         FRegistered : Boolean;

      public
         constructor Create(const aCodePtr : Pointer; const aSubAllocator : IdwsJITCodeSubAllocator); override;
         destructor Destroy; override;

         procedure RegisterTable(rtFn : PRUNTIME_FUNCTION);
   end;

   TProgramExpr86 = class (TJITTedProgramExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TFloatExpr86 = class (TJITTedFloatExpr)
      public
         function  EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;

   TIntegerExpr86 = class (TJITTedIntegerExpr)
      public
         function  EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   TdwsJITter_x86 = class (TdwsJITter)
      private
         FJIT : TdwsJITx86_64;
         Fx86 : Tx86_64_WriteOnlyStream;

      protected
         property jit : TdwsJITx86_64 read FJIT;
         property x86 : Tx86_64_WriteOnlyStream read Fx86;

      public
         constructor Create(jit : TdwsJITx86_64);

         function CompileFloat(expr : TTypedExpr) : Integer; override; final;
         function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; virtual;

         procedure CompileAssignFloat(expr : TTypedExpr; source : Integer); override; final;
         procedure DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister); virtual;

         function  CompileBooleanValue(expr : TTypedExpr) : Integer; override;
         procedure CompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override; final;
         procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); virtual;
   end;

   Tx86ConstFloat = class (TdwsJITter_x86)
      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
   end;
   Tx86ConstInt = class (TdwsJITter_x86)
      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
      function CompileInteger(expr : TTypedExpr) : Integer; override;
   end;
   Tx86ConstBoolean = class (TdwsJITter_x86)
      function  CompileInteger(expr : TTypedExpr) : Integer; override;
      function  CompileBooleanValue(expr : TTypedExpr) : Integer; override;
      procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
   end;

   Tx86InterpretedExpr = class (TdwsJITter_x86)
      procedure DoCallEval(expr : TExprBase; vmt : Integer);

      procedure CompileStatement(expr : TExprBase); override;

      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
      procedure DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister); override;

      function CompileInteger(expr : TTypedExpr) : Integer; override;
      procedure CompileAssignInteger(expr : TTypedExpr; source : Integer); override;

      procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
      function CompileBooleanValue(expr : TTypedExpr) : Integer; override;
   end;

   Tx86FloatVar = class (TdwsJITter_x86)
      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
      procedure DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister); override;
   end;
   Tx86IntVar = class (TdwsJITter_x86)
      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
      function CompileInteger(expr : TTypedExpr) : Integer; override;
      procedure CompileAssignInteger(expr : TTypedExpr; source : Integer); override;
   end;
//   Tx86BoolVar = class (TdwsJITter_x86)
//      procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
//      procedure CompileAssignBoolean(expr : TTypedExpr; source : Integer); override;
//   end;
//   Tx86ObjectVar = class (TdwsJITter_x86)
//      function CompileScriptObj(expr : TTypedExpr) : Integer; override;
//   end;
//   Tx86RecordVar = class (TdwsJITter_x86)
//      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
//      procedure DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister); override;
//      function CompileInteger(expr : TTypedExpr) : Integer; override;
//      procedure CompileAssignInteger(expr : TTypedExpr; source : Integer); override;
//      procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
//   end;
//   Tx86VarParam = class (TdwsJITter_x86)
//      class procedure CompileAsPVariant(x86 : Tx86_64_WriteOnlyStream; expr : TByRefParamExpr);
//      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
//      function CompileInteger(expr : TTypedExpr) : Integer; override;
//      procedure DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister); override;
//      procedure CompileAssignInteger(expr : TTypedExpr; source : Integer); override;
//   end;
//
//   Tx86FieldVar = class (Tx86InterpretedExpr)
//      procedure CompileToData(expr : TFieldVarExpr; dest : TgpRegister64);
//      function CompileInteger(expr : TTypedExpr) : Integer; override;
//      procedure CompileAssignInteger(expr : TTypedExpr; source : Integer); override;
//      procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
//   end;

   Tx86ArrayBase = class (Tx86InterpretedExpr)
      procedure CompileIndexToGPR(indexExpr : TTypedExpr; gpr : TgpRegister64; var delta : Integer);
   end;
//   Tx86StaticArray = class (Tx86ArrayBase)
//      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
//      function CompileInteger(expr : TTypedExpr) : Integer; override;
//      procedure DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister); override;
//      procedure CompileAssignInteger(expr : TTypedExpr; source : Integer); override;
//   end;
//   Tx86DynamicArrayBase = class (Tx86ArrayBase)
//      procedure CompileAsData(expr : TTypedExpr);
//   end;
//   Tx86DynamicArray = class (Tx86DynamicArrayBase)
//      procedure CompileAsItemPtr(expr : TDynamicArrayExpr; var delta : Integer);
//
//      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
//      function CompileInteger(expr : TTypedExpr) : Integer; override;
//      function CompileScriptObj(expr : TTypedExpr) : Integer; override;
//   end;
//   Tx86DynamicArraySet = class (Tx86DynamicArrayBase)
//      procedure CompileStatement(expr : TExprBase); override;
//   end;

   Tx86AssignConstToFloatVar = class (TdwsJITter_x86)
      procedure CompileStatement(expr : TExprBase); override;
   end;
   Tx86AssignConstToIntegerVar = class (Tx86InterpretedExpr)
      procedure CompileStatement(expr : TExprBase); override;
   end;
//   Tx86AssignConstToBoolVar = class (Tx86InterpretedExpr)
//      procedure CompileStatement(expr : TExprBase); override;
//   end;

   Tx86Assign = class (Tx86InterpretedExpr)
      procedure CompileStatement(expr : TExprBase); override;
   end;
//   Tx86AssignData = class (Tx86InterpretedExpr)
//      procedure CompileStatement(expr : TExprBase); override;
//   end;

//   Tx86OpAssignFloat = class (TdwsJITter_x86)
//      public
//         OP : TxmmOp;
//         constructor Create(jit : TdwsJITx86_64; op : TxmmOp);
//         procedure CompileStatement(expr : TExprBase); override;
//   end;

   Tx86Null = class (TdwsJITter_x86)
      procedure CompileStatement(expr : TExprBase); override;
   end;

   Tx86BlockExprNoTable = class (TdwsJITter_x86)
      procedure CompileStatement(expr : TExprBase); override;
   end;

   Tx86IfThen = class (TdwsJITter_x86)
      procedure CompileStatement(expr : TExprBase); override;
   end;
   Tx86IfThenElse = class (TdwsJITter_x86)
      procedure CompileStatement(expr : TExprBase); override;
   end;

   Tx86Loop = class (TdwsJITter_x86)
      procedure CompileStatement(expr : TExprBase); override;
   end;
   Tx86Repeat = class (Tx86Loop)
      procedure CompileStatement(expr : TExprBase); override;
   end;
//   Tx86While = class (Tx86Loop)
//      procedure CompileStatement(expr : TExprBase); override;
//   end;

   Tx86ForUpward = class (TdwsJITter_x86)
      procedure CompileStatement(expr : TExprBase); override;
   end;

//   Tx86Continue = class (TdwsJITter_x86)
//      procedure CompileStatement(expr : TExprBase); override;
//   end;
//   Tx86Break = class (TdwsJITter_x86)
//      procedure CompileStatement(expr : TExprBase); override;
//   end;
//   Tx86Exit = class (TdwsJITter_x86)
//      procedure CompileStatement(expr : TExprBase); override;
//   end;
//   Tx86ExitValue = class (Tx86Exit)
//      procedure CompileStatement(expr : TExprBase); override;
//   end;

   Tx86FloatBinOp = class (TdwsJITter_x86)
      public
         OP : TxmmOp;
         constructor Create(jit : TdwsJITx86_64; op : TxmmOp);
         function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
   end;
   Tx86SqrFloat = class (TdwsJITter_x86)
      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
      function CompileFloatOperand(sqrExpr, operand : TTypedExpr) : TxmmRegister;
   end;
   Tx86NegFloat = class (TdwsJITter_x86)
      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
   end;

   Tx86NegInt = class (TdwsJITter_x86)
      function CompileInteger(expr : TTypedExpr) : Integer; override;
   end;
   Tx86NotInt = class (TdwsJITter_x86)
      function CompileInteger(expr : TTypedExpr) : Integer; override;
   end;

   Tx86MultInt = class (TdwsJITter_x86)
      function CompileInteger(expr : TTypedExpr) : Integer; override;
   end;
{   Tx86MultIntPow2 = class (TdwsJITter_x86)
      function CompileInteger(expr : TTypedExpr) : Integer; override;
   end;
   Tx86DivInt = class (Tx86InterpretedExpr)
      function CompileInteger(expr : TTypedExpr) : Integer; override;
   end;
   Tx86ModInt = class (Tx86InterpretedExpr)
      function CompileInteger(expr : TTypedExpr) : Integer; override;
   end;
   Tx86ModFloat = class (TdwsJITter_x86)
      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
   end;  }
   Tx86IntegerBinOpExpr = class (TdwsJITter_x86)
      FOp : TgpOp;
      FCommutative : Boolean;
      constructor Create(jit : TdwsJITx86_64; const anOp : TgpOP; commutative : Boolean = True);
      function CompileInteger(expr : TTypedExpr) : Integer; override;
      procedure CompileConstantOperand(expr : TTypedExpr; const val : Int64);
      procedure CompileVarOperand(expr : TTypedExpr; const varStackAddr : Integer);
   end;

   Tx86Shr = class (TdwsJITter_x86)
      function CompileInteger(expr : TTypedExpr) : Integer; override;
   end;
   Tx86Shl = class (TdwsJITter_x86)
      function CompileInteger(expr : TTypedExpr) : Integer; override;
   end;

   Tx86Inc = class (Tx86InterpretedExpr)
      procedure DoCompileStatement(v : TIntVarExpr; i : TTypedExpr);
   end;
   Tx86IncIntVar = class (Tx86Inc)
      procedure CompileStatement(expr : TExprBase); override;
   end;
{   Tx86IncVarFunc = class (Tx86Inc)
      procedure CompileStatement(expr : TExprBase); override;
   end;
   Tx86Dec = class (Tx86InterpretedExpr)
      procedure DoCompileStatement(v : TIntVarExpr; i : TTypedExpr);
   end;
   Tx86DecIntVar = class (Tx86Dec)
      procedure CompileStatement(expr : TExprBase); override;
   end;
   Tx86DecVarFunc = class (Tx86Dec)
      procedure CompileStatement(expr : TExprBase); override;
   end;
}
   Tx86RelOpInt = class (TdwsJITter_x86)
      public
         FlagsPass : TboolFlags;
         constructor Create(jit : TdwsJITx86_64; flagsPass : TboolFlags);
         procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
   end;
   Tx86RelEqualInt = class (Tx86RelOpInt)
      constructor Create(jit : TdwsJITx86_64);
   end;
   Tx86RelNotEqualInt = class (Tx86RelOpInt)
      constructor Create(jit : TdwsJITx86_64);
   end;
   Tx86RelIntIsZero = class (TdwsJITter_x86)
      procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
   end;
   Tx86RelIntIsNotZero = class (Tx86RelIntIsZero)
      procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
   end;

   Tx86RelOpFloat = class (TdwsJITter_x86)
      public
         Flags : TboolFlags;
         constructor Create(jit : TdwsJITx86_64; flags : TboolFlags);
         procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
   end;
{
   Tx86NotExpr = class (TdwsJITter_x86)
      procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override; final;
   end;}
   Tx86BoolOrExpr = class (TdwsJITter_x86)
      procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
   end;
{   Tx86BoolAndExpr = class (TdwsJITter_x86)
      procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
   end;

   Tx86SetOfExpr = class (TdwsJITter_x86)
      procedure NormalizeEnumOperand(setTyp : TSetOfSymbol; operand : TTypedExpr; targetOutOfRange : TFixup);
      procedure ComputeFromValueInEAXOffsetInECXMaskInEDX(setTyp : TSetOfSymbol);
   end;
   Tx86SetOfInExpr = class (Tx86SetOfExpr)
      procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
   end;
   Tx86SetOfFunction = class (Tx86SetOfExpr)
      procedure CompileStatement(expr : TExprBase); override;
      procedure DoByteOp(reg : TgpRegister64; offset : Integer; mask : Byte); virtual; abstract;
      procedure DoWordOp(dest, src : TgpRegister64); virtual; abstract;
   end;
   Tx86SetOfInclude = class (Tx86SetOfFunction)
      procedure DoByteOp(reg : TgpRegister64; offset : Integer; mask : Byte); override;
      procedure DoWordOp(dest, src : TgpRegister64); override;
   end;
   Tx86SetOfExclude = class (Tx86SetOfFunction)
      procedure DoByteOp(reg : TgpRegister64; offset : Integer; mask : Byte); override;
      procedure DoWordOp(dest, src : TgpRegister64); override;
   end;

   Tx86OrdBool = class (Tx86InterpretedExpr)
      function CompileInteger(expr : TTypedExpr) : Integer; override;
   end;
   Tx86OrdInt = class (Tx86InterpretedExpr)
      function CompileInteger(expr : TTypedExpr) : Integer; override;
   end;
}
   Tx86ConvIntToFloat = class (TdwsJITter_x86)
      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
   end;

   Tx86MagicFunc = class (Tx86InterpretedExpr)
      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
      function CompileInteger(expr : TTypedExpr) : Integer; override;
      procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
      function CompileBooleanValue(expr : TTypedExpr) : Integer; override;
   end;

   Tx86MagicBoolFunc = class (Tx86MagicFunc)
      function CompileInteger(expr : TTypedExpr) : Integer; override;
   end;
{
   Tx86DirectCallFunc = class (Tx86InterpretedExpr)
      public
         AddrPtr : PPointer;
         constructor Create(jit : TdwsJITx86_64; addrPtr : PPointer);
         function CompileCall(funcSym : TFuncSymbol; const args : TExprBaseListRec) : Boolean;
         function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
         function CompileInteger(expr : TTypedExpr) : Integer; override;
         procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
         function CompileBooleanValue(expr : TTypedExpr) : Integer; override;
   end;

   Tx86AbsIntFunc = class (TdwsJITter_x86)
      function CompileInteger(expr : TTypedExpr) : Integer; override;
   end;
   Tx86AbsFloatFunc = class (TdwsJITter_x86)
      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
   end;

   Tx86SqrtFunc = class (Tx86MagicFunc)
      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
   end;}
   Tx86SqrFloatFunc = class (Tx86SqrFloat)
      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
   end;
{
   Tx86MinMaxFloatFunc = class (Tx86MagicFunc)
      public
         OP : TxmmOp;
         constructor Create(jit : TdwsJITx86_64; op : TxmmOp);
         function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
   end;

   Tx86RoundFunc = class (Tx86MagicFunc)
      function CompileInteger(expr : TTypedExpr) : Integer; override;
      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
   end;
    }
   Tx86OddFunc = class (Tx86MagicBoolFunc)
      procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
      function CompileBooleanValue(expr : TTypedExpr) : Integer; override;
   end;

   Tx86Unsigned32Func = class (Tx86MagicFunc)
      function CompileInteger(expr : TTypedExpr) : Integer; override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$R-}

const
   cExecInstanceGPR = gprRDI;
   cVolatileXMM : TxmmRegisters = [ xmm0, xmm1, xmm2, xmm3, xmm4, xmm5 ];

function int64_div(a, b : Int64) : Int64;
begin
   Result:=a div b;
end;

function int64_mod(a, b : Int64) : Int64;
begin
   Result:=a mod b;
end;

function double_trunc(const v : Double) : Int64;
begin
   Result:=Trunc(v);
end;

function double_frac(const v : Double) : Double;
begin
   Result:=Frac(v);
end;

function double_exp(const v : Double) : Double;
begin
   Result:=Exp(v);
end;

function double_ln(const v : Double) : Double;
begin
   Result:=Ln(v);
end;

function double_log2(const v : Double) : Double;
begin
   Result:=Log2(v);
end;

function double_log10(const v : Double) : Double;
begin
   Result:=Log10(v);
end;

function double_power(const base, exponent: Double) : Double;
begin
   Result := Math.Power(base, exponent);
end;

function double_cos(const v : Double) : Double;
begin
   Result:=Cos(v);
end;

function double_sin(const v : Double) : Double;
begin
   Result:=Sin(v);
end;

function double_tan(const v : Double) : Double;
begin
   Result:=Tan(v);
end;

var
   vAddr_Exp : function (const v : Double) : Double = double_exp;
   vAddr_Ln : function (const v : Double) : Double = double_ln;
   vAddr_Log2 : function (const v : Double) : Double = double_log2;
   vAddr_Log10 : function (const v : Double) : Double = double_log10;
   vAddr_Power : function (const base, exponent: Double) : Double = double_power;
   vAddr_Trunc : function (const v : Double) : Int64 = double_trunc;
   vAddr_Frac : function (const v : Double) : Double = double_frac;
   vAddr_div : function (a, b : Int64) : Int64 = int64_div;
   vAddr_mod : function (a, b : Int64) : Int64 = int64_mod;
   vAddr_IsNaN : function (const v : Double) : Boolean = Math.IsNan;
   vAddr_IsInfinite : function (const v : Double) : Boolean = Math.IsInfinite;
   vAddr_IsFinite : function (const v : Double) : Boolean = dwsMathFunctions.IsFinite;
   vAddr_IsPrime : function (const n : Int64) : Boolean = dwsMathFunctions.IsPrime;
   vAddr_Cos : function (const v : Double) : Double = double_cos;
   vAddr_Sin : function (const v : Double) : Double = double_sin;
   vAddr_Tan : function (const v : Double) : Double = double_tan;

// ------------------
// ------------------ TdwsJITx86_64 ------------------
// ------------------

procedure TRegisterStatus.Flush;
begin
   Contains := nil;
   Lock := 0;
end;

function TRegisterStatus.ContainsDataSymbol : TDataSymbol;
begin
   if Contains is TDataSymbol then
      Result := TDataSymbol(Contains)
   else Result := nil;
end;

// ------------------
// ------------------ TdwsJITx86_64 ------------------
// ------------------

// Create
//
constructor TdwsJITx86_64.Create;
begin
   inherited;

   FAllocator:=TdwsJITAllocatorWin.Create;

   JITTedProgramExprClass:=TProgramExpr86;
   JITTedFloatExprClass:=TFloatExpr86;
   JITTedIntegerExprClass:=TIntegerExpr86;

   FInterpretedJITter:=Tx86InterpretedExpr.Create(Self);

   RegisterJITter(TConstFloatExpr,              Tx86ConstFloat.Create(Self));
   RegisterJITter(TConstIntExpr,                Tx86ConstInt.Create(Self));
   RegisterJITter(TConstBooleanExpr,            Tx86ConstBoolean.Create(Self));

   RegisterJITter(TFloatVarExpr,                Tx86FloatVar.Create(Self));
   RegisterJITter(TIntVarExpr,                  Tx86IntVar.Create(Self));
//   RegisterJITter(TBoolVarExpr,                 Tx86BoolVar.Create(Self));
//   RegisterJITter(TObjectVarExpr,               Tx86ObjectVar.Create(Self));
//   RegisterJITter(TSelfObjectVarExpr,           Tx86ObjectVar.Create(Self));
   RegisterJITter(TVarParentExpr,               FInterpretedJITter.IncRefCount);

   RegisterJITter(TFieldExpr,                   FInterpretedJITter.IncRefCount);
   RegisterJITter(TRecordExpr,                  FInterpretedJITter.IncRefCount);
//   RegisterJITter(TRecordVarExpr,               Tx86RecordVar.Create(Self));
   RegisterJITter(TFieldExpr,                   FInterpretedJITter.IncRefCount);
//   RegisterJITter(TFieldVarExpr,                Tx86FieldVar.Create(Self));

//   RegisterJITter(TVarParamExpr,                Tx86VarParam.Create(Self));
   RegisterJITter(TConstParamExpr,              FInterpretedJITter.IncRefCount);
   RegisterJITter(TLazyParamExpr,               FInterpretedJITter.IncRefCount);
   RegisterJITter(TVarParentExpr,               FInterpretedJITter.IncRefCount);
   RegisterJITter(TVarParamParentExpr,          FInterpretedJITter.IncRefCount);

   RegisterJITter(TStaticArrayExpr,             FInterpretedJITter.IncRefCount);//Tx86StaticArray.Create(Self));
//   RegisterJITter(TDynamicArrayExpr,            Tx86DynamicArray.Create(Self));
//   RegisterJITter(TDynamicArrayVarExpr,         Tx86DynamicArray.Create(Self));
//   RegisterJITter(TDynamicArraySetExpr,         Tx86DynamicArraySet.Create(Self));
//   RegisterJITter(TDynamicArraySetVarExpr,      Tx86DynamicArraySet.Create(Self));
   RegisterJITter(TDynamicArraySetDataExpr,     FInterpretedJITter.IncRefCount);

   RegisterJITter(TArrayLengthExpr,             FInterpretedJITter.IncRefCount);
   RegisterJITter(TArraySetLengthExpr,          FInterpretedJITter.IncRefCount);
   RegisterJITter(TArrayAddExpr,                FInterpretedJITter.IncRefCount);
   RegisterJITter(TArrayInsertExpr,             FInterpretedJITter.IncRefCount);
   RegisterJITter(TArrayIndexOfExpr,            FInterpretedJITter.IncRefCount);
   RegisterJITter(TDynamicArrayIndexOfExpr,     FInterpretedJITter.IncRefCount);
   RegisterJITter(TArrayRemoveExpr,             FInterpretedJITter.IncRefCount);
   RegisterJITter(TArrayDeleteExpr,             FInterpretedJITter.IncRefCount);
   RegisterJITter(TArrayPopExpr,                FInterpretedJITter.IncRefCount);
   RegisterJITter(TArraySwapExpr,               FInterpretedJITter.IncRefCount);
   RegisterJITter(TArraySortExpr,               FInterpretedJITter.IncRefCount);
   RegisterJITter(TArraySortNaturalStringExpr,  FInterpretedJITter.IncRefCount);
   RegisterJITter(TArraySortNaturalIntegerExpr, FInterpretedJITter.IncRefCount);
   RegisterJITter(TArraySortNaturalFloatExpr,   FInterpretedJITter.IncRefCount);
   RegisterJITter(TArrayReverseExpr,            FInterpretedJITter.IncRefCount);
   RegisterJITter(TArrayMapExpr,                FInterpretedJITter.IncRefCount);
   RegisterJITter(TArrayConcatExpr,             FInterpretedJITter.IncRefCount);

   RegisterJITter(TOpenArrayLengthExpr,         FInterpretedJITter.IncRefCount);

   RegisterJITter(TNullExpr,                    Tx86Null.Create(Self));

   RegisterJITter(TBlockExpr,                   Tx86BlockExprNoTable.Create(Self));
   RegisterJITter(TBlockExprNoTable,            Tx86BlockExprNoTable.Create(Self));
   RegisterJITter(TBlockExprNoTable2,           Tx86BlockExprNoTable.Create(Self));
   RegisterJITter(TBlockExprNoTable3,           Tx86BlockExprNoTable.Create(Self));
   RegisterJITter(TBlockExprNoTable4,           Tx86BlockExprNoTable.Create(Self));

   RegisterJITter(TAssignConstToIntegerVarExpr, Tx86AssignConstToIntegerVar.Create(Self));
   RegisterJITter(TAssignConstToFloatVarExpr,   Tx86AssignConstToFloatVar.Create(Self));
//   RegisterJITter(TAssignConstToBoolVarExpr,    Tx86AssignConstToBoolVar.Create(Self));
   RegisterJITter(TAssignConstToStringVarExpr,  FInterpretedJITter.IncRefCount);
   RegisterJITter(TAssignExpr,                  Tx86Assign.Create(Self));
//   RegisterJITter(TAssignDataExpr,              Tx86AssignData.Create(Self));
   RegisterJITter(TAssignClassOfExpr,           FInterpretedJITter.IncRefCount);
   RegisterJITter(TAssignFuncExpr,              FInterpretedJITter.IncRefCount);
   RegisterJITter(TAssignNilToVarExpr,          FInterpretedJITter.IncRefCount);
   RegisterJITter(TAssignNilClassToVarExpr,     FInterpretedJITter.IncRefCount);
   RegisterJITter(TAssignArrayConstantExpr,     FInterpretedJITter.IncRefCount);

   RegisterJITter(TAssignedInstanceExpr,        FInterpretedJITter.IncRefCount);
   RegisterJITter(TAssignedInterfaceExpr,       FInterpretedJITter.IncRefCount);
   RegisterJITter(TAssignedMetaClassExpr,       FInterpretedJITter.IncRefCount);
   RegisterJITter(TAssignedFuncPtrExpr,         FInterpretedJITter.IncRefCount);

//   RegisterJITter(TPlusAssignFloatExpr,         Tx86OpAssignFloat.Create(Self, xmm_addsd));
//   RegisterJITter(TMinusAssignFloatExpr,        Tx86OpAssignFloat.Create(Self, xmm_subsd));
//   RegisterJITter(TMultAssignFloatExpr,         Tx86OpAssignFloat.Create(Self, xmm_multsd));
//   RegisterJITter(TDivideAssignExpr,            Tx86OpAssignFloat.Create(Self, xmm_divsd));

   RegisterJITter(TStringLengthExpr,            FInterpretedJITter.IncRefCount);
   RegisterJITter(TAppendStringVarExpr,         FInterpretedJITter.IncRefCount);
   RegisterJITter(TAppendConstStringVarExpr,    FInterpretedJITter.IncRefCount);
   RegisterJITter(TVarStringArraySetExpr,       FInterpretedJITter.IncRefCount);
   RegisterJITter(TStringArrayOpExpr,           FInterpretedJITter.IncRefCount);

   RegisterJITter(TPlusAssignIntExpr,           FInterpretedJITter.IncRefCount);
   RegisterJITter(TMinusAssignIntExpr,          FInterpretedJITter.IncRefCount);
   RegisterJITter(TMultAssignIntExpr,           FInterpretedJITter.IncRefCount);

   RegisterJITter(TPlusAssignExpr,              FInterpretedJITter.IncRefCount);
   RegisterJITter(TMinusAssignExpr,             FInterpretedJITter.IncRefCount);
   RegisterJITter(TMultAssignExpr,              FInterpretedJITter.IncRefCount);

   RegisterJITter(TIfThenExpr,                  Tx86IfThen.Create(Self));
   RegisterJITter(TIfThenElseExpr,              Tx86IfThenElse.Create(Self));
   RegisterJITter(TCaseExpr,                    FInterpretedJITter.IncRefCount);
   RegisterJITter(TCaseStringExpr,              FInterpretedJITter.IncRefCount);
   RegisterJITter(TCaseIntegerExpr,             FInterpretedJITter.IncRefCount);

   RegisterJITter(TLoopExpr,                    Tx86Loop.Create(Self));
   RegisterJITter(TRepeatExpr,                  Tx86Repeat.Create(Self));
//   RegisterJITter(TWhileExpr,                   Tx86While.Create(Self));

   RegisterJITter(TForUpwardExpr,               Tx86ForUpward.Create(Self));
//   RegisterJITter(TForUpwardStepExpr,           Tx86ForUpward.Create(Self));
   RegisterJITter(TForDownwardExpr,             FInterpretedJITter.IncRefCount);
   RegisterJITter(TForDownwardStepExpr,         FInterpretedJITter.IncRefCount);
   RegisterJITter(TForCharCodeInStrExpr,        FInterpretedJITter.IncRefCount);
   RegisterJITter(TForCharInStrExpr,            FInterpretedJITter.IncRefCount);

//   RegisterJITter(TContinueExpr,                Tx86Continue.Create(Self));
//   RegisterJITter(TBreakExpr,                   Tx86Break.Create(Self));
//   RegisterJITter(TExitExpr,                    Tx86Exit.Create(Self));
//   RegisterJITter(TExitValueExpr,               Tx86ExitValue.Create(Self));

   RegisterJITter(TRaiseExpr,                   FInterpretedJITter.IncRefCount);
   RegisterJITter(TExceptExpr,                  FInterpretedJITter.IncRefCount);
   RegisterJITter(TFinallyExpr,                 FInterpretedJITter.IncRefCount);

   RegisterJITter(TAddFloatExpr,                Tx86FloatBinOp.Create(Self, xmm_addsd));
   RegisterJITter(TSubFloatExpr,                Tx86FloatBinOp.Create(Self, xmm_subsd));
   RegisterJITter(TMultFloatExpr,               Tx86FloatBinOp.Create(Self, xmm_multsd));
   RegisterJITter(TSqrFloatExpr,                Tx86SqrFloat.Create(Self));
   RegisterJITter(TDivideExpr,                  Tx86FloatBinOp.Create(Self, xmm_divsd));
//   RegisterJITter(TModFloatExpr,                Tx86ModFloat.Create(Self));
   RegisterJITter(TNegFloatExpr,                Tx86NegFloat.Create(Self));

   RegisterJITter(TNegIntExpr,                  Tx86NegInt.Create(Self));
   RegisterJITter(TNotIntExpr,                  Tx86NotInt.Create(Self));
   RegisterJITter(TAddIntExpr,                  Tx86IntegerBinOpExpr.Create(Self, gpOp_add));
   RegisterJITter(TSubIntExpr,                  Tx86IntegerBinOpExpr.Create(Self, gpOp_sub, False));
   RegisterJITter(TMultIntExpr,                 Tx86MultInt.Create(Self));
   RegisterJITter(TSqrIntExpr,                  FInterpretedJITter.IncRefCount);
//   RegisterJITter(TDivExpr,                     Tx86DivInt.Create(Self));
//   RegisterJITter(TDivConstExpr,                Tx86DivInt.Create(Self));
//   RegisterJITter(TModExpr,                     Tx86ModInt.Create(Self));
//   RegisterJITter(TModConstExpr,                Tx86ModInt.Create(Self));
//   RegisterJITter(TMultIntPow2Expr,             Tx86MultIntPow2.Create(Self));
   RegisterJITter(TIntAndExpr,                  Tx86IntegerBinOpExpr.Create(Self, gpOp_and));
   RegisterJITter(TIntXorExpr,                  Tx86IntegerBinOpExpr.Create(Self, gpOp_xor));
   RegisterJITter(TIntOrExpr,                   Tx86IntegerBinOpExpr.Create(Self, gpOp_or));

   RegisterJITter(TShrExpr,                     Tx86Shr.Create(Self));
   RegisterJITter(TShlExpr,                     Tx86Shl.Create(Self));

   RegisterJITter(TInOpExpr,                    FInterpretedJITter.IncRefCount);
   RegisterJITter(TStringInOpExpr,              FInterpretedJITter.IncRefCount);
   RegisterJITter(TStringInOpStaticSetExpr,     FInterpretedJITter.IncRefCount);
   RegisterJITter(TIntegerInOpExpr,             FInterpretedJITter.IncRefCount);
   RegisterJITter(TBitwiseInOpExpr,             FInterpretedJITter.IncRefCount);

   RegisterJITter(TIncIntVarExpr,               Tx86IncIntVar.Create(Self));
//   RegisterJITter(TDecIntVarExpr,               Tx86DecIntVar.Create(Self));
   RegisterJITter(TIncIntVarWithConstExpr,      Tx86IncIntVar.Create(Self));
//
//   RegisterJITter(TIncVarFuncExpr,              Tx86IncVarFunc.Create(Self));
//   RegisterJITter(TDecVarFuncExpr,              Tx86DecVarFunc.Create(Self));

   RegisterJITter(TRelEqualIntExpr,             Tx86RelEqualInt.Create(Self));
   RegisterJITter(TRelNotEqualIntExpr,          Tx86RelNotEqualInt.Create(Self));
//   RegisterJITter(TRelGreaterIntExpr,           Tx86RelOpInt.Create(Self, flagsG, flagsL, flagsNBE));
//   RegisterJITter(TRelGreaterEqualIntExpr,      Tx86RelOpInt.Create(Self, flagsG, flagsL, flagsNB));
//   RegisterJITter(TRelLessIntExpr,              Tx86RelOpInt.Create(Self, flagsL, flagsG, flagsB));
//   RegisterJITter(TRelLessEqualIntExpr,         Tx86RelOpInt.Create(Self, flagsL, flagsG, flagsBE));

   RegisterJITter(TRelIntIsZeroExpr,            Tx86RelIntIsZero.Create(Self));
   RegisterJITter(TRelIntIsNotZeroExpr,         Tx86RelIntIsNotZero.Create(Self));

   RegisterJITter(TRelEqualFloatExpr,           Tx86RelOpFloat.Create(Self, flagsE));
   RegisterJITter(TRelNotEqualFloatExpr,        Tx86RelOpFloat.Create(Self, flagsNE));
   RegisterJITter(TRelGreaterFloatExpr,         Tx86RelOpFloat.Create(Self, flagsNBE));
   RegisterJITter(TRelGreaterEqualFloatExpr,    Tx86RelOpFloat.Create(Self, flagsNB));
   RegisterJITter(TRelLessFloatExpr,            Tx86RelOpFloat.Create(Self, flagsB));
   RegisterJITter(TRelLessEqualFloatExpr,       Tx86RelOpFloat.Create(Self, flagsBE));

   RegisterJITter(TRelEqualStringExpr,          FInterpretedJITter.IncRefCount);
   RegisterJITter(TRelNotEqualStringExpr,       FInterpretedJITter.IncRefCount);
   RegisterJITter(TRelGreaterStringExpr,        FInterpretedJITter.IncRefCount);
   RegisterJITter(TRelGreaterEqualStringExpr,   FInterpretedJITter.IncRefCount);
   RegisterJITter(TRelLessStringExpr,           FInterpretedJITter.IncRefCount);
   RegisterJITter(TRelLessEqualStringExpr,      FInterpretedJITter.IncRefCount);

   RegisterJITter(TRelEqualBoolExpr,            FInterpretedJITter.IncRefCount);
   RegisterJITter(TRelNotEqualBoolExpr,         FInterpretedJITter.IncRefCount);

   RegisterJITter(TRelEqualMetaExpr,            Tx86RelEqualInt.Create(Self));
//   RegisterJITter(TRelNotEqualMetaExpr,         Tx86RelNotEqualInt.Create(Self));

   RegisterJITter(TRelVarEqualNilExpr,          FInterpretedJITter.IncRefCount);
   RegisterJITter(TRelVarNotEqualNilExpr,       FInterpretedJITter.IncRefCount);

   RegisterJITter(TIsOpExpr,                    FInterpretedJITter.IncRefCount);
   RegisterJITter(TImplementsIntfOpExpr,        FInterpretedJITter.IncRefCount);
   RegisterJITter(TObjCmpEqualExpr,             FInterpretedJITter.IncRefCount);
   RegisterJITter(TObjCmpNotEqualExpr,          FInterpretedJITter.IncRefCount);
   RegisterJITter(TIntfCmpExpr,                 FInterpretedJITter.IncRefCount);
//
//   RegisterJITter(TNotBoolExpr,                 Tx86NotExpr.Create(Self));
   RegisterJITter(TBoolOrExpr,                  Tx86BoolOrExpr.Create(Self));
//   RegisterJITter(TBoolAndExpr,                 Tx86BoolAndExpr.Create(Self));
   RegisterJITter(TBoolXorExpr,                 FInterpretedJITter.IncRefCount);
   RegisterJITter(TBoolImpliesExpr,             FInterpretedJITter.IncRefCount);

   RegisterJITter(TCoalesceExpr,                FInterpretedJITter.IncRefCount);
   RegisterJITter(TCoalesceStrExpr,             FInterpretedJITter.IncRefCount);
   RegisterJITter(TCoalesceClassExpr,           FInterpretedJITter.IncRefCount);
   RegisterJITter(TCoalesceDynArrayExpr,        FInterpretedJITter.IncRefCount);

//   RegisterJITter(TSetOfInExpr,                 Tx86SetOfInExpr.Create(Self));
//   RegisterJITter(TSetOfSmallInExpr,            Tx86SetOfInExpr.Create(Self));
//   RegisterJITter(TSetOfIncludeExpr,            Tx86SetOfInclude.Create(Self));
//   RegisterJITter(TSetOfExcludeExpr,            Tx86SetOfExclude.Create(Self));

   RegisterJITter(TOrdExpr,                     FInterpretedJITter.IncRefCount);
//   RegisterJITter(TOrdBoolExpr,                 Tx86OrdBool.Create(Self));
//   RegisterJITter(TOrdIntExpr,                  Tx86OrdInt.Create(Self));
   RegisterJITter(TOrdStrExpr,                  FInterpretedJITter.IncRefCount);

   RegisterJITter(TSwapExpr,                    FInterpretedJITter.IncRefCount);
   RegisterJITter(TAssertExpr,                  FInterpretedJITter.IncRefCount);

   RegisterJITter(TDeclaredExpr,                FInterpretedJITter.IncRefCount);
   RegisterJITter(TDefinedExpr,                 FInterpretedJITter.IncRefCount);
   RegisterJITter(TConditionalDefinedExpr,      FInterpretedJITter.IncRefCount);

   RegisterJITter(TConvIntToFloatExpr,          Tx86ConvIntToFloat.Create(Self));
   RegisterJITter(TConvVarToFloatExpr,          FInterpretedJITter.IncRefCount);
   RegisterJITter(TConvVarToIntegerExpr,        FInterpretedJITter.IncRefCount);
   RegisterJITter(TConvOrdToIntegerExpr,        FInterpretedJITter.IncRefCount);

   RegisterJITter(TConstructorStaticExpr,       FInterpretedJITter.IncRefCount);
   RegisterJITter(TConstructorStaticDefaultExpr,FInterpretedJITter.IncRefCount);
   RegisterJITter(TConstructorStaticObjExpr,    FInterpretedJITter.IncRefCount);
   RegisterJITter(TConstructorVirtualExpr,      FInterpretedJITter.IncRefCount);
   RegisterJITter(TConstructorVirtualObjExpr,   FInterpretedJITter.IncRefCount);
   RegisterJITter(TMethodStaticExpr,            FInterpretedJITter.IncRefCount);
   RegisterJITter(TMethodVirtualExpr,           FInterpretedJITter.IncRefCount);
   RegisterJITter(TMethodInterfaceExpr,         FInterpretedJITter.IncRefCount);
   RegisterJITter(TMethodInterfaceAnonymousExpr, FInterpretedJITter.IncRefCount);
   RegisterJITter(TClassMethodStaticExpr,       FInterpretedJITter.IncRefCount);
   RegisterJITter(TClassMethodVirtualExpr,      FInterpretedJITter.IncRefCount);
   RegisterJITter(TRecordMethodExpr,            FInterpretedJITter.IncRefCount);
   RegisterJITter(THelperMethodExpr,            FInterpretedJITter.IncRefCount);
   RegisterJITter(TFuncPtrExpr,                 FInterpretedJITter.IncRefCount);

   RegisterJITter(TFuncExpr,                    FInterpretedJITter.IncRefCount);
   RegisterJITter(TFuncSimpleExpr,              FInterpretedJITter.IncRefCount);
   RegisterJITter(TMagicProcedureExpr,          FInterpretedJITter.IncRefCount);
   RegisterJITter(TMagicFloatFuncExpr,          Tx86MagicFunc.Create(Self));
   RegisterJITter(TMagicIntFuncExpr,            Tx86MagicFunc.Create(Self));
   RegisterJITter(TMagicBoolFuncExpr,           Tx86MagicFunc.Create(Self));

//   RegisterJITter(TAbsIntFunc,                  Tx86AbsIntFunc.Create(Self));
//   RegisterJITter(TAbsFloatFunc,                Tx86AbsFloatFunc.Create(Self));
//
//   RegisterJITter(TSqrtFunc,                    Tx86SqrtFunc.Create(Self));
   RegisterJITter(TSqrFloatFunc,                Tx86SqrFloatFunc.Create(Self));
//   RegisterJITter(TMaxFunc,                     Tx86MinMaxFloatFunc.Create(Self, xmm_maxsd));
//   RegisterJITter(TMinFunc,                     Tx86MinMaxFloatFunc.Create(Self, xmm_minsd));

//   RegisterJITter(TExpFunc,                     Tx86DirectCallFunc.Create(Self, @@vAddr_Exp));
//   RegisterJITter(TLnFunc,                      Tx86DirectCallFunc.Create(Self, @@vAddr_Ln));
//   RegisterJITter(TLog2Func,                    Tx86DirectCallFunc.Create(Self, @@vAddr_Log2));
//   RegisterJITter(TLog10Func,                   Tx86DirectCallFunc.Create(Self, @@vAddr_Log10));
//
//   RegisterJITter(TPowerFunc,                   Tx86DirectCallFunc.Create(Self, @@vAddr_Power));
//
//   RegisterJITter(TRoundFunc,                   Tx86RoundFunc.Create(Self));
//   RegisterJITter(TTruncFunc,                   Tx86DirectCallFunc.Create(Self, @@vAddr_Trunc));
//   RegisterJITter(TFracFunc,                    Tx86DirectCallFunc.Create(Self, @@vAddr_Frac));
//   RegisterJITter(TIsNaNFunc,                   Tx86DirectCallFunc.Create(Self, @@vAddr_IsNaN));
//   RegisterJITter(TIsInfiniteFunc,              Tx86DirectCallFunc.Create(Self, @@vAddr_IsInfinite));
//   RegisterJITter(TIsFiniteFunc,                Tx86DirectCallFunc.Create(Self, @@vAddr_IsFinite));
//   RegisterJITter(TIsPrimeFunc,                 Tx86DirectCallFunc.Create(Self, @@vAddr_IsPrime));
//
   RegisterJITter(TOddFunc,                     Tx86OddFunc.Create(Self));

   RegisterJITter(TUnsigned32Func,              Tx86Unsigned32Func.Create(Self));

//   RegisterJITter(TCosFunc,                     Tx86DirectCallFunc.Create(Self, @@vAddr_Cos));
//   RegisterJITter(TSinFunc,                     Tx86DirectCallFunc.Create(Self, @@vAddr_Sin));
//   RegisterJITter(TTanFunc,                     Tx86DirectCallFunc.Create(Self, @@vAddr_Tan));
end;

// Destroy
//
destructor TdwsJITx86_64.Destroy;
begin
   inherited;
   FAllocator.Free;
   FInterpretedJITter.Free;
end;

// CreateOutput
//
function TdwsJITx86_64.CreateOutput : Tx86BaseWriteOnlyStream;
begin
   x86:=Tx86_64_WriteOnlyStream.Create;
   Result:=x86;
end;

// CreateFixupLogic
//
function TdwsJITx86_64.CreateFixupLogic : TFixupLogic;
begin
   Result:=Tx86_64FixupLogic.Create;
end;

// AllocXMMReg
//
function TdwsJITx86_64.AllocXMMReg(expr : TExprBase) : TxmmRegister;
var
   i : TxmmRegister;
   contains : TObject;
begin
   if expr is TVarExpr then
      contains := TVarExpr(expr).DataSymbol
   else contains := expr;

   // find unused register
   for i := xmm1 to High(TxmmRegister) do begin
      if FRegs[i].Contains = nil then begin
         FRegs[i].Contains := contains;
         FRegs[i].Lock := 1;
         FPreamble.NotifyAlteration(i);
         Exit(i);
      end;
   end;

   // repurpose register if it's not locked
   for i := xmm1 to High(TxmmRegister) do begin
      if FRegs[i].Lock = 0 then begin
         FRegs[i].Contains := contains;
         FRegs[i].Lock := 1;
         FPreamble.NotifyAlteration(i);
         Exit(i);
      end;
   end;

   OutputFailedOn := expr;
   Result := xmm0;
end;

// ReleaseXMMReg
//
procedure TdwsJITx86_64.ReleaseXMMReg(reg : TxmmRegister);
begin
   if FRegs[reg].Lock>0 then
      Dec(FRegs[reg].Lock);
end;

// FlushXMMRegs
//
procedure TdwsJITx86_64.FlushXMMRegs(option : TxmmFlushOption);
var
   reg : TxmmRegister;
begin
   case option of
      xmmFlushAll :
         for reg := xmm0 to High(TxmmRegister) do
            FRegs[reg].Flush;
      xmmFlushVolatile :
         for reg := xmm0 to High(TxmmRegister) do
            if reg in cVolatileXMM then
               FRegs[reg].Flush;
      xmmFlushIntermediateExprs :
         for reg := xmm0 to High(TxmmRegister) do
            if FRegs[reg].ContainsDataSymbol = nil then
               FRegs[reg].Flush;
   else
      Assert(False);
   end;
end;

// CurrentXMMReg
//
function TdwsJITx86_64.CurrentXMMReg(expr : TExprBase) : TxmmRegister;
var
   i : TxmmRegister;
   contains : TObject;
begin
   if expr is TVarExpr then
      contains := TVarExpr(expr).DataSymbol
   else contains := expr;

   for i := xmm0 to High(TxmmRegister) do begin
      if FRegs[i].Contains=contains then begin
         Inc(FRegs[i].Lock);
         Exit(i);
      end;
   end;
   Result:=xmmNone;
end;

// SetContainsXMMReg
//
procedure TdwsJITx86_64.SetContainsXMMReg(reg : TxmmRegister; expr : TExprBase);
var
   i : TxmmRegister;
   contains : TObject;
begin
   if expr is TVarExpr then
      contains := TVarExpr(expr).DataSymbol
   else contains := expr;

   for i:=xmm0 to High(FRegs) do begin
      if FRegs[i].Contains=contains then begin
         if i<>reg then
            FRegs[i].Flush;
      end;
   end;
   FRegs[reg].Contains:=contains;
end;

// ResetXMMReg
//
procedure TdwsJITx86_64.ResetXMMReg;
begin
   for var i := xmm0 to High(FRegs) do begin
      FRegs[i].Contains := nil;
      FRegs[i].Lock := 0;
   end;
end;

// SaveXMMRegs
//
procedure TdwsJITx86_64.SaveXMMRegs(firstReg : TxmmRegister = xmm0);
var
   i : TxmmRegister;
begin
   Assert(FSavedXMM=[]);

   for i:=firstReg to High(FRegs) do begin
      if FRegs[i].Lock > 0 then begin
         Include(FSavedXMM, i);
         FRegsStackAddr[i] := FPreamble.AllocateStackSpace(SizeOf(Double));
         x86._movsd_qword_ptr_reg_reg(gprRBP, FRegsStackAddr[i], i);
      end;
   end;
end;

// RestoreXMMRegs
//
procedure TdwsJITx86_64.RestoreXMMRegs(firstReg : TxmmRegister = xmm0);
var
   i : TxmmRegister;
begin
   if FSavedXMM=[] then Exit;

   for i:=High(FRegs) downto firstReg do begin
      if i in FSavedXMM then begin
         x86._movsd_reg_qword_ptr_reg(i, gprRBP, FRegsStackAddr[i]);
      end else FRegs[i].Contains:=nil;
   end;

   FSavedXMM:=[];
end;

// StackAddrOfFloat
//
function TdwsJITx86_64.StackAddrOfFloat(expr : TTypedExpr) : Integer;
begin
   if expr.ClassType = TFloatVarExpr then
      Result:=TFloatVarExpr(expr).StackAddr
   else Result:=-1;
end;

// CompileFloat
//
function TdwsJITx86_64.CompileFloat(expr : TTypedExpr) : TxmmRegister;
begin
   Result:=TxmmRegister(inherited CompileFloat(expr));
end;

// CompileAssignFloat
//
procedure TdwsJITx86_64.CompileAssignFloat(expr : TTypedExpr; source : TxmmRegister);
begin
   inherited CompileAssignFloat(expr, Ord(source));
end;

// CompileBoolean
//
procedure TdwsJITx86_64.CompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
begin
   inherited CompileBoolean(expr, targetTrue, targetFalse);
end;

// CompiledOutput
//
function TdwsJITx86_64.CompiledOutput : TdwsJITCodeBlock;
var
   compiledBytes : TBytes;
   unwindBytes : TBytes;
   staticDataBytes : TBytes;
   outputBytes : TBytes;
   ptr : Pointer;
   sub : IdwsJITCodeSubAllocator;
   block64 : TdwsJITCodeBlock86_64;
   pUnwindInfo : PUNWIND_INFO;
   pRuntimeFunction : PRUNTIME_FUNCTION;
   unwindSize, i : Integer;
begin
   FPreamble.HasCalls := x86.FlagCalls;

   Fixups.FlushFixups(Output.ToBytes, Output);

   compiledBytes := Output.ToBytes;

   unwindSize := SizeOf(RUNTIME_FUNCTION) + SizeOf(UNWIND_INFO)
               + FPreamble.Unwind.SizeInBytes;
   // round up to next multiple of 16 with at least 8 bytes of nop padding
   i := unwindSize;
   unwindSize := ((unwindSize + 8 + 15) div 16) * 16;
   SetLength(unwindBytes, unwindSize);
   while i < unwindSize do begin
      unwindBytes[i] := $90; // NOP
      Inc(i);
   end;

   pRuntimeFunction := @unwindBytes[0];
   pUnwindInfo := @unwindBytes[SizeOf(RUNTIME_FUNCTION)];

   pUnwindInfo.Version := 1;
   pUnwindInfo.Flags := 0;

   pUnwindInfo.SizeOfProlog := FPreamble.Unwind.PrologSize;
   if x86.FrameRegisterOffset > 0 then
      pUnwindInfo.FrameRegister := UNWIND_RBP
   else pUnwindInfo.FrameRegister := UNWIND_RAX;
   pUnwindInfo.FrameOffset := x86.FrameRegisterOffset div 16;

   pUnwindInfo.CountOfCodes := FPreamble.Unwind.SizeInOps;
   FPreamble.Unwind.CopyUnwindCodesTo(@pUnwindInfo.UnwindCode[0]);

   pRuntimeFunction.BeginAddress := unwindSize;
   pRuntimeFunction.EndAddress := unwindSize + Length(compiledBytes);
   pRuntimeFunction.UnwindInfoAddress := SizeOf(RUNTIME_FUNCTION);

   // complete output is unwind data + compiled bytes + nop padding [ + static data ]
   Assert(Length(compiledBytes) < Fixups.StaticDataBase);
   outputBytes := unwindBytes + compiledBytes;
   i := Length(outputBytes);
   SetLength(outputBytes, i + Fixups.StaticDataBase - Length(compiledBytes));
   Assert((Length(outputBytes) and $F) = 0); // 16 bytes alignment
   while i < Length(outputBytes) do begin
      outputBytes[i] := $90; // NOP
      Inc(i);
   end;
   staticDataBytes := Fixups.StaticData;
   if Length(staticDataBytes) > 0 then
      outputBytes := outputBytes + staticDataBytes;

   ptr := Allocator.Allocate(outputBytes, sub);
   block64 := TdwsJITCodeBlock86_64.Create(Pointer(IntPtr(ptr) + unwindSize), sub);
   block64.Steppable := (jitoDoStep in Options);

   block64.RegisterTable(ptr);

//   var ib : Pointer;
//   OutputDebugString(RtlLookupFunctionEntry(block64.CodePtr, ib, nil).ToString(ib));

   Fixups.ClearFixups;

   Result := block64;
end;

// StartJIT
//
procedure TdwsJITx86_64.StartJIT(expr : TExprBase; exitable : Boolean);
begin
   inherited;
   ResetXMMReg;
   x86.ClearFlags;
   Fixups.ClearFixups;
   (Fixups as Tx86_64FixupLogic).Options:=Options;
   FPreamble:=TFixupPreamble.Create;
   Fixups.AddFixup(FPreamble);
   FPostamble:=TFixupPostamble.Create(FPreamble);
   FPostamble.Logic:=Fixups;
end;

// EndJIT
//
procedure TdwsJITx86_64.EndJIT;
begin
   inherited;
   Fixups.AddFixup(FPostamble);
end;

// EndFloatJIT
//
procedure TdwsJITx86_64.EndFloatJIT(resultHandle : Integer);
begin
   x86._movsd_reg_reg(xmm0, TxmmRegister(resultHandle));
   inherited;
end;

// EndIntegerJIT
//
procedure TdwsJITx86_64.EndIntegerJIT(resultHandle : Integer);
begin
   inherited;
end;

// _xmm_reg_expr
//
procedure TdwsJITx86_64._xmm_reg_expr(op : TxmmOp; dest : TxmmRegister; expr : TTypedExpr);
var
   addrRight : Integer;
   regRight : TxmmRegister;
   c : TConstFloatExpr;
begin
   if expr.ClassType=TConstFloatExpr then begin

      c := TConstFloatExpr(expr);
      if (op = xmm_multsd) and (c.Value = 2) then
         x86._xmm_reg_reg(xmm_addsd, dest, dest)
      else Fixups.NewOpRegImm(OP, dest, c.Value);

   end else begin

      regRight := CurrentXMMReg(expr);
      if regRight <> xmmNone then begin

         x86._xmm_reg_reg(OP, dest, regRight);
         ReleaseXMMReg(regRight);

      end else begin

         addrRight:=StackAddrOfFloat(expr);
         if addrRight>=0 then begin

            x86._xmm_reg_execmem(OP, dest, addrRight);

         end else begin

            regRight:=CompileFloat(expr);
            x86._xmm_reg_reg(OP, dest, regRight);
            ReleaseXMMReg(regRight);

         end;

      end;

   end;
end;

// _comisd_reg_expr
//
procedure TdwsJITx86_64._comisd_reg_expr(dest : TxmmRegister; expr : TTypedExpr);
var
   addrRight : Integer;
   regRight : TxmmRegister;
begin
   if expr.ClassType=TConstFloatExpr then begin

      Fixups.NewComisdRegImm(dest, TConstFloatExpr(expr).Value);

   end else begin

      regRight := CurrentXMMReg(expr);
      if regRight <> xmmNone then begin

         x86._comisd_reg_reg(dest, regRight);
         ReleaseXMMReg(regRight);

      end else begin

         addrRight:=StackAddrOfFloat(expr);
         if addrRight>=0 then begin

            x86._comisd_reg_execmem(dest, addrRight);

         end else begin

            regRight:=CompileFloat(expr);
            x86._comisd_reg_reg(dest, regRight);
            ReleaseXMMReg(regRight);

         end;

      end;

   end;
end;

// _movsd_reg_imm
//
procedure TdwsJITx86_64._movsd_reg_imm(dest : TxmmRegister; const value : Double);
begin
   Fixups.NewMovsdRegImm(dest, value);
end;

// _store_rax
//
function TdwsJITx86_64._store_rax : Integer;
begin
   Result:=FPreamble.AllocateStackSpace(SizeOf(Int64));
   x86._mov_qword_ptr_reg_reg(gprRBP, Result, gprRAX);
end;

// _restore_rax
//
procedure TdwsJITx86_64._restore_rax(addr : Integer);
begin
   x86._mov_reg_qword_ptr_reg(gprRAX, gprRBP, addr);
end;

// _mov_reg_execInstance
//
procedure TdwsJITx86_64._mov_reg_execInstance(reg : TgpRegister64);
begin
   FPreamble.PreserveExec := True;

   x86._mov_reg_reg(reg, cExecInstanceGPR);
end;

// _mov_reg_execStatus
//
procedure TdwsJITx86_64._mov_reg_execStatus(reg : TgpRegister64);
begin
   FPreamble.PreserveExec := True;

   x86._mov_reg_byte_ptr_reg(reg, cExecInstanceGPR, TdwsExecution.Status_Offset);
end;

// _mov_execStatus_imm
//
procedure TdwsJITx86_64._mov_execStatus_imm(value : Int32);
begin
   FPreamble.PreserveExec := True;

   x86._mov_byte_ptr_reg_imm(cExecInstanceGPR, TdwsExecution.Status_Offset, 0);
end;

// _DoStep
//
var
   cPtr_TdwsExecution_DoStep : Pointer = @TdwsExecution.DoStep;
procedure TdwsJITx86_64._DoStep(expr : TExprBase);
begin
   if not (jitoDoStep in Options) then Exit;

   _mov_reg_execInstance(gprRDX);

   x86._call_absmem(cPtr_TdwsExecution_DoStep);

   FlushXMMRegs(xmmFlushVolatile);
end;

// _RangeCheck
//
{var
   cPtr_TProgramExpr_RaiseUpperExceeded : Pointer = @TProgramExpr.RaiseUpperExceeded;
   cPtr_TProgramExpr_RaiseLowerExceeded : Pointer = @TProgramExpr.RaiseLowerExceeded;
procedure TdwsJITx86_64._RangeCheck(expr : TExprBase; reg : TgpRegister64; delta, miniInclusive, maxiExclusive : Integer);
var
   passed, passedMini : TFixupTarget;
begin
   delta:=delta-miniInclusive;
   maxiExclusive:=maxiExclusive-miniInclusive;
   if delta<>0 then
      x86._add_reg_int32(reg, delta);

   if not (jitoRangeCheck in Options) then Exit;

   passed:=Fixups.NewHangingTarget(True);

   x86._cmp_reg_int32(reg, maxiExclusive);

   Fixups.NewJump(flagsB, passed);

   if delta<>0 then
      x86._add_reg_int32(reg, -delta);
   x86._cmp_reg_int32(reg, miniInclusive);

   passedMini:=Fixups.NewHangingTarget(False);

   Fixups.NewJump(flagsGE, passedMini);

   x86._mov_reg_reg(gprECX, reg);
   _mov_reg_execInstance(gprEDX);
   x86._mov_reg_dword(gprEAX, DWORD(expr));
   x86._call_absmem(@cPtr_TProgramExpr_RaiseLowerExceeded);

   Fixups.AddFixup(passedMini);

   x86._mov_reg_reg(gprECX, reg);
   _mov_reg_execInstance(gprEDX);
   x86._mov_reg_dword(gprEAX, DWORD(expr));
   x86._call_absmem(@cPtr_TProgramExpr_RaiseUpperExceeded);

   Fixups.AddFixup(passed);
end;        }

// ------------------
// ------------------ TdwsJITCodeBlock86_64 ------------------
// ------------------

// Create
//
constructor TdwsJITCodeBlock86_64.Create(const aCodePtr : Pointer; const aSubAllocator : IdwsJITCodeSubAllocator);
begin
   inherited;
end;

// Destroy
//
destructor TdwsJITCodeBlock86_64.Destroy;
begin
   inherited;
   if FRegistered then
      RtlDeleteFunctionTable(FRuntimeFunction);
end;

// RegisterTable
//
procedure TdwsJITCodeBlock86_64.RegisterTable(rtFn : PRUNTIME_FUNCTION);
begin
   FRuntimeFunction := rtFn;
   if not RtlAddFunctionTable(rtFn, 1, rtFn) then
      raise Exception.Create('Failed RtlAddFunctionTable');
   FRegistered := True;
end;

// ------------------
// ------------------ TFixupAlignedTarget ------------------
// ------------------

// GetSize
//
function TFixupAlignedTarget.GetSize : Integer;
begin
   {$ifdef ALIGN_JUMP_TARGETS }
   if TargetCount=0 then begin
      Result:=0;
   end else begin
      Result:=(16-(FixedLocation and $F)) and $F;
      if Result>7 then
         Result:=0;
   end;
   {$else}
   Result := 0;
   {$endif}
end;

// Write
//
procedure TFixupAlignedTarget.Write(output : TWriteOnlyBlockStream);
begin
   (output as Tx86_64_WriteOnlyStream)._nop(GetSize);
end;

// JumpLocation
//
function TFixupAlignedTarget.JumpLocation : Integer;
begin
   Result:=FixedLocation+GetSize;
end;

// ------------------
// ------------------ TFixupJump ------------------
// ------------------

// Create
//
constructor TFixupJump.Create(flags : TboolFlags);
begin
   inherited Create;
   FFlags:=flags;
end;

// Optimize
//
function TFixupJump.Optimize : TFixupOptimizeAction;
begin
   if (Next=Target) and (JumpLocation=Next.Location) then begin
      Target:=nil;
      Result:=foaRemove;
   end else Result:=inherited;
end;

// GetSize
//
function TFixupJump.GetSize : Integer;
begin
   if (Next=Target) and (Location=Next.Location) then
      Result := 0
   else Result := Tx86_Platform_WriteOnlyStream.SizeOf_jump(FFlags, Target.JumpLocation-FixedLocation);
end;

// NearJump
//
function TFixupJump.NearJump : Boolean;
var
   delta : Integer;
begin
   if FLongJump then
      Result:=False
   else begin
      delta := FixedLocation-Target.JumpLocation;
      Result := (Int8(delta) = delta);
      if (FixedLocation<>0) and (Target.FixedLocation<>0) then
         FLongJump:=not Result;
   end;
end;

// Write
//
procedure TFixupJump.Write(output : TWriteOnlyBlockStream);
var
   offset : Integer;
begin
   if (Next=Target) and (Location=Next.Location) then
      Exit;

   offset:=Target.JumpLocation-FixedLocation;
   (output as Tx86_Platform_WriteOnlyStream)._jump(FFlags, offset);
end;

// ------------------
// ------------------ TFixupPreamble ------------------
// ------------------

// Create
//
constructor TFixupPreamble.Create;
begin
   inherited;
   FUnwind := TUnwindCodeBuilder.Create;
end;

// Destroy
//
destructor TFixupPreamble.Destroy;
begin
   inherited;
   FUnwind.Free;
end;

// StackTotalSpace
//
function TFixupPreamble.StackTotalSpace : Integer;
begin
   Result := AllocatedStackSpace;
   if HasCalls then
      Inc(Result, $20);
end;

// NeedRBP
//
function TFixupPreamble.NeedRBP : Boolean;
begin
   Result := AllocatedStackSpace > 0;
end;

// Prepare
//
procedure TFixupPreamble.Prepare;
begin
   FUnwind.PushNonVolatile(UNWIND_REG(cExecMemGPR));

   if FPreserveExec then begin
      FUnwind.PushNonVolatile(UNWIND_REG(cExecInstanceGPR));
   end;

   if NeedRBP then begin
      FUnwind.PushNonVolatile(UNWIND_RBP);
      FUnwind.AddCustomPrologOp([ $48, $89, $E5 ]); // mov rbp, rsp
   end;

   FUnwind.PushXMM128s(FAlteredXMM and not($3F));

   FUnwind.Done(StackTotalSpace);
end;

// Optimize
//
function TFixupPreamble.Optimize : TFixupOptimizeAction;
begin
   if FUnwind.PrologSize = 0 then
      Prepare;
   Result := foaNone;
end;

// GetSize
//
function TFixupPreamble.GetSize : Integer;
begin
   Result := FUnwind.PrologSize
           + 3*Ord(FPreserveExec) // mov execInstance, rdx
           + 4; // mov execmem reg, [rdx + stack mixin]
end;

// Write
//
procedure TFixupPreamble.Write(output : TWriteOnlyBlockStream);
var
   x86 : Tx86_64_WriteOnlyStream;
begin
   x86 := (output as Tx86_64_WriteOnlyStream);

   x86.WriteBytes(FUnwind.Prolog);

   if FPreserveExec then
      x86._mov_reg_reg(cExecInstanceGPR, gprRDX);

   x86._mov_reg_qword_ptr_reg(cExecMemGPR, gprRDX, TdwsExecution.StackMixin_Offset);
end;

// AllocateStackSpace
//
function TFixupPreamble.AllocateStackSpace(bytes : Integer) : Integer;
begin
   Inc(FAllocatedStackSpace, bytes);
   Result:=-AllocatedStackSpace;
end;

// NotifyAlteration
//
procedure TFixupPreamble.NotifyAlteration(xmm : TxmmRegister);
begin
   FAlteredXMM := FAlteredXMM or (1 shl Ord(xmm));
end;

// ------------------
// ------------------ TFixupPostamble ------------------
// ------------------

// Create
//
constructor TFixupPostamble.Create(preamble : TFixupPreamble);
begin
   inherited Create;
   FPreamble:=preamble;
end;

// GetSize
//
function TFixupPostamble.GetSize : Integer;
begin
   Result := FPreamble.FUnwind.EpilogSize;
end;

// Write
//
procedure TFixupPostamble.Write(output : TWriteOnlyBlockStream);
var
   x86 : Tx86_64_WriteOnlyStream;
begin
   x86:=(output as Tx86_64_WriteOnlyStream);

   x86.WriteBytes(FPreamble.FUnwind.Epilog);
end;

// ------------------
// ------------------ TStaticDataFixup ------------------
// ------------------

// Create
//
constructor TStaticDataFixup.Create(const aPrefixBytes : TBytes);
begin
   inherited Create;
   FPrefixBytes := Copy(aPrefixBytes);
end;

// GetSize
//
function TStaticDataFixup.GetSize : Integer;
begin
   Result := Length(FPrefixBytes) + 4;
end;

// Write
//
procedure TStaticDataFixup.Write(output : TWriteOnlyBlockStream);
begin
   output.WriteBytes(FPrefixBytes);
   output.WriteInt32(RelativeOffset - GetSize);
end;

// RelativeOffset
//
function TStaticDataFixup.RelativeOffset : Integer;
begin
   Result := Logic.StaticDataBase + DataIndex*SizeOf(UInt64) - FixedLocation;
end;

// ------------------
// ------------------ Tx86_64FixupLogic ------------------
// ------------------

// NewHangingTarget
//
function Tx86_64FixupLogic.NewHangingTarget(align : Boolean) : TFixupTarget;
begin
   if align and not (jitoNoBranchAlignment in Options) then
      Result:=TFixupAlignedTarget.Create
   else Result:=TFixupTarget.Create;
   Result.Logic:=Self;
end;

// AddStaticData
//
function Tx86_64FixupLogic.AddStaticData(const data : UInt64) : Integer;
var
   i : Integer;
begin
   Result := Length(FStaticData);
   for i := 0 to Result-1 do
      if FStaticData[i] = data then
         Exit(i);

   if FStaticDataFree8BytesSlot >= 0 then begin
      Result := FStaticDataFree8BytesSlot;
      FStaticDataFree8BytesSlot := -1;
   end else SetLength(FStaticData, Result+1);

   FStaticData[Result] := data;
end;

// AddStaticData128
//
function Tx86_64FixupLogic.AddStaticData128(const data1, data2 : UInt64) : Integer;
var
   i : Integer;
begin
   Result := Length(FStaticData);
   i := 0;
   while i < Result-2 do begin
      if (FStaticData[i] = data1) and (FStaticData[i+1] = data2) then Exit(i);
      Inc(i, 2);
   end;

   if (Result and 1) <> 0 then begin
      // align 16
      FStaticDataFree8BytesSlot := Result;
      Inc(Result);
   end;

   SetLength(FStaticData, Result+2);
   FStaticData[Result] := data1;
   FStaticData[Result+1] := data2;
end;

// CompileStaticData
//
function Tx86_64FixupLogic.CompileStaticData : TBytes;
var
   n : Integer;
begin
   n := Length(FStaticData)*SizeOf(UInt64);
   SetLength(Result, n);
   if n > 0 then
      System.Move(FStaticData[0], Result[0], n);
end;

// AfterResolve
//
procedure Tx86_64FixupLogic.AfterResolve;
var
   iter : TFixup;
begin
   iter := Base;
   Assert(iter <> nil);
   while iter.Next <> nil do
      iter := iter.Next;
   // StaticData Base is at least 8 bytes after tail, with 16-byte alignment
   StaticDataBase := ((iter.FixedLocation + iter.GetSize + 8 + 15) shr 4) shl 4;
end;

// ClearFixups
//
procedure Tx86_64FixupLogic.ClearFixups;
begin
   inherited;
   FStaticData := nil;
   FStaticDataBase := 0;
   FStaticDataFree8BytesSlot := -1;
end;

// ------------------
// ------------------ Tx86_64FixupLogicHelper ------------------
// ------------------

// NewJump
//
function Tx86_64FixupLogicHelper.NewJump(flags : TboolFlags) : TFixupJump;
begin
   Result:=TFixupJump.Create(flags);
   AddFixup(Result);
end;

// NewJump
//
function Tx86_64FixupLogicHelper.NewJump(flags : TboolFlags; target : TFixup) : TFixupJump;
begin
   if target<>nil then begin
      Result:=NewJump(flags);
      Result.Target:=target;
   end else Result:=nil;
end;

// NewJump
//
function Tx86_64FixupLogicHelper.NewJump(target : TFixup) : TFixupJump;
begin
   Result:=NewJump(flagsNone, target);
end;

// NewConditionalJumps
//
procedure Tx86_64FixupLogicHelper.NewConditionalJumps(flagsTrue : TboolFlags; targetTrue, targetFalse : TFixup);
begin
   if (targetTrue<>nil) and (targetTrue.Location<>0) then begin
      NewJump(flagsTrue, targetTrue);
      NewJump(NegateBoolFlags(flagsTrue), targetFalse);
   end else begin
      NewJump(NegateBoolFlags(flagsTrue), targetFalse);
      NewJump(flagsTrue, targetTrue);
   end;
end;

// NewMovsdRegImm
//
function Tx86_64FixupLogicHelper.NewMovsdRegImm(reg : TxmmRegister; const value : Double) : TStaticDataFixup;
begin
   if reg < xmm8 then
      Result := TStaticDataFixup.Create([$F2, $0F, $10, $05 + 8*(Ord(reg) and 7)])
   else Result := TStaticDataFixup.Create([$F2, $44, $0F, $10, $05 + 8*(Ord(reg) and 7)]);
   Result.Logic := Self;
   Result.DataIndex := AddStaticData(PUInt64(@value)^);
   AddFixup(Result);
end;

// NewComisdRegImm
//
function Tx86_64FixupLogicHelper.NewComisdRegImm(reg : TxmmRegister; const value : Double) : TStaticDataFixup;
begin
   if reg < xmm8 then
      Result := TStaticDataFixup.Create([$66, $0F, $2F, $05 + 8*(Ord(reg) and 7)])
   else Result := TStaticDataFixup.Create([$66, $44, $0F, $2F, $05 + 8*(Ord(reg) and 7)]);
   Result.Logic := Self;
   Result.DataIndex := AddStaticData(PUInt64(@value)^);
   AddFixup(Result);
end;

// NewNegRegImm
//
function Tx86_64FixupLogicHelper.NewNegRegImm(reg : TxmmRegister) : TStaticDataFixup;
begin
   if reg < xmm8 then
      Result := TStaticDataFixup.Create([$0F, $57, $05 + 8*(Ord(reg) and 7)])
   else Result := TStaticDataFixup.Create([$44, $0F, $57, $05 + 8*(Ord(reg) and 7)]);
   Result.Logic := Self;
   Result.DataIndex := AddStaticData128($8000000000000000, $8000000000000000);
   AddFixup(Result);
end;

// NewOpRegImm
//
function Tx86_64FixupLogicHelper.NewOpRegImm(op : TxmmOp; reg : TxmmRegister; const value : Double) : TStaticDataFixup;
begin
   if reg < xmm8 then
      Result := TStaticDataFixup.Create([$F2, $0F, Ord(op), $05 + 8*(Ord(reg) and 7)])
   else Result := TStaticDataFixup.Create([$F2, $44, $0F, Ord(op), $05 + 8*(Ord(reg) and 7)]);
   Assert(reg in [xmm0..High(TxmmRegister)]);
   Result.Logic := Self;
   Result.DataIndex := AddStaticData(PUInt64(@value)^);
   AddFixup(Result);
end;

// AddStaticData
//
function Tx86_64FixupLogicHelper.AddStaticData(const data : UInt64) : Integer;
begin
   Result := (Self as Tx86_64FixupLogic).AddStaticData(data);
end;

// AddStaticData128
//
function Tx86_64FixupLogicHelper.AddStaticData128(const data1, data2 : UInt64) : Integer;
begin
   Result := (Self as Tx86_64FixupLogic).AddStaticData128(data1, data2);
end;

// StaticData
//
function Tx86_64FixupLogicHelper.StaticData : TBytes;
begin
   Result := (Self as Tx86_64FixupLogic).CompileStaticData;
end;

// GetStaticDataBase
//
function Tx86_64FixupLogicHelper.GetStaticDataBase : Integer;
begin
   Result := (Self as Tx86_64FixupLogic).StaticDataBase;
end;

// SetStaticDataBase
//
procedure Tx86_64FixupLogicHelper.SetStaticDataBase(v : Integer);
begin
   (Self as Tx86_64FixupLogic).StaticDataBase := v;
end;

// ------------------
// ------------------ TdwsJITter_x86 ------------------
// ------------------

// Create
//
constructor TdwsJITter_x86.Create(jit : TdwsJITx86_64);
begin
   inherited Create(jit);
   FJIT:=jit;
   Fx86:=jit.x86;
end;

// CompileFloat
//
function TdwsJITter_x86.CompileFloat(expr : TTypedExpr) : Integer;
begin
   Result:=Ord(DoCompileFloat(expr));
end;

// DoCompileFloat
//
function TdwsJITter_x86.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
begin
   jit.CompileInteger(expr);

   Result:=jit.AllocXMMReg(expr);

   x86._cvtsi2sd(Result, gprRAX);
end;

// CompileAssignFloat
//
procedure TdwsJITter_x86.CompileAssignFloat(expr : TTypedExpr; source : Integer);
begin
   DoCompileAssignFloat(expr, TxmmRegister(source));
end;

// DoCompileAssignFloat
//
procedure TdwsJITter_x86.DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister);
begin
   jit.OutputFailedOn:=expr;
end;

// CompileBooleanValue
//
function TdwsJITter_x86.CompileBooleanValue(expr : TTypedExpr) : Integer;
var
   targetTrue, targetFalse, targetDone : TFixupTarget;
begin
   targetTrue:=jit.Fixups.NewHangingTarget(False);
   targetFalse:=jit.Fixups.NewHangingTarget(False);
   targetDone:=jit.Fixups.NewHangingTarget(False);

   jit.CompileBoolean(expr, targetTrue, targetFalse);

   jit.Fixups.AddFixup(targetFalse);
   x86._mov_al_byte(0);
   jit.Fixups.NewJump(targetDone);

   jit.Fixups.AddFixup(targetTrue);
   x86._mov_al_byte(1);

   jit.Fixups.AddFixup(targetDone);

   Result:=0;
end;

// CompileBoolean
//
procedure TdwsJITter_x86.CompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
begin
   DoCompileBoolean(TBooleanBinOpExpr(expr), targetTrue, targetFalse);
end;

// DoCompileBoolean
//
procedure TdwsJITter_x86.DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
begin
   jit.OutputFailedOn:=expr;
end;

// ------------------
// ------------------ TProgramExpr86 ------------------
// ------------------

// EvalNoResult
//
procedure TProgramExpr86.EvalNoResult(exec : TdwsExecution);
asm
   call [rcx + OFFSET FCodePtr]
end;

// ------------------
// ------------------ TFloatExpr86 ------------------
// ------------------

// EvalAsFloat
//
function TFloatExpr86.EvalAsFloat(exec : TdwsExecution) : Double;
asm
   call [rcx + OFFSET FCodePtr]
end;

// ------------------
// ------------------ TIntegerExpr86 ------------------
// ------------------

// EvalAsInteger
//
function TIntegerExpr86.EvalAsInteger(exec : TdwsExecution) : Int64;
asm
   call [rcx + OFFSET FCodePtr]
end;

// ------------------
// ------------------ Tx86AssignConstToFloatVar ------------------
// ------------------

// CompileStatement
//
procedure Tx86AssignConstToFloatVar.CompileStatement(expr : TExprBase);
var
   e : TAssignConstToFloatVarExpr;
   reg : TxmmRegister;
begin
   e:=TAssignConstToFloatVarExpr(expr);

   reg := jit.AllocXMMReg(e.Left);

   // check below is necessary as -Nan will be reported equal to zero
   if (not IsNaN(e.Right)) and (e.Right=0)  then
      x86._xorps_reg_reg(reg, reg)
   else jit._movsd_reg_imm(reg, e.Right);

   jit.CompileAssignFloat(e.Left, reg);
end;

// ------------------
// ------------------ Tx86AssignConstToIntegerVar ------------------
// ------------------

// CompileStatement
//
procedure Tx86AssignConstToIntegerVar.CompileStatement(expr : TExprBase);
var
   e : TAssignConstToIntegerVarExpr;
begin
   e:=TAssignConstToIntegerVarExpr(expr);

   if e.Left.ClassType=TIntVarExpr then begin

      x86._mov_execmem_imm(TVarExpr(e.Left).StackAddr, e.Right);

   end else inherited;
end;
{
// ------------------
// ------------------ Tx86AssignConstToBoolVar ------------------
// ------------------

// CompileStatement
//
procedure Tx86AssignConstToBoolVar.CompileStatement(expr : TExprBase);
var
   e : TAssignConstToBoolVarExpr;
begin
   e:=TAssignConstToBoolVarExpr(expr);

   if e.Left.ClassType=TBoolVarExpr then begin

      if e.Right then
         x86._mov_reg_dword(gprEAX, 1)
      else x86._xor_reg_reg(gprEAX, gprEAX);
      x86._mov_execmem_reg(TVarExpr(e.Left).StackAddr, 0, gprEAX);

   end else inherited;
end;
}
// ------------------
// ------------------ Tx86Assign ------------------
// ------------------

// CompileStatement
//
procedure Tx86Assign.CompileStatement(expr : TExprBase);
var
   e : TAssignExpr;
   reg : TxmmRegister;
begin
   e:=TAssignExpr(expr);

   if jit.IsFloat(e.Left) then begin

      reg:=jit.CompileFloat(e.Right);
      jit.CompileAssignFloat(e.Left, reg);

   end else if jit.IsInteger(e.Left) then begin

      jit.CompileInteger(e.Right);
      jit.CompileAssignInteger(e.Left, 0);

   end else if jit.IsBoolean(e.Left) then begin

      jit.CompileBooleanValue(e.Right);
      jit.CompileAssignBoolean(e.Left, 0);

   end else inherited;
end;
{
// ------------------
// ------------------ Tx86AssignData ------------------
// ------------------

// CompileStatement
//
procedure Tx86AssignData.CompileStatement(expr : TExprBase);
var
   e : TAssignDataExpr;
   size : Integer;
begin
   e:=TAssignDataExpr(expr);

   if e.Right.Typ.UnAliasedType.ClassType=TSetOfSymbol then begin

      size:=e.Right.Typ.Size;
      if size=1 then begin

         jit.CompileInteger(e.Right);
         jit.CompileAssignInteger(e.Left, 0);

      end else inherited;

   end else inherited;
end;

// ------------------
// ------------------ Tx86OpAssignFloat ------------------
// ------------------

// Create
//
constructor Tx86OpAssignFloat.Create(jit : TdwsJITx86_64; op : TxmmOp);
begin
   inherited Create(jit);
   Self.OP:=op;
end;

// CompileStatement
//
procedure Tx86OpAssignFloat.CompileStatement(expr : TExprBase);
var
   e : TOpAssignExpr;
   reg, regRight : TxmmRegister;
   jitLeft : TdwsJITter;
   delta : Integer;
begin
   e:=TOpAssignExpr(expr);

   regRight:=jit.CompileFloat(e.Right);

   if e.Left is TDynamicArrayExpr then begin

      jitLeft:=jit.FindJITter(e.Left.ClassType);

      (jitLeft as Tx86DynamicArray).CompileAsItemPtr(TDynamicArrayExpr(e.Left), delta);

      reg:=jit.AllocXMMReg(e.Left);
      x86._movsd_reg_qword_ptr_indexed(reg, gprEAX, gprECX, 1, delta);
      x86._xmm_reg_reg(OP, reg, regRight);
      x86._movsd_qword_ptr_indexed_reg(gprEAX, gprECX, 1, delta, reg);

   end else begin

      reg:=jit.CompileFloat(e.Left);

      x86._xmm_reg_reg(OP, reg, regRight);

      jit.CompileAssignFloat(e.Left, reg);

   end;

   if regRight<>reg then
      jit.ReleaseXMMReg(regRight);
end;
}
// ------------------
// ------------------ Tx86Null ------------------
// ------------------

// CompileStatement
//
procedure Tx86Null.CompileStatement(expr : TExprBase);
begin
   // nothing
end;

// ------------------
// ------------------ Tx86BlockExprNoTable ------------------
// ------------------

// CompileStatement
//
procedure Tx86BlockExprNoTable.CompileStatement(expr : TExprBase);
var
   i : Integer;
   subExpr : TExprBase;
begin
   for i:=0 to expr.SubExprCount-1 do begin
      if jit.OutputFailedOn<>nil then break;
      subExpr:=expr.SubExpr[i];
      jit._DoStep(subExpr);
      jit.CompileStatement(subExpr);
      jit.FlushXMMRegs(xmmFlushIntermediateExprs);
   end;
end;

// ------------------
// ------------------ Tx86IfThen ------------------
// ------------------

// CompileStatement
//
procedure Tx86IfThen.CompileStatement(expr : TExprBase);
var
   e : TIfThenExpr;
   targetTrue, targetFalse : TFixupTarget;
begin
   e:=TIfThenExpr(expr);

   targetTrue:=jit.Fixups.NewHangingTarget(False);
   targetFalse:=jit.Fixups.NewHangingTarget(False);

   jit.CompileBoolean(e.CondExpr, targetTrue, targetFalse);

   jit.Fixups.AddFixup(targetTrue);

   jit.CompileStatement(e.ThenExpr);

   jit.Fixups.AddFixup(targetFalse);

   jit.ResetXMMReg;
end;

// ------------------
// ------------------ Tx86IfThenElse ------------------
// ------------------

// CompileStatement
//
procedure Tx86IfThenElse.CompileStatement(expr : TExprBase);
var
   e : TIfThenElseExpr;
   targetTrue, targetFalse, targetDone : TFixupTarget;
begin
   e:=TIfThenElseExpr(expr);

   targetTrue:=jit.Fixups.NewHangingTarget(False);
   targetFalse:=jit.Fixups.NewHangingTarget(True);
   targetDone:=jit.Fixups.NewHangingTarget(False);

   jit.CompileBoolean(e.CondExpr, targetTrue, targetFalse);

   jit.Fixups.AddFixup(targetTrue);

   jit.CompileStatement(e.ThenExpr);
   jit.Fixups.NewJump(flagsNone, targetDone);

   jit.Fixups.AddFixup(targetFalse);

   jit.CompileStatement(e.ElseExpr);

   jit.Fixups.AddFixup(targetDone);

   jit.ResetXMMReg;
end;

// ------------------
// ------------------ Tx86Loop ------------------
// ------------------

// CompileStatement
//
procedure Tx86Loop.CompileStatement(expr : TExprBase);
var
   e : TLoopExpr;
   targetLoop, targetExit : TFixupTarget;
begin
   e:=TLoopExpr(expr);

   jit.FlushXMMRegs(xmmFlushAll);

   targetLoop:=jit.Fixups.NewTarget(True);
   targetExit:=jit.Fixups.NewHangingTarget(False);

   jit.EnterLoop(targetLoop, targetExit);

   jit._DoStep(e.LoopExpr);
   jit.CompileStatement(e.LoopExpr);

   jit.Fixups.NewJump(flagsNone, targetLoop);

   jit.Fixups.AddFixup(targetExit);

   jit.LeaveLoop;

   jit.FlushXMMRegs(xmmFlushAll);
end;

// ------------------
// ------------------ Tx86Repeat ------------------
// ------------------

// CompileStatement
//
procedure Tx86Repeat.CompileStatement(expr : TExprBase);
var
   e : TLoopExpr;
   targetLoop, targetExit : TFixupTarget;
begin
   e:=TLoopExpr(expr);

   jit.FlushXMMRegs(xmmFlushAll);

   targetLoop:=jit.Fixups.NewTarget(True);
   targetExit:=jit.Fixups.NewHangingTarget(False);

   jit.EnterLoop(targetLoop, targetExit);

   jit._DoStep(e.LoopExpr);
   jit.CompileStatement(e.LoopExpr);

   jit._DoStep(e.CondExpr);
   jit.CompileBoolean(e.CondExpr, targetExit, targetLoop);

   jit.Fixups.AddFixup(targetExit);

   jit.LeaveLoop;

   jit.FlushXMMRegs(xmmFlushAll);
end;
{
// ------------------
// ------------------ Tx86While ------------------
// ------------------

// CompileStatement
//
procedure Tx86While.CompileStatement(expr : TExprBase);
var
   e : TLoopExpr;
   targetLoop, targetLoopStart, targetExit : TFixupTarget;
begin
   e:=TLoopExpr(expr);

   jit.ResetXMMReg;

   targetLoop:=jit.Fixups.NewTarget(True);
   targetLoopStart:=jit.Fixups.NewHangingTarget(False);
   targetExit:=jit.Fixups.NewHangingTarget(True);

   jit.EnterLoop(targetLoop, targetExit);

   jit._DoStep(e.CondExpr);
   jit.CompileBoolean(e.CondExpr, targetLoopStart, targetExit);

   jit.Fixups.AddFixup(targetLoopStart);

   jit._DoStep(e.LoopExpr);
   jit.CompileStatement(e.LoopExpr);

   jit.Fixups.NewJump(flagsNone, targetLoop);

   jit.Fixups.AddFixup(targetExit);

   jit.LeaveLoop;

   jit.ResetXMMReg;
end;
}
// ------------------
// ------------------ Tx86ForUpward ------------------
// ------------------

// CompileStatement
//
var
   cPtr_TForStepExpr_RaiseForLoopStepShouldBeStrictlyPositive : Pointer = @TForStepExpr.RaiseForLoopStepShouldBeStrictlyPositive;
procedure Tx86ForUpward.CompileStatement(expr : TExprBase);
var
   e : TForExpr;
   loopStart : TFixupTarget;
   loopContinue : TFixupTarget;
   loopAfter : TFixupTarget;
   stepCheckPassed : TFixupJump;
   fromValue, toValue, stepValue : Int64;
   toValueIsConstant : Boolean;
   toValueOffset : Integer;
   stepValueIsConstant : Boolean;
   stepValueOffset : Integer;
begin
   e:=TForExpr(expr);

   jit.FlushXMMRegs(xmmFlushAll);

   toValueIsConstant:=(e.ToExpr is TConstIntExpr);
   if toValueIsConstant then begin
      toValue:=TConstIntExpr(e.ToExpr).Value;
   end else begin
      toValue:=0;
   end;

   if e.FromExpr is TConstIntExpr then begin

      fromValue:=TConstIntExpr(e.FromExpr).Value;

      x86._mov_execmem_imm(e.VarExpr.StackAddr, fromValue);

   end else begin

      jit.CompileInteger(e.FromExpr);
      x86._mov_execmem_reg(e.VarExpr.StackAddr, gprRAX);

   end;

   if not toValueIsConstant then begin

      jit.CompileInteger(e.ToExpr);
      toValueOffset:=jit._store_rax;

   end else toValueOffset:=0;

   if e is TForUpwardStepExpr then begin

      if TForUpwardStepExpr(e).StepExpr is TConstIntExpr then begin

         stepValueIsConstant:=True;
         stepValue:=TConstIntExpr(TForUpwardStepExpr(e).StepExpr).Value;
         stepValueOffset:=0;

      end else begin

         stepValueIsConstant:=False;
         stepValue:=0;
         jit.CompileInteger(TForUpwardStepExpr(e).StepExpr);

         x86._cmp_reg_imm(gprRDX, 0);
         stepCheckPassed:=jit.Fixups.NewJump(flagsGE);

         // todo fix registers
         Assert(False);
         x86._push_reg(gprRDX);
         x86._push_reg(gprRAX);
         jit._mov_reg_execInstance(gprRDX);
         x86._mov_reg_dword(gprRAX, DWORD(expr));
         x86._call_absmem(@cPtr_TForStepExpr_RaiseForLoopStepShouldBeStrictlyPositive);

         stepCheckPassed.NewTarget(True);

         stepValueOffset:=jit._store_rax;

      end;

   end else begin

      stepValueIsConstant:=True;
      stepValue:=1;
      stepValueOffset:=0;

   end;

   loopStart:=jit.Fixups.NewTarget(True);
   loopContinue:=jit.Fixups.NewHangingTarget(False);
   loopAfter:=jit.Fixups.NewHangingTarget(True);

   jit.EnterLoop(loopContinue, loopAfter);

   if toValueIsConstant then
      x86._cmp_execmem_imm(e.VarExpr.StackAddr, toValue)
   else begin
      x86._mov_reg_qword_ptr_reg(gprRAX, gprRBP, toValueOffset);
      x86._cmp_execmem_reg(e.VarExpr.StackAddr, gprRAX);
   end;
   jit.Fixups.NewJump(flagsG, loopAfter);

   jit._DoStep(e.DoExpr);
   jit.CompileStatement(e.DoExpr);

   jit.Fixups.AddFixup(loopContinue);

   if stepValueIsConstant then
      x86._add_execmem_imm(e.VarExpr.StackAddr, stepValue)
   else begin
      jit._restore_rax(stepValueOffset);
      x86._add_execmem_reg(e.VarExpr.StackAddr, gprRAX);
   end;

   jit.Fixups.NewJump(flagsNone, loopStart);

   if JIT.LoopContext.Exited then
      jit.FlushXMMRegs(xmmFlushAll);

   jit.LeaveLoop;

   jit.Fixups.AddFixup(loopAfter);
end;
{
// ------------------
// ------------------ Tx86Continue ------------------
// ------------------

// CompileStatement
//
procedure Tx86Continue.CompileStatement(expr : TExprBase);
begin
   if jit.LoopContext<>nil then
      jit.Fixups.NewJump(flagsNone, jit.LoopContext.TargetContinue)
   else jit.OutputFailedOn:=expr;
end;

// ------------------
// ------------------ Tx86Break ------------------
// ------------------

// CompileStatement
//
procedure Tx86Break.CompileStatement(expr : TExprBase);
begin
   if jit.LoopContext<>nil then
      jit.Fixups.NewJump(flagsNone, jit.LoopContext.TargetExit)
   else jit.OutputFailedOn:=expr;
end;

// ------------------
// ------------------ Tx86Exit ------------------
// ------------------

// CompileStatement
//
procedure Tx86Exit.CompileStatement(expr : TExprBase);
begin
   if jit.ExitTarget<>nil then
      jit.Fixups.NewJump(flagsNone, jit.ExitTarget)
   else jit.OutputFailedOn:=expr;
end;

// ------------------
// ------------------ Tx86ExitValue ------------------
// ------------------

// CompileStatement
//
procedure Tx86ExitValue.CompileStatement(expr : TExprBase);
begin
   jit.CompileStatement(TExitValueExpr(expr).AssignExpr);

   inherited;
end;}

// ------------------
// ------------------ Tx86FloatBinOp ------------------
// ------------------

// Create
//
constructor Tx86FloatBinOp.Create(jit : TdwsJITx86_64; op : TxmmOp);
begin
   inherited Create(jit);
   Self.OP:=op;
end;

// CompileFloat
//
function Tx86FloatBinOp.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
var
   e : TFloatBinOpExpr;
begin
   e := TFloatBinOpExpr(expr);

   if (OP in [ xmm_addsd, xmm_multsd ]) and (e.Left is TConstExpr) then begin
      Result := jit.CompileFloat(e.Right);
      jit._xmm_reg_expr(OP, Result, e.Left);
   end else begin
      Result := jit.CompileFloat(e.Left);
      jit._xmm_reg_expr(OP, Result, e.Right);
   end;

   jit.SetContainsXMMReg(Result, expr);
end;

// ------------------
// ------------------ Tx86ModFloat ------------------
// ------------------

// DoCompileFloat
//
{function Tx86ModFloat.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
var
   e : TModFloatExpr;
   regLeft, regRight : TxmmRegister;
   loop : TFixupTarget;
begin
   e:=TModFloatExpr(expr);

   jit.FPreamble.NeedTempSpace(SizeOf(Double));

   regRight:=jit.CompileFloat(e.Right);
   x86._movsd_esp_reg(regRight);
   x86._fld_esp;
   jit.ReleaseXMMReg(regRight);

   regLeft:=jit.CompileFloat(e.Left);
   x86._movsd_esp_reg(regLeft);
   x86._fld_esp;
   jit.ReleaseXMMReg(regLeft);

   loop:=jit.Fixups.NewTarget(False);
   x86.WriteBytes([$D9, $F8]); // fprem
   x86.WriteBytes([$DF, $E0]); // fnstsw ax
   x86.WriteByte($9E);         // sahf
   jit.Fixups.NewJump(flagsP, loop);

   x86._ffree(1);

   x86._fstp_esp;
   Result:=jit.AllocXMMReg(expr);
   x86._movsd_reg_esp(Result);
end;}

// ------------------
// ------------------ Tx86SqrFloat ------------------
// ------------------

// CompileFloat
//
function Tx86SqrFloat.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
var
   e : TSqrFloatExpr;
begin
   e:=TSqrFloatExpr(expr);

   Result:=CompileFloatOperand(e, e.Expr);
end;

// CompileFloatOperand
//
function Tx86SqrFloat.CompileFloatOperand(sqrExpr, operand : TTypedExpr) : TxmmRegister;
var
   reg : TxmmRegister;
begin
   reg:=jit.CompileFloat(operand);

   if operand.ClassType=TFloatVarExpr then begin

      Result := jit.AllocXMMReg(sqrExpr);

      if x86.SupportsAVX then begin
         x86._vmulsd(Result, reg, reg);
      end else begin
         x86._movsd_reg_reg(Result, reg);
         x86._xmm_reg_reg(xmm_multsd, Result, reg);
      end;

      jit.ReleaseXMMReg(reg);

   end else begin

      Result:=reg;

      x86._xmm_reg_reg(xmm_multsd, Result, Result);

      jit.SetContainsXMMReg(Result, sqrExpr);

   end;
end;

// ------------------
// ------------------ Tx86NegFloat ------------------
// ------------------

// DoCompileFloat
//
function Tx86NegFloat.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
var
   e : TNegFloatExpr;
begin
   e:=TNegFloatExpr(expr);

   Result:=jit.CompileFloat(e.Expr);

   jit.Fixups.NewNegRegImm(Result);

   jit.SetContainsXMMReg(Result, expr);
end;

// ------------------
// ------------------ Tx86ConstFloat ------------------
// ------------------

// CompileFloat
//
function Tx86ConstFloat.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
var
   e : TConstFloatExpr;
begin
   e:=TConstFloatExpr(expr);

   Result:=jit.AllocXMMReg(e);

   jit.Fixups.NewMovsdRegImm(Result, e.Value);
//   x86._movsd_reg_absmem(Result, @e.Value);
end;

// ------------------
// ------------------ Tx86ConstInt ------------------
// ------------------

// CompileFloat
//
function Tx86ConstInt.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
var
   e : TConstIntExpr;
begin
   e := TConstIntExpr(expr);

   if (e.Value>-MaxInt) and (e.Value<MaxInt) then begin

      Result := jit.AllocXMMReg(e);

      x86._xmm_reg_absmem(xmm_cvtsi2sd, Result, @e.Value);

   end else Result := inherited;
end;

// CompileInteger
//
function Tx86ConstInt.CompileInteger(expr : TTypedExpr) : Integer;
var
   e : TConstIntExpr;
begin
   e:=TConstIntExpr(expr);

   x86._mov_reg_imm(gprRAX, e.Value);

   Result:=0;
end;

// ------------------
// ------------------ Tx86ConstBoolean ------------------
// ------------------

// CompileInteger
//
function Tx86ConstBoolean.CompileInteger(expr : TTypedExpr) : Integer;
begin
   if TConstBooleanExpr(expr).Value then
      x86._mov_reg_imm(gprRAX, 1)
   else x86._mov_reg_imm(gprRAX, 0);
   Result:=0;
end;

// CompileBooleanValue
//
function Tx86ConstBoolean.CompileBooleanValue(expr : TTypedExpr) : Integer;
begin
   Result := CompileInteger(expr);
end;

// DoCompileBoolean
//
procedure Tx86ConstBoolean.DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
begin
   if TConstBooleanExpr(expr).Value then
      jit.Fixups.NewJump(targetTrue)
   else jit.Fixups.NewJump(targetFalse);
end;

// ------------------
// ------------------ Tx86InterpretedExpr ------------------
// ------------------

// DoCallEval
//
procedure Tx86InterpretedExpr.DoCallEval(expr : TExprBase; vmt : Integer);
begin
   jit._mov_reg_execInstance(gprRDX);
   x86._mov_reg_qword(gprRCX, QWORD(expr));
   x86._mov_reg_qword_ptr_reg(gprRAX, gprRCX);

   x86._call_reg(gprRAX, vmt);

   if jit.FSavedXMM = [] then
      jit.FlushXMMRegs(xmmFlushVolatile);

   jit.QueueGreed(expr);
end;

// CompileStatement
//
procedure Tx86InterpretedExpr.CompileStatement(expr : TExprBase);
begin
   DoCallEval(expr, vmt_TExprBase_EvalNoResult);

   jit._mov_reg_execStatus(gprRAX);

   x86._cmp_reg_imm(gprRAX, Ord(esrExit));
   if jit.ExitTarget<>nil then
      jit.Fixups.NewJump(flagsE, jit.ExitTarget)
   else jit.OutputFailedOn:=expr;

   if jit.LoopContext<>nil then begin

      jit._mov_execStatus_imm(0);

      x86._cmp_reg_imm(gprRAX, Ord(esrBreak));
      jit.Fixups.NewJump(flagsE, jit.LoopContext.TargetExit);
      x86._cmp_reg_imm(gprRAX, Ord(esrContinue));
      jit.Fixups.NewJump(flagsE, jit.LoopContext.TargetContinue);

   end;
end;

// DoCompileFloat
//
function Tx86InterpretedExpr.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
begin
   if jit.IsInteger(expr) then
      Exit(inherited DoCompileFloat(expr));

   jit.SaveXMMRegs;

   DoCallEval(expr, vmt_TExprBase_EvalAsFloat);

   jit.RestoreXMMRegs;

   Result := jit.AllocXMMReg(expr);
   x86._movsd_reg_reg(Result, xmm0);

   jit.QueueGreed(expr);
end;

// DoCompileAssignFloat
//
procedure Tx86InterpretedExpr.DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister);
begin
   jit.ReleaseXMMReg(source);
   jit.SaveXMMRegs;

   x86._movsd_reg_reg(xmm2, source);   // passed as 3rd param (first is self, second is exec)

   DoCallEval(expr, vmt_TExprBase_AssignValueAsFloat);

   jit.RestoreXMMRegs;
end;

// CompileInteger
//
function Tx86InterpretedExpr.CompileInteger(expr : TTypedExpr) : Integer;
begin
   jit.SaveXMMRegs;

   DoCallEval(expr, vmt_TExprBase_EvalAsInteger);

   jit.RestoreXMMRegs;

   jit.QueueGreed(expr);

   Result:=0;
end;

// CompileAssignInteger
//
procedure Tx86InterpretedExpr.CompileAssignInteger(expr : TTypedExpr; source : Integer);
begin
   jit.SaveXMMRegs;

   x86._mov_reg_reg(gprR8, gprRAX);
   DoCallEval(expr, vmt_TExprBase_AssignValueAsInteger);

   jit.RestoreXMMRegs;
end;

// DoCompileBoolean
//
procedure Tx86InterpretedExpr.DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
begin
   jit.SaveXMMRegs;

   DoCallEval(expr, vmt_TExprBase_EvalAsBoolean);

   jit.RestoreXMMRegs;

   x86._test_al_al;
   jit.Fixups.NewConditionalJumps(flagsNZ, targetTrue, targetFalse);

   jit.QueueGreed(expr);
end;

// CompileBooleanValue
//
function Tx86InterpretedExpr.CompileBooleanValue(expr : TTypedExpr) : Integer;
begin
   jit.SaveXMMRegs;

   DoCallEval(expr, vmt_TExprBase_EvalAsBoolean);

   jit.RestoreXMMRegs;

   x86._op_reg_imm(gpOp_and, gprRAX, 255);

   jit.QueueGreed(expr);

   Result:=0;
end;

// ------------------
// ------------------ Tx86FloatVar ------------------
// ------------------

// CompileFloat
//
function Tx86FloatVar.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
var
   e : TFloatVarExpr;
begin
   e := TFloatVarExpr(expr);

   Result:=jit.CurrentXMMReg(e);

   if Result=xmmNone then begin

      Result := jit.AllocXMMReg(e);
      x86._movsd_reg_execmem(Result, e.StackAddr);

   end;
end;

// DoCompileAssignFloat
//
procedure Tx86FloatVar.DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister);
var
   e : TFloatVarExpr;
begin
   e:=TFloatVarExpr(expr);

   x86._movsd_execmem_reg(e.StackAddr, source);

   jit.SetContainsXMMReg(source, e);
end;

// ------------------
// ------------------ Tx86IntVar ------------------
// ------------------

// CompileFloat
//
function Tx86IntVar.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
var
   e : TIntVarExpr;
begin
   e:=TIntVarExpr(expr);

   Result:=jit.AllocXMMReg(e);

   x86._mov_reg_execmem(gprRAX, e.StackAddr);
   x86._cvtsi2sd(Result, gprRAX);
end;

// CompileInteger
//
function Tx86IntVar.CompileInteger(expr : TTypedExpr) : Integer;
var
   e : TIntVarExpr;
begin
   e:=TIntVarExpr(expr);

   x86._mov_reg_execmem(gprRAX, e.StackAddr);

   Result:=0;
end;

// CompileAssignInteger
//
procedure Tx86IntVar.CompileAssignInteger(expr : TTypedExpr; source : Integer);
var
   e : TIntVarExpr;
begin
   e:=TIntVarExpr(expr);

   x86._mov_execmem_reg(e.StackAddr, gprRAX);
end;
{
// ------------------
// ------------------ Tx86BoolVar ------------------
// ------------------

// DoCompileBoolean
//
procedure Tx86BoolVar.DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
var
   e : TBoolVarExpr;
begin
   e:=TBoolVarExpr(expr);

   x86._cmp_execmem_int32(e.StackAddr, 0, 0);
   jit.Fixups.NewJump(flagsNZ, targetTrue);
   jit.Fixups.NewJump(targetFalse);
end;

// CompileAssignBoolean
//
procedure Tx86BoolVar.CompileAssignBoolean(expr : TTypedExpr; source : Integer);
var
   e : TBoolVarExpr;
begin
   e:=TBoolVarExpr(expr);

   x86._mov_dword_ptr_reg_reg(cExecMemGPR, StackAddrToOffset(e.StackAddr), gprEAX);
end;

// ------------------
// ------------------ Tx86ObjectVar ------------------
// ------------------

// CompileScriptObj
//
function Tx86ObjectVar.CompileScriptObj(expr : TTypedExpr) : Integer;
begin
   Result:=Ord(gprEAX);
   x86._mov_reg_execmem(gprEAX, TObjectVarExpr(expr).StackAddr);
end;

// ------------------
// ------------------ Tx86RecordVar ------------------
// ------------------

// CompileFloat
//
function Tx86RecordVar.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
var
   e : TRecordVarExpr;
begin
   e:=TRecordVarExpr(expr);

   Result:=jit.AllocXMMReg(e);
   if jit.IsFloat(e) then begin

      x86._movsd_reg_execmem(Result, e.VarPlusMemberOffset);

   end else if jit.IsInteger(e) then begin

      jit.FPreamble.NeedTempSpace(SizeOf(Double));
      x86._fild_execmem(e.VarPlusMemberOffset);
      x86._fstp_esp;
      x86._movsd_reg_esp(Result);

   end else Result:=inherited;
end;

// DoCompileAssignFloat
//
procedure Tx86RecordVar.DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister);
var
   e : TRecordVarExpr;
begin
   e:=TRecordVarExpr(expr);

   if jit.IsFloat(e) then begin

      x86._movsd_execmem_reg(e.VarPlusMemberOffset, source);
      jit.ReleaseXMMReg(source);

   end else inherited;
end;

// CompileInteger
//
function Tx86RecordVar.CompileInteger(expr : TTypedExpr) : Integer;
var
   e : TRecordVarExpr;
begin
   e:=TRecordVarExpr(expr);

   if jit.IsInteger(e) then begin

      x86._mov_eaxedx_execmem(e.VarPlusMemberOffset);
      Result:=0;

   end else Result:=inherited;
end;

// CompileAssignInteger
//
procedure Tx86RecordVar.CompileAssignInteger(expr : TTypedExpr; source : Integer);
var
   e : TRecordVarExpr;
begin
   e:=TRecordVarExpr(expr);

   if jit.IsInteger(e) then begin

      x86._mov_execmem_eaxedx(e.VarPlusMemberOffset);

   end else inherited;
end;

// DoCompileBoolean
//
procedure Tx86RecordVar.DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
var
   e : TRecordVarExpr;
begin
   e:=TRecordVarExpr(expr);

   if jit.IsBoolean(e) then begin

      x86._cmp_execmem_int32(e.VarPlusMemberOffset, 0, 0);
      jit.Fixups.NewConditionalJumps(flagsNZ, targetTrue, targetFalse);

   end else inherited;
end;

// ------------------
// ------------------ Tx86VarParam ------------------
// ------------------

// CompileAsPVariant
//
class procedure Tx86VarParam.CompileAsPVariant(x86 : Tx86_64_WriteOnlyStream; expr : TByRefParamExpr);
begin
   x86._mov_reg_execmem(gprEAX, expr.StackAddr);
   x86._xor_reg_reg(gprEDX, gprEDX);
   x86._mov_reg_dword_ptr_reg(gprECX, gprEAX);
   x86._call_reg(gprECX, vmt_IDataContext_AsPVariant);
end;

// DoCompileFloat
//
function Tx86VarParam.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
var
   e : TVarParamExpr;
begin
   e:=TVarParamExpr(expr);

   CompileAsPVariant(x86, e);

   if jit.IsFloat(e) then begin

      Result:=jit.AllocXMMReg(e, e.DataSym);
      x86._movsd_reg_qword_ptr_reg(Result, gprEAX, cVariant_DataOffset);

   end else Result:=inherited;
end;

// CompileInteger
//
function Tx86VarParam.CompileInteger(expr : TTypedExpr) : Integer;
var
   e : TVarParamExpr;
begin
   e:=TVarParamExpr(expr);

   CompileAsPVariant(x86, e);

   if jit.IsInteger(e) then begin

      x86._mov_eaxedx_qword_ptr_reg(gprEAX, cVariant_DataOffset);
      Result:=0;

   end else Result:=inherited;
end;

// DoCompileAssignFloat
//
procedure Tx86VarParam.DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister);
var
   e : TVarParamExpr;
begin
   e:=TVarParamExpr(expr);

   if jit.IsFloat(e) then begin

      CompileAsPVariant(x86, e);

      x86._movsd_qword_ptr_reg_reg(gprEAX, cVariant_DataOffset, source);

   end else inherited;
end;

// CompileAssignInteger
//
procedure Tx86VarParam.CompileAssignInteger(expr : TTypedExpr; source : Integer);
var
   e : TVarParamExpr;
   addr : Integer;
begin
   e:=TVarParamExpr(expr);

   if jit.IsInteger(e) then begin

      addr:=jit._store_eaxedx;

      CompileAsPVariant(x86, e);

      x86._mov_reg_reg(gprECX, gprEAX);
      jit._restore_eaxedx(addr);
      x86._mov_qword_ptr_reg_eaxedx(gprECX, cVariant_DataOffset);

   end else inherited;
end;

// ------------------
// ------------------ Tx86FieldVar ------------------
// ------------------

// CompileToData
//
procedure Tx86FieldVar.CompileToData(expr : TFieldVarExpr; dest : TgpRegister64);
begin
   jit.CompileScriptObj(expr.ObjectExpr);

   // TODO check object

   x86._mov_reg_dword_ptr_reg(dest, gprEAX, vmt_ScriptObjInstance_IScriptObj_To_FData);
end;

// CompileInteger
//
function Tx86FieldVar.CompileInteger(expr : TTypedExpr) : Integer;
var
   e : TFieldVarExpr;
begin
   e:=TFieldVarExpr(expr);

   CompileToData(e, gprECX);

   x86._mov_eaxedx_qword_ptr_reg(gprECX, e.FieldSym.Offset*SizeOf(Variant)+cVariant_DataOffset);

   Result:=0;
end;

// CompileAssignInteger
//
procedure Tx86FieldVar.CompileAssignInteger(expr : TTypedExpr; source : Integer);
var
   e : TFieldVarExpr;
   addr : Integer;
begin
   e:=TFieldVarExpr(expr);

   addr:=jit.FPreamble.AllocateStackSpace(SizeOf(Int64));
   x86._mov_qword_ptr_reg_eaxedx(gprEBP, addr);

   CompileToData(e, gprECX);

   x86._mov_eaxedx_qword_ptr_reg(gprEBP, addr);
   x86._mov_qword_ptr_reg_eaxedx(gprECX, e.FieldSym.Offset*SizeOf(Variant)+cVariant_DataOffset);
end;

// DoCompileBoolean
//
procedure Tx86FieldVar.DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
var
   e : TFieldVarExpr;
begin
   e:=TFieldVarExpr(expr);

   CompileToData(e, gprECX);

   x86._mov_reg_dword_ptr_reg(gprEAX, gprECX, e.FieldSym.Offset*SizeOf(Variant)+cVariant_DataOffset);
   x86._test_al_al;

   jit.Fixups.NewConditionalJumps(flagsNZ, targetTrue, targetFalse);
end;
}
// ------------------
// ------------------ Tx86ArrayBase ------------------
// ------------------

// CompileIndexToGPR
//
procedure Tx86ArrayBase.CompileIndexToGPR(indexExpr : TTypedExpr; gpr : TgpRegister64; var delta : Integer);
var
   tempPtrOffset : Integer;
   sign : Integer;
begin
   delta:=0;
   if indexExpr.ClassType=TConstIntExpr then begin

      x86._mov_reg_imm(gpr, TConstIntExpr(indexExpr).Value);

   end else if indexExpr.ClassType=TIntVarExpr then begin

      x86._mov_reg_execmem(gpr, TIntVarExpr(indexExpr).StackAddr);

   end else begin

      if indexExpr.ClassType=TAddIntExpr then
         sign:=1
      else if indexExpr.ClassType=TSubIntExpr then
         sign:=-1
      else sign:=0;
      if sign<>0 then begin
         if TIntegerBinOpExpr(indexExpr).Right is TConstIntExpr then begin
            CompileIndexToGPR(TIntegerBinOpExpr(indexExpr).Left, gpr, delta);
            delta:=delta+sign*TConstIntExpr(TIntegerBinOpExpr(indexExpr).Right).Value;
            Exit;
         end else if TIntegerBinOpExpr(indexExpr).Left is TConstIntExpr then begin
            CompileIndexToGPR(TIntegerBinOpExpr(indexExpr).Right, gpr, delta);
            delta:=delta+sign*TConstIntExpr(TIntegerBinOpExpr(indexExpr).Left).Value;
            Exit;
         end;
      end;

      tempPtrOffset := jit.FPreamble.AllocateStackSpace(SizeOf(Pointer));
      x86._mov_qword_ptr_reg_reg(gprRBP, tempPtrOffset, gprRAX);

      jit.CompileInteger(indexExpr);
      x86._mov_reg_reg(gpr, gprRAX);

      x86._mov_reg_qword_ptr_reg(gprRAX, gprRBP, tempPtrOffset);

   end;
end;
{
// ------------------
// ------------------ Tx86StaticArray ------------------
// ------------------

// DoCompileFloat
//
function Tx86StaticArray.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
var
   e : TStaticArrayExpr;
   index, delta : Integer;
begin
   e:=TStaticArrayExpr(expr);

   if jit.IsFloat(e) then begin

 (     if (e.BaseExpr is TByRefParamExpr) and (e.IndexExpr is TConstIntExpr) then begin

         index := TConstIntExpr(e.IndexExpr).Value-e.LowBound;

         Tx86VarParam.CompileAsPVariant(x86, TByRefParamExpr(e.BaseExpr));
         x86._mov_reg_dword(gprECX, index*SizeOf(Variant));
         Result:=jit.AllocXMMReg(e);
         x86._movsd_reg_qword_ptr_indexed(Result, gprRAX, gprRCX, 1, cVariant_DataOffset);

      end else *) if e.BaseExpr.ClassType = TVarExpr then begin

         if e.IndexExpr is TConstIntExpr then begin

            index := TConstIntExpr(e.IndexExpr).Value - e.LowBound;

            Result := jit.AllocXMMReg(e);
            x86._movsd_reg_execmem(Result, TVarExpr(e.BaseExpr).StackAddr+index);

         end else begin

            CompileIndexToGPR(e.IndexExpr, gprRCX, delta);
            // TODO  jit._RangeCheck(e, gprRCX, delta, e.LowBound, e.LowBound+e.Count);

            Result := jit.AllocXMMReg(e);
            x86._imul_reg_reg_imm(gprRCX, gprRCX, SizeOf(Variant));
            x86._movsd_reg_qword_ptr_indexed(Result, cExecMemGPR, gprRCX, StackAddrToOffset(TVarExpr(e.BaseExpr).StackAddr));

         end;

      end else Result:=inherited;

   end else Result:=inherited;
end;

// CompileInteger
//
function Tx86StaticArray.CompileInteger(expr : TTypedExpr) : Integer;
var
   e : TStaticArrayExpr;
   delta, offsetBase : Integer;
begin
   e:=TStaticArrayExpr(expr);

   if (e.BaseExpr is TConstExpr) then begin

      x86._mov_reg_imm(gprRAX, NativeUInt(@TConstExpr(e.BaseExpr).Data[0]));
      offsetBase := 0;

//   end else if (e.BaseExpr.ClassType=TVarExpr) then begin
//
//      x86._mov_reg_reg(gprEAX, cExecMemGPR);
//      offsetBase := TVarExpr(e.BaseExpr).StackAddr*SizeOf(Variant);

   end else if (e.BaseExpr is TFieldExpr) then  begin

      jit.CompileScriptObj(TFieldExpr(e.BaseExpr).ObjectExpr);
      // TODO object check
      x86._mov_reg_qword_ptr_reg(gprRAX, gprRAX, vmt_ScriptObjInstance_IScriptObj_To_FData);
      offsetBase := TFieldExpr(e.BaseExpr).FieldSym.Offset*SizeOf(Variant);

   end else Exit(inherited);

   if e.IndexExpr is TConstIntExpr then begin

      // assume range check done at compile time when index is constant on a static array
      Inc(offsetBase, TConstIntExpr(e.IndexExpr).Value*SizeOf(Variant));
      if offsetBase <> 0 then
         x86._add_reg_imm(gprRAX, offsetBase);

      x86._mov_reg_qword_ptr_reg(gprRAX, gprRAX, cVariant_DataOffset);

   end else begin

      if offsetBase <> 0 then
         x86._add_reg_imm(gprRAX, offsetBase);

      CompileIndexToGPR(e.IndexExpr, gprRCX, delta);
      jit._RangeCheck(e, gprRCX, delta, e.LowBound, e.LowBound+e.Count);

      x86._imul_reg_reg_imm(gprRCX, gprRCX, SizeOf(Variant));

      x86._mov_reg_dword_ptr_indexed(gprRAX, gprRAX, gprRCX, 1, cVariant_DataOffset);

   end;
   Result:=0;
end;

// DoCompileAssignFloat
//
procedure Tx86StaticArray.DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister);
var
   e : TStaticArrayExpr;
   index : Integer;
begin
   e:=TStaticArrayExpr(expr);

   if jit.IsFloat(e) then begin

      if (e.BaseExpr.ClassType=TVarParamExpr) and (e.IndexExpr is TConstIntExpr) then begin

         index:=TConstIntExpr(e.IndexExpr).Value;

         Tx86VarParam.CompileAsPVariant(x86, TVarParamExpr(e.BaseExpr));
         x86._mov_reg_dword(gprECX, index*SizeOf(Variant));
         x86._movsd_qword_ptr_indexed_reg(gprEAX, gprECX, 1, cVariant_DataOffset, source);

      end else if (e.BaseExpr.ClassType=TVarExpr) and (e.IndexExpr is TConstIntExpr) then begin

         index:=TConstIntExpr(e.IndexExpr).Value;

         x86._movsd_execmem_reg(TVarExpr(e.BaseExpr).StackAddr+index, source);

         x86._imul_reg_reg_imm(gprRCX, gprRCX, SizeOf(Variant));

      end else inherited;

   end else inherited;
end;

// CompileAssignInteger
//
procedure Tx86StaticArray.CompileAssignInteger(expr : TTypedExpr; source : Integer);
var
   e : TStaticArrayExpr;
   index, delta, offset : Integer;
begin
   e:=TStaticArrayExpr(expr);

   if jit.IsInteger(e) then begin

      if e.BaseExpr.ClassType=TVarExpr then begin

         if e.IndexExpr is TConstIntExpr then begin

            index := TConstIntExpr(e.IndexExpr).Value;
            // assume statically checked before
            x86._mov_execmem_eaxedx(TVarExpr(e.BaseExpr).StackAddr+index);

         end else begin

            CompileIndexToGPR(e.IndexExpr, gprECX, delta);
            jit._RangeCheck(e, gprECX, delta, e.LowBound, e.LowBound+e.Count);

            x86._imul_reg_reg_imm(gprRCX, gprRCX, SizeOf(Variant));

            x86._add_reg_reg(gprECX, cExecMemGPR);

            offset := TVarExpr(e.BaseExpr).StackAddr * SizeOf(Variant);

            x86._mov_dword_ptr_reg_reg(gprECX, offset+cVariant_DataOffset+4, gprEDX);
            x86._mov_dword_ptr_reg_reg(gprECX, offset+cVariant_DataOffset, gprEAX);

         end;

      end else inherited;

   end else inherited;
end;

// ------------------
// ------------------ Tx86DynamicArrayBase ------------------
// ------------------

// CompileAsData
//
procedure Tx86DynamicArrayBase.CompileAsData(expr : TTypedExpr);
begin
   jit.CompileScriptObj(expr);
   x86._mov_reg_dword_ptr_reg(gprEAX, gprEAX, vmt_ScriptDynamicArray_IScriptObj_To_FData);
end;

// ------------------
// ------------------ Tx86DynamicArray ------------------
// ------------------

// CompileAsItemPtr
//
procedure Tx86DynamicArray.CompileAsItemPtr(expr : TDynamicArrayExpr; var delta : Integer);
begin
   CompileAsData(expr.BaseExpr);

   CompileIndexToGPR(expr.IndexExpr, gprECX, delta);
   delta:=delta*SizeOf(Variant)*expr.Typ.Size+cVariant_DataOffset;

   x86._shift_reg_imm(gpShl, gprECX, 4);
   // TODO : range checking
end;

// DoCompileFloat
//
function Tx86DynamicArray.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
var
   e : TDynamicArrayExpr;
   delta : Integer;
begin
   e:=TDynamicArrayExpr(expr);

   if jit.IsFloat(e) then begin

      CompileAsItemPtr(e, delta);

      Result:=jit.AllocXMMReg(e);
      x86._movsd_reg_qword_ptr_indexed(Result, gprEAX, gprECX, 1, delta);

   end else Result:=inherited;
end;

// CompileInteger
//
function Tx86DynamicArray.CompileInteger(expr : TTypedExpr) : Integer;
var
   e : TDynamicArrayExpr;
   delta : Integer;
begin
   e:=TDynamicArrayExpr(expr);

   CompileAsItemPtr(e, delta);

   x86._mov_reg_dword_ptr_indexed(gprEDX, gprEAX, gprECX, 1, delta+4);
   x86._mov_reg_dword_ptr_indexed(gprEAX, gprEAX, gprECX, 1, delta);

   Result:=0;
end;

// CompileScriptObj
//
function Tx86DynamicArray.CompileScriptObj(expr : TTypedExpr) : Integer;
var
   e : TDynamicArrayExpr;
   delta : Integer;
begin
   e:=TDynamicArrayExpr(expr);

   CompileAsItemPtr(e, delta);

   Result:=Ord(gprEAX);
   x86._mov_reg_dword_ptr_indexed(gprEAX, gprEAX, gprECX, 1, delta);
end;

// ------------------
// ------------------ Tx86DynamicArraySet ------------------
// ------------------

// CompileStatement
//
procedure Tx86DynamicArraySet.CompileStatement(expr : TExprBase);
var
   e : TDynamicArraySetExpr;
   reg : TxmmRegister;
   delta : Integer;
begin
   e:=TDynamicArraySetExpr(expr);

   if jit.IsFloat(e.ArrayExpr.Typ.Typ) then begin

      reg:=jit.CompileFloat(e.ValueExpr);
      CompileAsData(e.ArrayExpr);

      CompileIndexToGPR(e.IndexExpr, gprECX, delta);
      // TODO : range checking

      delta:=delta*SizeOf(Variant)+cVariant_DataOffset;

      x86._shift_reg_imm(gpShl, gprECX, 4);

      x86._movsd_qword_ptr_indexed_reg(gprEAX, gprECX, 1, delta, reg);

   end else begin

      inherited;

   end;
end;
}
// ------------------
// ------------------ Tx86NegInt ------------------
// ------------------

// CompileInteger
//
function Tx86NegInt.CompileInteger(expr : TTypedExpr) : Integer;
begin
   Result:=jit.CompileInteger(TNegIntExpr(expr).Expr);

   x86._neg_reg(gprRAX);
end;

// ------------------
// ------------------ Tx86NotInt ------------------
// ------------------

// CompileInteger
//
function Tx86NotInt.CompileInteger(expr : TTypedExpr) : Integer;
begin
   Result:=jit.CompileInteger(TNotIntExpr(expr).Expr);

   x86._not_reg(gprRAX);
end;

// ------------------
// ------------------ Tx86IntegerBinOpExpr ------------------
// ------------------

// Create
//
constructor Tx86IntegerBinOpExpr.Create(jit : TdwsJITx86_64; const anOp : TgpOP;
                                        commutative : Boolean = True);
begin
   inherited Create(jit);
   FOp := anOp;
   FCommutative := commutative;
end;

// CompileInteger
//
function Tx86IntegerBinOpExpr.CompileInteger(expr : TTypedExpr) : Integer;
var
   e : TIntegerBinOpExpr;
   addr : Integer;
begin
   e:=TIntegerBinOpExpr(expr);

   if e.Right is TConstIntExpr then

      CompileConstantOperand(e.Left, TConstIntExpr(e.Right).Value)

   else if FCommutative and (e.Left is TConstIntExpr) then

      CompileConstantOperand(e.Right, TConstIntExpr(e.Left).Value)

   else if e.Right.ClassType = TIntVarExpr then

      CompileVarOperand(e.Left, TIntVarExpr(e.Right).StackAddr)

   else if FCommutative and (e.Left.ClassType = TIntVarExpr) then

      CompileVarOperand(e.Right, TIntVarExpr(e.Left).StackAddr)

   else begin

      jit.CompileInteger(e.Right);
      addr := jit._store_rax;

      jit.CompileInteger(e.Left);
      x86._op_reg_qword_ptr_reg(FOp, gprRAX, gprRBP, addr);

   end;
   Result:=0;
end;

// CompileConstantOperand
//
procedure Tx86IntegerBinOpExpr.CompileConstantOperand(expr : TTypedExpr; const val : Int64);
begin
   jit.CompileInteger(expr);
   x86._op_reg_imm(FOp, gprRAX, val)
end;

// CompileVarOperand
//
procedure Tx86IntegerBinOpExpr.CompileVarOperand(expr : TTypedExpr; const varStackAddr : Integer);
var
   addr : Integer;
begin
   addr := StackAddrToOffset(varStackAddr);

   jit.CompileInteger(expr);
   x86._op_reg_qword_ptr_reg(FOp, gprRAX, cExecMemGPR, addr);
end;

// ------------------
// ------------------ Tx86MultInt ------------------
// ------------------

// CompileInteger
//
function Tx86MultInt.CompileInteger(expr : TTypedExpr) : Integer;

   procedure CompileOperand(expr : TTypedExpr; var reg : TgpRegister64; var addr : Integer);
   begin
      if expr is TIntVarExpr then begin
         reg:=cExecMemGPR;
         addr:=StackAddrToOffset(TIntVarExpr(expr).StackAddr);
      end else begin
         reg:=gprRBP;
         jit.CompileInteger(expr);
         addr:=jit._store_rax;
      end;
   end;

   function Test(a, b : Int64) : Int64;
   begin
      Result := a * b;
   end;

var
   e : TMultIntExpr;
   leftReg, rightReg : TgpRegister64;
   leftAddr, rightAddr : Integer;
begin
   e:=TMultIntExpr(expr);

   Test(1, 2);

   CompileOperand(e.Left, leftReg, leftAddr);
   CompileOperand(e.Right, rightReg, rightAddr);

   x86._mov_reg_qword_ptr_reg(gprRAX, leftReg, leftAddr);
   x86._imul_reg_qword_ptr_reg(gprRAX, rightReg, rightAddr);

   Result:=0;
end;
{
// ------------------
// ------------------ Tx86MultIntPow2 ------------------
// ------------------

// CompileInteger
//
function Tx86MultIntPow2.CompileInteger(expr : TTypedExpr) : Integer;
var
   e : TMultIntPow2Expr;
begin
   e:=TMultIntPow2Expr(expr);

   Result:=jit.CompileInteger(e.Expr);
   x86._shl_eaxedx_imm(e.Shift+1);
end;

// ------------------
// ------------------ Tx86DivInt ------------------
// ------------------

// CompileInteger
//
function Tx86DivInt.CompileInteger(expr : TTypedExpr) : Integer;

   procedure DivideByPowerOfTwo(p : Integer);
   begin
      x86._mov_reg_reg(gprECX, gprEDX);
      x86._shift_reg_imm(gpShr, gprECX, 32-p);
      x86._add_reg_reg(gprEAX, gprECX);
      x86._adc_reg_int32(gprEDX, 0);
      x86._sar_eaxedx_imm(p);
   end;

var
   e : TDivExpr;
   d : Int64;
   i : Integer;
begin
   e:=TDivExpr(expr);

   Result:=jit.CompileInteger(e.Left);

   if e.Right is TConstIntExpr then begin

      d:=TConstIntExpr(e.Right).Value;
      if d=1 then begin
         Exit;
      end;

      if d>0 then begin
         // is it a power of two?
         i:=WhichPowerOfTwo(d);
         if (i>0) and (i<=31) then begin
            DivideByPowerOfTwo(i);
            Exit;
         end;
      end;

   end;

   x86._push_reg(gprEDX);
   x86._push_reg(gprEAX);

   jit.CompileInteger(e.Right);
   x86._push_reg(gprEDX);
   x86._push_reg(gprEAX);

   x86._call_absmem(@@vAddr_div);
end;

// ------------------
// ------------------ Tx86ModInt ------------------
// ------------------

// CompileInteger
//
function Tx86ModInt.CompileInteger(expr : TTypedExpr) : Integer;
var
   e : TModExpr;
   jumpPositive, jumpDone : TFixupJump;
   d : Int64;
begin
   e:=TModExpr(expr);

   if e.Right is TConstIntExpr then begin

      d:=TConstIntExpr(e.Right).Value;
      if (d>0) and (WhichPowerOfTwo(d)>=0) then begin

         Dec(d);

         Result:=jit.CompileInteger(e.Left);
         x86._test_reg_reg(gprEDX, gprEDX);
         jumpPositive:=jit.Fixups.NewJump(flagsNS);

         x86._neg_eaxedx;
         if (d shr 32)<>0 then
            x86._op_reg_int32(gpOp_and, gprEDX, d shr 32);
         x86._op_reg_int32(gpOp_and, gprEAX, d);
         x86._neg_eaxedx;

         jumpDone:=jit.Fixups.NewJump(flagsNone);
         jumpPositive.NewTarget(False);

         if (d shr 32)<>0 then
            x86._op_reg_int32(gpOp_and, gprEDX, d shr 32);
         x86._op_reg_int32(gpOp_and, gprEAX, d);

         jumpDone.NewTarget(False);

         Exit;

      end;

   end;

   Result:=jit.CompileInteger(e.Left);
   x86._push_reg(gprEDX);
   x86._push_reg(gprEAX);

   jit.CompileInteger(e.Right);
   x86._push_reg(gprEDX);
   x86._push_reg(gprEAX);

   x86._call_absmem(@@vAddr_mod);
end;
}
// ------------------
// ------------------ Tx86Shr ------------------
// ------------------

// CompileInteger
//
function Tx86Shr.CompileInteger(expr : TTypedExpr) : Integer;
var
   e : TShrExpr;
begin
   e:=TShrExpr(expr);

   Result:=jit.CompileInteger(e.Left);

   if e.Right is TConstIntExpr then begin

      x86._shift_reg_imm(gpShr, gprRAX, TConstIntExpr(e.Right).Value);

   end else Result:=inherited;
end;

// ------------------
// ------------------ Tx86Shl ------------------
// ------------------

// CompileInteger
//
function Tx86Shl.CompileInteger(expr : TTypedExpr) : Integer;
var
   e : TShlExpr;
begin
   e:=TShlExpr(expr);

   Result:=jit.CompileInteger(e.Left);

   if e.Right is TConstIntExpr then begin

      x86._shift_reg_imm(gpShr, gprRAX, TConstIntExpr(e.Right).Value);

   end else inherited;
end;

// ------------------
// ------------------ Tx86Inc ------------------
// ------------------

// DoCompileStatement
//
procedure Tx86Inc.DoCompileStatement(v : TIntVarExpr; i : TTypedExpr);
begin
   if i is TConstIntExpr then begin

      x86._add_execmem_imm(v.StackAddr, TConstIntExpr(i).Value);

   end else begin

      jit.CompileInteger(i);

      x86._add_execmem_reg(v.StackAddr, gprRAX);

   end;
end;

// ------------------
// ------------------ Tx86IncIntVar ------------------
// ------------------

// CompileStatement
//
procedure Tx86IncIntVar.CompileStatement(expr : TExprBase);
var
   e : TIncIntVarExpr;
begin
   e := TIncIntVarExpr(expr);
   DoCompileStatement(e.Left as TIntVarExpr, e.Right);
end;
{
// ------------------
// ------------------ Tx86IncVarFunc ------------------
// ------------------

// CompileStatement
//
procedure Tx86IncVarFunc.CompileStatement(expr : TExprBase);
var
   e : TIncVarFuncExpr;
begin
   e:=TIncVarFuncExpr(expr);

   if e.Args[0] is TIntVarExpr then
      DoCompileStatement(TIntVarExpr(e.Args[0]), e.Args[1] as TTypedExpr)
   else inherited;
end;

// ------------------
// ------------------ Tx86Dec ------------------
// ------------------

// DoCompileStatement
//
procedure Tx86Dec.DoCompileStatement(v : TIntVarExpr; i : TTypedExpr);
begin
   if i is TConstIntExpr then begin

      x86._execmem64_dec(v.StackAddr, TConstIntExpr(i).Value);

   end else begin

      jit.CompileInteger(i);

      x86._sub_execmem_reg(v.StackAddr, 0, gprEAX);
      x86._sbb_execmem_reg(v.StackAddr, 4, gprEDX);

   end;
end;

// ------------------
// ------------------ Tx86DecIntVar ------------------
// ------------------

// CompileStatement
//
procedure Tx86DecIntVar.CompileStatement(expr : TExprBase);
var
   e : TDecIntVarExpr;
begin
   e:=TDecIntVarExpr(expr);
   DoCompileStatement(e.Left as TIntVarExpr, e.Right);;
end;

// ------------------
// ------------------ Tx86DecVarFunc ------------------
// ------------------

// CompileStatement
//
procedure Tx86DecVarFunc.CompileStatement(expr : TExprBase);
var
   e : TDecVarFuncExpr;
begin
   e:=TDecVarFuncExpr(expr);

   if e.Args[0] is TIntVarExpr then
      DoCompileStatement(TIntVarExpr(e.Args[0]), e.Args[1] as TTypedExpr)
   else inherited;
end;
}
// ------------------
// ------------------ Tx86RelOpInt ------------------
// ------------------

// Create
//
constructor Tx86RelOpInt.Create(jit : TdwsJITx86_64; flagsPass : TboolFlags);
begin
   inherited Create(jit);
   Self.FlagsPass:=flagsPass;
end;

// DoCompileBoolean
//
procedure Tx86RelOpInt.DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
var
   e : TIntegerRelOpExpr;
   addr : Integer;
begin
   e:=TIntegerRelOpExpr(expr);

   if e.Right is TConstIntExpr then begin

      if e.Left is TIntVarExpr then begin

         addr:=TIntVarExpr(e.Left).StackAddr;

         x86._cmp_execmem_imm(addr, TConstIntExpr(e.Right).Value);
         jit.Fixups.NewConditionalJumps(FlagsPass, targetTrue, targetFalse);

      end else begin

         jit.CompileInteger(e.Left);

         x86._cmp_reg_imm(gprRAX, TConstIntExpr(e.Right).Value);
         jit.Fixups.NewConditionalJumps(FlagsPass, targetTrue, targetFalse);

      end;

   end else if e.Right is TIntVarExpr then begin

      jit.CompileInteger(e.Left);

      addr:=TIntVarExpr(e.Right).StackAddr;

      x86._cmp_reg_execmem(gprRAX, addr);
      jit.Fixups.NewConditionalJumps(FlagsPass, targetTrue, targetFalse);

   end else begin

      jit.CompileInteger(e.Right);

      addr:=jit._store_rax;

      jit.CompileInteger(e.Left);

      x86._op_reg_qword_ptr_reg(gpOp_cmp, gprRAX, gprRBP, addr);
      jit.Fixups.NewConditionalJumps(FlagsPass, targetTrue, targetFalse);

   end;
end;

// ------------------
// ------------------ Tx86RelEqualInt ------------------
// ------------------

// Create
//
constructor Tx86RelEqualInt.Create(jit : TdwsJITx86_64);
begin
   inherited Create(jit, flagsZ);
end;

// ------------------
// ------------------ Tx86RelNotEqualInt ------------------
// ------------------

// Create
//
constructor Tx86RelNotEqualInt.Create(jit : TdwsJITx86_64);
begin
   inherited Create(jit, flagsNZ);
end;

// ------------------
// ------------------ Tx86RelIntIsZero ------------------
// ------------------

// DoCompileBoolean
//
procedure Tx86RelIntIsZero.DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
var
   e : TUnaryOpBoolExpr;
   addr : Integer;
begin
   e:=TUnaryOpBoolExpr(expr);

   if e.Expr is TIntVarExpr then begin

      addr := TIntVarExpr(e.Expr).StackAddr;

      x86._cmp_execmem_imm(addr, 0);
      jit.Fixups.NewConditionalJumps(flagsZ, targetTrue, targetFalse);

   end else begin

      jit.CompileInteger(e.Expr);

      x86._cmp_reg_imm(gprRAX, 0);
      jit.Fixups.NewConditionalJumps(flagsZ, targetTrue, targetFalse);

   end;
end;

// ------------------
// ------------------ Tx86RelIntIsNotZero ------------------
// ------------------

// DoCompileBoolean
//
procedure Tx86RelIntIsNotZero.DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
begin
   inherited DoCompileBoolean(expr, targetFalse, targetTrue);
end;

// ------------------
// ------------------ Tx86RelOpFloat ------------------
// ------------------

// Create
//
constructor Tx86RelOpFloat.Create(jit : TdwsJITx86_64; flags : TboolFlags);
begin
   inherited Create(jit);
   Self.Flags:=flags;
end;

// DoCompileBoolean
//
procedure Tx86RelOpFloat.DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
var
   e : TRelGreaterFloatExpr;
   regLeft : TxmmRegister;
begin
   e:=TRelGreaterFloatExpr(expr);

   regLeft:=jit.CompileFloat(e.Left);

   jit._comisd_reg_expr(regLeft, e.Right);

   jit.Fixups.NewConditionalJumps(Flags, targetTrue, targetFalse);
end;
{
// ------------------
// ------------------ Tx86NotExpr ------------------
// ------------------

// DoCompileBoolean
//
procedure Tx86NotExpr.DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
var
   e : TNotBoolExpr;
begin
   e:=TNotBoolExpr(expr);

   jit.CompileBoolean(e.Expr, targetFalse, targetTrue);
end;
}
// ------------------
// ------------------ Tx86BoolOrExpr ------------------
// ------------------

// DoCompileBoolean
//
procedure Tx86BoolOrExpr.DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
var
   e : TBooleanBinOpExpr;
//   targetFirstFalse : TFixupTarget;
begin
   e := TBooleanBinOpExpr(expr);

//   targetFirstFalse:=jit.Fixups.NewHangingTarget(False);
//   jit.CompileBoolean(e.Left, targetTrue, targetFirstFalse);
//   jit.Fixups.AddFixup(targetFirstFalse);
   jit.CompileBoolean(e.Left, targetTrue, nil);
   jit.CompileBoolean(e.Right, targetTrue, targetFalse);
end;
{
// ------------------
// ------------------ Tx86BoolAndExpr ------------------
// ------------------

// JumpSafeCompile
//
procedure Tx86BoolAndExpr.DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
var
   e : TBooleanBinOpExpr;
   targetFirstTrue : TFixupTarget;
begin
   e:=TBooleanBinOpExpr(expr);

   targetFirstTrue:=jit.Fixups.NewHangingTarget(False);
   jit.CompileBoolean(e.Left, targetFirstTrue, targetFalse);
   jit.Fixups.AddFixup(targetFirstTrue);
   jit.CompileBoolean(e.Right, targetTrue, targetFalse);
end;

// ------------------
// ------------------ Tx86SetOfExpr ------------------
// ------------------

// NormalizeEnumOperand
//
procedure Tx86SetOfExpr.NormalizeEnumOperand(setTyp : TSetOfSymbol; operand : TTypedExpr;
                                             targetOutOfRange : TFixup);
begin
   jit.CompileInteger(operand);

   if setTyp.MinValue<>0 then
      x86._sub_reg_int32(gprEAX, setTyp.MinValue);

   x86._cmp_reg_int32(gprEAX, setTyp.CountValue);
   jit.Fixups.NewJump(flagsGE, targetOutOfRange);
end;

// ComputeFromValueInEAXOffsetInECXMaskInEDX
//
procedure Tx86SetOfExpr.ComputeFromValueInEAXOffsetInECXMaskInEDX(setTyp : TSetOfSymbol);
begin
   // this is a bit complicated because of the array of Variant layout
   // bits are in 64bit packages with 64bit padding and the test is 32bits

   x86._mov_reg_reg(gprECX, gprEAX);            // ECX: value
   x86._op_reg_int32(gpOp_and, gprECX, 31);     // ECX: value and 31
   x86._mov_reg_dword(gprEDX, 1);
   x86._shift_reg_cl(gpShl, gprEDX);            // EDX: 32bit mask

   x86._shift_reg_imm(gpShr, gprEAX, 5);        // EAX: 32bit index
   x86._mov_reg_reg(gprECX, gprEAX);            // ECX: 32bit index

   x86._op_reg_int32(gpOp_and, gprEAX, 1);      // EAX; 32bit index in 64bit package
   x86._shift_reg_imm(gpShl, gprEAX, 2);        // EAX; offset in 64bit package

   x86._shift_reg_imm(gpShr, gprECX, 1);        // ECX: 64bit index
   x86._shift_reg_imm(gpShl, gprECX, 4);        // ECX: SizeOf(Variant) offset

   x86._add_reg_reg(gprECX, gprEAX);            // ECX: offset
end;

// ------------------
// ------------------ Tx86SetOfInExpr ------------------
// ------------------

// DoCompileBoolean
//
procedure Tx86SetOfInExpr.DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
var
   e : TSetOfInExpr;
   rightVar : TVarExpr;
   rightVarOffset : Integer;
   v : Integer;
   offset : Integer;
   byteMask : Byte;
   opAddrRegister : TgpRegister64;
begin
   e:=TSetOfInExpr(expr);

   if e.Right.ClassType = TBaseTypeVarExpr then
      rightVar := TVarExpr(e.Right)
   else begin
      inherited;
      Exit;
   end;
   rightVarOffset:=StackAddrToOffset(rightVar.StackAddr);

   if e.Left is TConstIntExpr then begin

      v:=TConstIntExpr(e.Left).Value-e.SetType.MinValue;
      if v>=e.SetType.CountValue then begin
         jit.Fixups.NewJump(targetFalse);
         Exit;
      end;

      offset:=e.SetType.ValueToByteOffsetMask(v, byteMask);
      offset:=(offset and 7)+(offset shr 8)*SizeOf(Variant);

      x86._test_dword_ptr_reg_byte(cExecMemGPR, rightVarOffset+offset, byteMask);
      jit.Fixups.NewConditionalJumps(flagsNZ, targetTrue, targetFalse);
      Exit;

   end else begin

      NormalizeEnumOperand(e.SetType, e.Left, targetFalse);

      if e.SetType.CountValue<=32 then begin

         x86._mov_reg_reg(gprECX, gprEAX);
         x86._mov_reg_dword(gprEDX, 1);
         x86._shift_reg_cl(gpShl, gprEDX);

         opAddrRegister:=cExecMemGPR;

      end else begin

         ComputeFromValueinEAXOffsetInECXMaskInEDX(e.SetType);
         x86._add_reg_reg(gprECX, cExecMemGPR);

         opAddrRegister:=gprECX;

      end;

      x86._test_dword_ptr_reg_reg(opAddrRegister, rightVarOffset, gprEDX);

   end;

   jit.Fixups.NewConditionalJumps(flagsNZ, targetTrue, targetFalse);
end;

// ------------------
// ------------------ Tx86SetOfFunction ------------------
// ------------------

// CompileStatement
//
procedure Tx86SetOfFunction.CompileStatement(expr : TExprBase);
var
   e : TSetOfFunctionExpr;
   offset : Integer;
   varOffset : Integer;
   mask : Byte;
   targetOutOfRange : TFixupTarget;
begin
   e:=TSetOfFunctionExpr(expr);

   if e.BaseExpr.ClassType = TBaseTypeVarExpr then begin

      varOffset:=StackAddrToOffset(TVarExpr(e.BaseExpr).StackAddr);

      if e.Operand is TConstIntExpr then begin

         offset:=e.SetType.ValueToByteOffsetMask(TConstIntExpr(e.Operand).Value, mask);
         varOffset:=varOffset+(offset div 8)*SizeOf(Variant)+(offset and 7);

         DoByteOp(cExecMemGPR, varOffset, mask);

      end else begin

         targetOutOfRange:=jit.Fixups.NewHangingTarget(False);

         NormalizeEnumOperand(e.SetType, e.Operand, targetOutOfRange);

         ComputeFromValueInEAXOffsetInECXMaskInEDX(e.SetType);
         x86._add_reg_reg(gprECX, cExecMemGPR);

         x86._mov_reg_dword_ptr_reg(gprEAX, gprECX, varOffset);
         DoWordOp(gprEAX, gprEDX);
         x86._mov_dword_ptr_reg_reg(gprECX, varOffset, gprEAX);

         jit.Fixups.AddFixup(targetOutOfRange);

      end;

   end else inherited;
end;

// ------------------
// ------------------ Tx86SetOfInclude ------------------
// ------------------

// DoByteOp
//
procedure Tx86SetOfInclude.DoByteOp(reg : TgpRegister64; offset : Integer; mask : Byte);
begin
   x86._or_dword_ptr_reg_byte(cExecMemGPR, offset, mask);
end;

// DoWordOp
//
procedure Tx86SetOfInclude.DoWordOp(dest, src : TgpRegister64);
begin
   x86._op_reg_reg(gpOp_or, dest, src);
end;

// ------------------
// ------------------ Tx86SetOfExclude ------------------
// ------------------

// DoByteOp
//
procedure Tx86SetOfExclude.DoByteOp(reg : TgpRegister64; offset : Integer; mask : Byte);
begin
   x86._and_dword_ptr_reg_byte(cExecMemGPR, offset, not mask);
end;

// DoWordOp
//
procedure Tx86SetOfExclude.DoWordOp(dest, src : TgpRegister64);
begin
   x86._not_reg(src);
   x86._op_reg_reg(gpOp_and, dest, src);
end;

// ------------------
// ------------------ Tx86OrdBool ------------------
// ------------------

// CompileInteger
//
function Tx86OrdBool.CompileInteger(expr : TTypedExpr) : Integer;
var
   e : TOrdBoolExpr;
begin
   e:=TOrdBoolExpr(expr);

   Result:=jit.CompileBooleanValue(e.Expr);
   x86._mov_reg_dword(gprEDX, 0);
end;

// ------------------
// ------------------ Tx86OrdInt ------------------
// ------------------

// CompileInteger
//
function Tx86OrdInt.CompileInteger(expr : TTypedExpr) : Integer;
var
   e : TOrdIntExpr;
begin
   e:=TOrdIntExpr(expr);

   jit.CompileInteger(e.Expr);

   Result:=0;
end;
}
// ------------------
// ------------------ Tx86ConvIntToFloat ------------------
// ------------------

// DoCompileFloat
//
function Tx86ConvIntToFloat.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
begin
   Result:=jit.CompileFloat(TConvIntToFloatExpr(expr).Expr);
end;

// ------------------
// ------------------ Tx86MagicFunc ------------------
// ------------------

// DoCompileFloat
//
function Tx86MagicFunc.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
var
   jitter : TdwsJITter_x86;
   e : TMagicFuncExpr;
begin
   e:=(expr as TMagicFuncExpr);

   jitter:=TdwsJITter_x86(jit.FindJITter(TMagicFuncSymbol(e.FuncSym).InternalFunction.ClassType));
   if jitter<>nil then

      Result:=jitter.DoCompileFloat(expr)

   else Result:=inherited;
end;

// CompileInteger
//
function Tx86MagicFunc.CompileInteger(expr : TTypedExpr) : Integer;
var
   jitter : TdwsJITter_x86;
   e : TMagicIntFuncExpr;
begin
   e:=(expr as TMagicIntFuncExpr);

   jitter:=TdwsJITter_x86(jit.FindJITter(TMagicFuncSymbol(e.FuncSym).InternalFunction.ClassType));
   if jitter<>nil then

      Result:=jitter.CompileInteger(expr)

   else Result:=inherited;
end;

// DoCompileBoolean
//
procedure Tx86MagicFunc.DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
var
   jitter : TdwsJITter_x86;
   e : TMagicFuncExpr;
begin
   e:=(expr as TMagicFuncExpr);

   jitter:=TdwsJITter_x86(jit.FindJITter(TMagicFuncSymbol(e.FuncSym).InternalFunction.ClassType));
   if jitter<>nil then

      jitter.DoCompileBoolean(expr, targetTrue, targetFalse)

   else inherited;
end;

// CompileBooleanValue
//
function Tx86MagicFunc.CompileBooleanValue(expr : TTypedExpr) : Integer;
var
   jitter : TdwsJITter_x86;
   e : TMagicFuncExpr;
begin
   if ClassType<>Tx86MagicFunc then
      Exit(inherited);

   e:=(expr as TMagicFuncExpr);

   jitter:=TdwsJITter_x86(jit.FindJITter(TMagicFuncSymbol(e.FuncSym).InternalFunction.ClassType));
   if jitter<>nil then

      Result:=jitter.CompileBooleanValue(expr)

   else Result:=inherited;
end;

// ------------------
// ------------------ Tx86MagicBoolFunc ------------------
// ------------------

// CompileInteger
//
function Tx86MagicBoolFunc.CompileInteger(expr : TTypedExpr) : Integer;
begin
   Result := CompileBooleanValue(expr);
end;
{
// ------------------
// ------------------ Tx86DirectCallFunc ------------------
// ------------------

// Create
//
constructor Tx86DirectCallFunc.Create(jit : TdwsJITx86_64; addrPtr : PPointer);
begin
   inherited Create(jit);
   Self.AddrPtr:=addrPtr;
end;

// CompileCall
//
function Tx86DirectCallFunc.CompileCall(funcSym : TFuncSymbol; const args : TExprBaseListRec) : Boolean;
var
   i, stackOffset : Integer;
   p : TParamSymbol;
   paramReg : array of TxmmRegister;
   paramOffset : array of Integer;
begin
   Result:=False;

   SetLength(paramReg, funcSym.Params.Count);
   SetLength(paramOffset, funcSym.Params.Count);

   stackOffset:=0;
   for i:=0 to funcSym.Params.Count-1 do begin
      p:=funcSym.Params[i];
      if jit.IsFloat(p.Typ) then begin

         paramReg[i]:=jit.CompileFloat(args[i] as TTypedExpr);
         Inc(stackOffset, SizeOf(Double));
         paramOffset[i]:=stackOffset;

      end else Exit;
   end;

   x86._sub_reg_int32(gprESP, stackOffset);

   for i:=0 to High(paramReg) do begin
      if paramReg[i]<>xmmNone then
         x86._movsd_esp_reg(stackOffset-paramOffset[i], paramReg[i]);
   end;

   x86._call_absmem(addrPtr);

   Result:=True;
end;

// DoCompileFloat
//
function Tx86DirectCallFunc.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
var
   e : TMagicFuncExpr;
begin
   e:=TMagicFuncExpr(expr);

   if not CompileCall(e.FuncSym, e.Args) then begin

      jit.OutputFailedOn:=expr;
      Result:=xmm0;

   end else if jit.IsFloat(e.FuncSym.Typ) then begin

      jit.FPreamble.NeedTempSpace(SizeOf(Double));
      x86._fstp_esp;
      Result:=jit.AllocXMMReg(expr);
      x86._movsd_reg_esp(Result);

   end else begin

      jit.OutputFailedOn:=expr;
      Result:=xmm0;

   end;
end;

// CompileInteger
//
function Tx86DirectCallFunc.CompileInteger(expr : TTypedExpr) : Integer;
var
   e : TMagicFuncExpr;
begin
   e:=TMagicFuncExpr(expr);

   if not CompileCall(e.FuncSym, e.Args) then
      jit.OutputFailedOn:=expr;

   Result:=0;
end;

// DoCompileBoolean
//
procedure Tx86DirectCallFunc.DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
var
   e : TMagicFuncExpr;
begin
   e:=TMagicFuncExpr(expr);

   if not CompileCall(e.FuncSym, e.Args) then
      jit.OutputFailedOn:=expr;

   x86._test_al_al;
   jit.Fixups.NewConditionalJumps(flagsNZ, targetTrue, targetFalse);
end;

// CompileBooleanValue
//
function Tx86DirectCallFunc.CompileBooleanValue(expr : TTypedExpr) : Integer;
var
   e : TMagicFuncExpr;
begin
   e:=TMagicFuncExpr(expr);

   if not CompileCall(e.FuncSym, e.Args) then
      jit.OutputFailedOn:=expr;

   x86._op_reg_int32(gpOp_and, gprEAX, 1);
   Result:=0;
end;

// ------------------
// ------------------ Tx86AbsIntFunc ------------------
// ------------------

// CompileInteger
//
function Tx86AbsIntFunc.CompileInteger(expr : TTypedExpr) : Integer;
var
   jump : TFixupJump;
begin
   Result := jit.CompileInteger(TMagicFuncExpr(expr).Args[0] as TTypedExpr);

   x86._test_reg_reg(gprEDX, gprEDX);

   jump:=jit.Fixups.NewJump(flagsNL);

   x86._neg_reg(gprEDX);
   x86._neg_reg(gprEAX);
   x86._sbb_reg_int32(gprEDX, 0);

   jump.NewTarget(False);
end;

// ------------------
// ------------------ Tx86AbsFloatFunc ------------------
// ------------------

// DoCompileFloat
//
function Tx86AbsFloatFunc.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
begin
   Result:=jit.CompileFloat(TMagicFuncExpr(expr).Args[0] as TTypedExpr);

   // andpd Result, dqword ptr [AbsMask]
   x86.WriteBytes([$66, $0F, $54, $05+Ord(Result)*8]);
   x86.WritePointer(jit.AbsMaskPD);

   jit.ContainsXMMReg(Result, expr);
end;

// ------------------
// ------------------ Tx86SqrtFunc ------------------
// ------------------

// DoCompileFloat
//
function Tx86SqrtFunc.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
begin
   Result:=jit.AllocXMMReg(expr);

   jit._xmm_reg_expr(xmm_sqrtsd, Result, TMagicFuncExpr(expr).Args[0] as TTypedExpr);
end;
}
// ------------------
// ------------------ Tx86SqrFloatFunc ------------------
// ------------------

// DoCompileFloat
//
function Tx86SqrFloatFunc.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
begin
   Result:=CompileFloatOperand(expr, TMagicFuncExpr(expr).Args[0] as TTypedExpr);
end;
{
// ------------------
// ------------------ Tx86MinMaxFloatFunc ------------------
// ------------------

// Create
//
constructor Tx86MinMaxFloatFunc.Create(jit : TdwsJITx86_64; op : TxmmOp);
begin
   inherited Create(jit);
   Self.OP:=op;
end;

// DoCompileFloat
//
function Tx86MinMaxFloatFunc.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
var
   e : TMagicFuncExpr;
begin
   e:=TMagicFuncExpr(expr);

   Result:=jit.CompileFloat(e.Args[0] as TTypedExpr);

   jit._xmm_reg_expr(OP, Result, e.Args[1] as TTypedExpr);

   jit.ContainsXMMReg(Result, expr);
end;

// ------------------
// ------------------ Tx86RoundFunc ------------------
// ------------------

// CompileInteger
//
function Tx86RoundFunc.CompileInteger(expr : TTypedExpr) : Integer;
var
   reg : TxmmRegister;
begin
   reg:=jit.CompileFloat(TMagicFuncExpr(expr).Args[0] as TTypedExpr);

   jit.FPreamble.NeedTempSpace(SizeOf(Double));

   x86._movsd_esp_reg(reg);

   jit.ReleaseXMMReg(reg);

   x86._fld_esp;
   x86._fistp_esp;

   x86._mov_eaxedx_qword_ptr_reg(gprESP, 0);

   Result:=0;
end;

// DoCompileFloat
//
function Tx86RoundFunc.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
var
   reg : TxmmRegister;
begin
   reg:=jit.CompileFloat(TMagicFuncExpr(expr).Args[0] as TTypedExpr);

   jit.FPreamble.NeedTempSpace(SizeOf(Double));

   x86._movsd_esp_reg(reg);

   jit.ReleaseXMMReg(reg);

   x86._fld_esp;
   x86._fistp_esp;
   x86._fild_esp;
   x86._fstp_esp;

   Result := jit.AllocXMMReg(expr);

   x86._movsd_reg_esp(Result);
end;
}
// ------------------
// ------------------ Tx86OddFunc ------------------
// ------------------

// DoCompileBoolean
//
procedure Tx86OddFunc.DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
begin
   jit.CompileInteger(TMagicFuncExpr(expr).Args[0] as TTypedExpr);

   x86._test_reg_imm(gprRAX, 1);

   jit.Fixups.NewConditionalJumps(flagsNZ, targetTrue, targetFalse);
end;

// CompileBooleanValue
//
function Tx86OddFunc.CompileBooleanValue(expr : TTypedExpr) : Integer;
begin
   jit.CompileInteger(TMagicFuncExpr(expr).Args[0] as TTypedExpr);

   x86._test_reg_imm(gprRAX, 1);
   x86._set_al_flags(flagsNZ);
   x86._op_reg_imm(gpOp_and, gprRAX, 1);

   Result := 0;
end;


{$else}
implementation
{$endif}

// ------------------
// ------------------ Tx86Unsigned32Func ------------------
// ------------------

// CompileInteger
//
function Tx86Unsigned32Func.CompileInteger(expr : TTypedExpr) : Integer;
begin
   jit.CompileInteger(TMagicFuncExpr(expr).Args[0] as TTypedExpr);

   x86.WriteBytes([ $89, $C0 ]); // mov eax, eax

   Result := 0;
end;

end.
