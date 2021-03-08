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
   dwsCoreExprs, dwsRelExprs, dwsMagicExprs, dwsConstExprs, dwsArrayExprs,
   dwsMathFunctions, dwsDataContext, dwsConvExprs, dwsSetOfExprs, dwsMethodExprs,
   dwsArrayIndexOfExprs,
   dwsJIT, dwsJITFixups, dwsJITAllocatorWin, dwsJITx86Intrinsics, dwsVMTOffsets,
   dwsWin64FunctionTables, dwsJITRegisterAllocator;

type

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
         FAlteredGP : Cardinal;

         procedure Prepare;

      public
         constructor Create;
         destructor Destroy; override;

         function Optimize : TFixupOptimizeAction; override;
         function  GetSize : Integer; override;
         procedure Write(output : TWriteOnlyBlockStream); override;

         function AllocateStackSpace(bytes : Integer) : Integer;
         procedure NotifyXMMAlteration(xmm : TxmmRegister);
         procedure NotifyGPAlteration(gp : TgpRegister64);

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

   TRegisterFlushOption = (regFlushAll, regFlushVolatile, regFlushIntermediateExprs, regFlushDataSymbols);

   Tx86_64FixupLogicHelper = class helper for TFixupLogic
      function NewJump(flags : TboolFlags) : TFixupJump; overload;
      function NewJump(flags : TboolFlags; target : TFixup) : TFixupJump; overload;
      function NewJump(target : TFixup) : TFixupJump; overload;
      procedure NewConditionalJumps(flagsTrue : TboolFlags; targetTrue, targetFalse : TFixup);

      function NewMovsdRegImm(reg : TxmmRegister; const value : Double) : TStaticDataFixup;
      function NewComisdRegImm(reg : TxmmRegister; const value : Double) : TStaticDataFixup;
      function NewCmpRegImm(reg : TgpRegister64; const value : Int64) : TStaticDataFixup;
      function NewGPOpRegImm(const op : TgpOp; reg : TgpRegister64; const value : Int64) : TStaticDataFixup;
      function NewNegRegImm(reg : TxmmRegister) : TStaticDataFixup;
      function NewXMMOpRegImm(op : TxmmOp; reg : TxmmRegister; const value : Double) : TStaticDataFixup;
      function NewXMMOpRegPDImm(op : TxmmOp_pd; reg : TxmmRegister; const value1, value2 : Double) : TStaticDataFixup;

      function AddStaticData(const data : UInt64) : Integer;
      function AddStaticData128(const data1, data2 : UInt64) : Integer;
      function StaticData : TBytes;

      function GetStaticDataBase : Integer;
      procedure SetStaticDataBase(v : Integer);
      property StaticDataBase : Integer read GetStaticDataBase write SetStaticDataBase;
   end;

   TdwsJITx86_64 = class (TdwsJIT)
      private
         FPreamble : TFixupPreamble;
         FPostamble : TFixupPostamble;

         FXMMRegs : array [TxmmRegister] of TRegisterStatus;
         FXMMRegsStackAddr : array [TxmmRegister] of Integer;
         FXMMSaved : TxmmRegisters;

         FGPRegs : array [TgpRegister64] of TRegisterStatus;
         FGPRegsStackAddr : array [TgpRegister64] of Integer;
         FGPRSaved : TgpRegister64s;

         x86 : Tx86_64_WriteOnlyStream;

         FAllocator : TdwsJITAllocatorWin;

         FInterpretedJITter : TdwsJITter;

      protected
         function CreateOutput : Tx86BaseWriteOnlyStream; override;
         function CreateFixupLogic : TFixupLogic; override;

         function  CanonicXMMDataSymbol(expr : TExprBase) : TDataSymbol;
         function  CanonicGPRDataSymbol(expr : TExprBase) : TDataSymbol;

         procedure ResetRegisters;
         procedure ResetRegStackAddr;

         procedure StartJIT(expr : TExprBase; exitable : Boolean); override;
         procedure EndJIT; override;
         procedure EndFloatJIT(resultHandle : Integer); override;
         procedure EndIntegerJIT(resultHandle : Integer); override;

      public
         constructor Create; override;
         destructor Destroy; override;

         procedure FlushRegisters(option : TRegisterFlushOption);

         procedure SaveRegisters;
         procedure RestoreRegisters;

         function  AllocXMMReg(expr : TExprBase) : TxmmRegister;
         procedure ReleaseXMMReg(reg : TxmmRegister);
         function  CurrentXMMReg(expr : TExprBase) : TxmmRegister;
         function  IsSymbolXMMReg(reg : TxmmRegister) : Boolean;
         procedure SetContainsXMMReg(reg : TxmmRegister; expr : TExprBase);
         procedure FlushXMMSymbol(dataSymbol : TDataSymbol);

         function  AllocGPReg(expr : TExprBase) : TgpRegister64;
         procedure ReleaseGPReg(reg : TgpRegister64);
         function  CurrentGPReg(expr : TExprBase; var reg : TgpRegister64) : Boolean;
         function  IsSymbolGPReg(reg : TgpRegister64) : Boolean;
         procedure SetContainsGPReg(reg : TgpRegister64; expr : TExprBase);
         procedure FlushGPReg(reg : TgpRegister64);
         procedure FlushGPRSymbol(dataSymbol : TDataSymbol);

         function StackAddrOfFloat(expr : TTypedExpr) : Integer;

         property Allocator : TdwsJITAllocatorWin read FAllocator write FAllocator;

         function CompileFloat(expr : TTypedExpr) : TxmmRegister; inline;
         procedure CompileAssignFloat(expr : TTypedExpr; source : TxmmRegister); inline;

         function CompileInteger(expr : TTypedExpr) : Integer;
         function CompileIntegerToRegister(expr : TTypedExpr) : TgpRegister64;
         procedure CompileAssignInteger(expr : TTypedExpr; source : TgpRegister64); inline;
         procedure CompileAssignExprToInteger(dest, source : TTypedExpr);

         function CompileScriptObj(expr : TTypedExpr) : TgpRegister64;
         function CompileScriptDynArray(expr : TTypedExpr) : TgpRegister64;

         procedure CompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);

         function CompiledOutput : TdwsJITCodeBlock; override;

         procedure _xmm_reg_expr(op : TxmmOp; dest : TxmmRegister; expr : TTypedExpr);
         procedure _comisd_reg_expr(dest : TxmmRegister; expr : TTypedExpr);
         procedure _cmp_reg_imm(reg : TgpRegister64; value : Int64);
         procedure _op_reg_imm(const op : TgpOp; reg : TgpRegister64; value : Int64);
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

         function CompileScriptObj(expr : TTypedExpr) : Integer; override; final;
         function DoCompileScriptObj(expr : TTypedExpr) : TgpRegister64; virtual;

         function CompileInteger(expr : TTypedExpr) : Integer; override; final;
         function DoCompileInteger(expr : TTypedExpr) : TgpRegister64; virtual;

         procedure CompileAssignInteger(expr : TTypedExpr; source : Integer); override; final;
         procedure DoCompileAssignInteger(expr : TTypedExpr; source : TgpRegister64); virtual;

         function  CompileBooleanValue(expr : TTypedExpr) : Integer; override;
         procedure CompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override; final;
         procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); virtual;
   end;

   Tx86ConstFloat = class (TdwsJITter_x86)
      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
   end;
   Tx86ConstInt = class (TdwsJITter_x86)
      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
      function DoCompileInteger(expr : TTypedExpr) : TgpRegister64; override;
   end;
   Tx86ConstBoolean = class (TdwsJITter_x86)
      function  DoCompileInteger(expr : TTypedExpr) : TgpRegister64; override;
      function  CompileBooleanValue(expr : TTypedExpr) : Integer; override;
      procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
   end;

   Tx86InterpretedExpr = class (TdwsJITter_x86)
      procedure DoCallEval(expr : TExprBase; vmt : Integer);

      procedure CompileStatement(expr : TExprBase); override;

      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
      procedure DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister); override;

      function  DoCompileInteger(expr : TTypedExpr) : TgpRegister64; override;
      procedure DoCompileAssignInteger(expr : TTypedExpr; source : TgpRegister64); override;

      procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
      function CompileBooleanValue(expr : TTypedExpr) : Integer; override;

      procedure CompileAssignBoolean(expr : TTypedExpr; source : Integer); override;
   end;

   Tx86FloatVar = class (TdwsJITter_x86)
      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
      procedure DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister); override;
   end;
   Tx86IntVar = class (TdwsJITter_x86)
      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
      function  DoCompileInteger(expr : TTypedExpr) : TgpRegister64; override;
      procedure DoCompileAssignInteger(expr : TTypedExpr; source : TgpRegister64); override;
   end;
   Tx86BoolVar = class (TdwsJITter_x86)
      procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
      procedure CompileAssignBoolean(expr : TTypedExpr; source : Integer); override;
   end;
   Tx86ObjectVar = class (TdwsJITter_x86)
      function DoCompileScriptObj(expr : TTypedExpr) : TgpRegister64; override;
   end;
//   Tx86RecordVar = class (TdwsJITter_x86)
//      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
//      procedure DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister); override;
//      function CompileInteger(expr : TTypedExpr) : Integer; override;
//      procedure CompileAssignInteger(expr : TTypedExpr; source : Integer); override;
//      procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
//   end;
//   Tx86VarParam = class (Tx86InterpretedExpr)
//      class procedure CompileAsPVariant(x86 : Tx86_64_WriteOnlyStream; expr : TByRefParamExpr);
//      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
//      function CompileInteger(expr : TTypedExpr) : Integer; override;
//      procedure DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister); override;
//      procedure CompileAssignInteger(expr : TTypedExpr; source : Integer); override;
//   end;

//   Tx86FieldVar = class (Tx86InterpretedExpr)
//      procedure CompileToData(expr : TFieldVarExpr; dest : TgpRegister64);
//      function CompileInteger(expr : TTypedExpr) : Integer; override;
//      procedure CompileAssignInteger(expr : TTypedExpr; source : Integer); override;
//      procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
//   end;

   Tx86ArrayBase = class (Tx86InterpretedExpr)
   end;
   Tx86StaticArray = class (Tx86ArrayBase)
      function CompileToItemPtr(expr : TStaticArrayExpr; var ptrReg : TgpRegister64; var offset : Integer) : Boolean;

      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
      function  DoCompileInteger(expr : TTypedExpr) : TgpRegister64; override;
      procedure DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister); override;
//      procedure CompileAssignInteger(expr : TTypedExpr; source : Integer); override;
   end;

   Tx86DynamicArrayBase = class (Tx86ArrayBase)
      function CompileAsData(expr : TTypedExpr; elementType : TVarType) : TgpRegister64;
      function CompileAsItemPtr(base, index : TTypedExpr; var offset : Integer; elementType : TVarType) : TgpRegister64;
   end;
   Tx86DynamicArray = class (Tx86DynamicArrayBase)
      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
      function DoCompileInteger(expr : TTypedExpr) : TgpRegister64; override;
      function DoCompileScriptObj(expr : TTypedExpr) : TgpRegister64; override;
   end;
   Tx86DynamicArraySet = class (Tx86DynamicArrayBase)
      procedure CompileStatement(expr : TExprBase); override;
   end;

   Tx86AssignConstToFloatVar = class (TdwsJITter_x86)
      procedure CompileStatement(expr : TExprBase); override;
   end;
   Tx86AssignConstToIntegerVar = class (Tx86InterpretedExpr)
      procedure CompileStatement(expr : TExprBase); override;
   end;
   Tx86AssignConstToBoolVar = class (Tx86InterpretedExpr)
      procedure CompileStatement(expr : TExprBase); override;
   end;

   Tx86Assign = class (Tx86InterpretedExpr)
      procedure CompileStatement(expr : TExprBase); override;
   end;
//   Tx86AssignData = class (Tx86InterpretedExpr)
//      procedure CompileStatement(expr : TExprBase); override;
//   end;

   Tx86OpAssignFloat = class (TdwsJITter_x86)
      public
         OP : TxmmOp;
         constructor Create(jit : TdwsJITx86_64; op : TxmmOp);
         procedure CompileStatement(expr : TExprBase); override;
   end;

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
   Tx86While = class (Tx86Loop)
      procedure CompileStatement(expr : TExprBase); override;
   end;

   Tx86ForUpward = class (TdwsJITter_x86)
      procedure CompileStatement(expr : TExprBase); override;
   end;

   Tx86Continue = class (TdwsJITter_x86)
      procedure CompileStatement(expr : TExprBase); override;
   end;
   Tx86Break = class (TdwsJITter_x86)
      procedure CompileStatement(expr : TExprBase); override;
   end;
   Tx86Exit = class (TdwsJITter_x86)
      procedure CompileStatement(expr : TExprBase); override;
   end;
   Tx86ExitValue = class (Tx86Exit)
      procedure CompileStatement(expr : TExprBase); override;
   end;

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
      function DoCompileInteger(expr : TTypedExpr) : TgpRegister64; override;
   end;
   Tx86NotInt = class (TdwsJITter_x86)
      function DoCompileInteger(expr : TTypedExpr) : TgpRegister64; override;
   end;

   Tx86MultInt = class (TdwsJITter_x86)
      function DoCompileInteger(expr : TTypedExpr) : TgpRegister64; override;
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
      function  DoCompileInteger(expr : TTypedExpr) : TgpRegister64; override;
   end;

   Tx86Shift = class (Tx86InterpretedExpr)
      FShiftOp : TgpShift;
      constructor Create(jit : TdwsJITx86_64; const shiftOp : TgpShift);
      function DoCompileInteger(expr : TTypedExpr) : TgpRegister64; override;
   end;

   Tx86Inc = class (Tx86InterpretedExpr)
      procedure DoCompileStatement(v : TIntVarExpr; i : TTypedExpr);
   end;
   Tx86IncIntVar = class (Tx86Inc)
      procedure CompileStatement(expr : TExprBase); override;
   end;
   Tx86IncVarFunc = class (Tx86Inc)
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

   Tx86NotExpr = class (TdwsJITter_x86)
      procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override; final;
   end;
   Tx86BoolOrExpr = class (TdwsJITter_x86)
      procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
   end;
   Tx86BoolAndExpr = class (TdwsJITter_x86)
      procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
   end;
{
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
      function DoCompileInteger(expr : TTypedExpr) : TgpRegister64; override;
      procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
      function CompileBooleanValue(expr : TTypedExpr) : Integer; override;
   end;

   Tx86MagicBoolFunc = class (Tx86MagicFunc)
      function DoCompileInteger(expr : TTypedExpr) : TgpRegister64; override;
   end;

   Tx86DirectCallFunc = class (Tx86InterpretedExpr)
      public
         AddrPtr : PPointer;
         constructor Create(jit : TdwsJITx86_64; addrPtr : PPointer);
         function CompileCall(funcSym : TFuncSymbol; const args : TExprBaseListRec) : Boolean;
         function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
         function DoCompileInteger(expr : TTypedExpr) : TgpRegister64; override;
         procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
         function CompileBooleanValue(expr : TTypedExpr) : Integer; override;
   end;

   Tx86AbsIntFunc = class (TdwsJITter_x86)
      function DoCompileInteger(expr : TTypedExpr) : TgpRegister64; override;
   end;
   Tx86AbsFloatFunc = class (TdwsJITter_x86)
      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
   end;

   Tx86SqrtFunc = class (Tx86MagicFunc)
      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
   end;
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
}
   Tx86RoundFunc = class (Tx86MagicFunc)
      function DoCompileInteger(expr : TTypedExpr) : TgpRegister64; override;
      function DoCompileFloat(expr : TTypedExpr) : TxmmRegister; override;
   end;

   Tx86OddFunc = class (Tx86MagicBoolFunc)
      procedure DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); override;
      function CompileBooleanValue(expr : TTypedExpr) : Integer; override;
   end;

   Tx86Unsigned32Func = class (Tx86MagicFunc)
      function DoCompileInteger(expr : TTypedExpr) : TgpRegister64; override;
   end;

   Tx86ClampIntFunc = class (Tx86MagicFunc)
      function DoCompileInteger(expr : TTypedExpr) : TgpRegister64; override;
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
   cVariant_DataOffset = 8;

   cVolatileXMM : TxmmRegisters = [ xmm0, xmm1, xmm2, xmm3, xmm4, xmm5 ];
   cVolatileGPR : TgpRegister64s = [ gprRAX, gprRCX, gprRDX, gprR8, gprR9, gprR10, gprR11 ];

   cAllocatableGPRegs : TgpRegister64s = [
      //gprRAX, gprRCX, gprRDX, // volatile
      //gprRBX, gprRBP, gprRSP  // special purpose
      gprRSI,
      // gprRDI,                // special purpose (exec instance)
      //gprR8, gprR9,           // volatile
      //gprR10, gprR11,         // volatile, to be preserved by caller, used in sycal/sysret
      gprR12, gprR13, gprR14, gprR15
   ];


function int64_div(a, b : Int64) : Int64;
begin
   Result:=a div b;
end;

function int64_mod(a, b : Int64) : Int64;
begin
   Result:=a mod b;
end;

function double_floor(v : Double) : Int64;
begin
   Result := Floor(v);
end;

function double_ceil(v : Double) : Int64;
begin
   Result := Ceil(v);
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
   vAddr_Floor : function (v : Double) : Int64 = double_floor;
   vAddr_Ceil : function (v : Double) : Int64 = double_ceil;
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
   RegisterJITter(TBoolVarExpr,                 Tx86BoolVar.Create(Self));
   RegisterJITter(TObjectVarExpr,               Tx86ObjectVar.Create(Self));
   RegisterJITter(TSelfObjectVarExpr,           Tx86ObjectVar.Create(Self));
   RegisterJITter(TVarParentExpr,               FInterpretedJITter.IncRefCount);

   RegisterJITter(TFieldExpr,                   FInterpretedJITter.IncRefCount);
   RegisterJITter(TRecordExpr,                  FInterpretedJITter.IncRefCount);
   RegisterJITter(TRecordVarExpr,               FInterpretedJITter.IncRefCount);// Tx86RecordVar.Create(Self));
   RegisterJITter(TFieldExpr,                   FInterpretedJITter.IncRefCount);
   RegisterJITter(TFieldVarExpr,                FInterpretedJITter.IncRefCount);// Tx86FieldVar.Create(Self));

   RegisterJITter(TVarParamExpr,                FInterpretedJITter.IncRefCount);// Tx86VarParam.Create(Self));
   RegisterJITter(TConstParamExpr,              FInterpretedJITter.IncRefCount);
   RegisterJITter(TLazyParamExpr,               FInterpretedJITter.IncRefCount);
   RegisterJITter(TVarParentExpr,               FInterpretedJITter.IncRefCount);
   RegisterJITter(TVarParamParentExpr,          FInterpretedJITter.IncRefCount);

   RegisterJITter(TStaticArrayExpr,             Tx86StaticArray.Create(Self));
   RegisterJITter(TDynamicArrayExpr,            Tx86DynamicArray.Create(Self));
   RegisterJITter(TDynamicArrayVarExpr,         Tx86DynamicArray.Create(Self));
   RegisterJITter(TDynamicArraySetExpr,         Tx86DynamicArraySet.Create(Self));
   RegisterJITter(TDynamicArraySetVarExpr,      Tx86DynamicArraySet.Create(Self));
   RegisterJITter(TDynamicArraySetDataExpr,     FInterpretedJITter.IncRefCount);

   RegisterJITter(TArrayLengthExpr,             FInterpretedJITter.IncRefCount);
   RegisterJITter(TArraySetLengthExpr,          FInterpretedJITter.IncRefCount);
   RegisterJITter(TArrayAddExpr,                FInterpretedJITter.IncRefCount);
   RegisterJITter(TArrayInsertExpr,             FInterpretedJITter.IncRefCount);

   RegisterJITter(TDynamicArrayIndexOfDataExpr,    FInterpretedJITter.IncRefCount);
   RegisterJITter(TDynamicArrayIndexOfFuncPtrExpr, FInterpretedJITter.IncRefCount);
   RegisterJITter(TDynamicArrayIndexOfVariantExpr, FInterpretedJITter.IncRefCount);
   RegisterJITter(TDynamicArrayIndexOfIntegerExpr, FInterpretedJITter.IncRefCount);
   RegisterJITter(TDynamicArrayIndexOfStringExpr,  FInterpretedJITter.IncRefCount);
   RegisterJITter(TDynamicArrayIndexOfFloatExpr,   FInterpretedJITter.IncRefCount);

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
   RegisterJITter(TAssignConstToBoolVarExpr,    Tx86AssignConstToBoolVar.Create(Self));
   RegisterJITter(TAssignConstToStringVarExpr,  FInterpretedJITter.IncRefCount);
   RegisterJITter(TAssignExpr,                  Tx86Assign.Create(Self));
   RegisterJITter(TAssignDataExpr,              FInterpretedJITter.IncRefCount);// Tx86AssignData.Create(Self));
   RegisterJITter(TAssignClassOfExpr,           FInterpretedJITter.IncRefCount);
   RegisterJITter(TAssignFuncExpr,              FInterpretedJITter.IncRefCount);
   RegisterJITter(TAssignNilToVarExpr,          FInterpretedJITter.IncRefCount);
   RegisterJITter(TAssignNilClassToVarExpr,     FInterpretedJITter.IncRefCount);
   RegisterJITter(TAssignArrayConstantExpr,     FInterpretedJITter.IncRefCount);

   RegisterJITter(TAssignedInstanceExpr,        FInterpretedJITter.IncRefCount);
   RegisterJITter(TAssignedInterfaceExpr,       FInterpretedJITter.IncRefCount);
   RegisterJITter(TAssignedMetaClassExpr,       FInterpretedJITter.IncRefCount);
   RegisterJITter(TAssignedFuncPtrExpr,         FInterpretedJITter.IncRefCount);

   RegisterJITter(TPlusAssignFloatExpr,         Tx86OpAssignFloat.Create(Self, xmm_addsd));
   RegisterJITter(TMinusAssignFloatExpr,        Tx86OpAssignFloat.Create(Self, xmm_subsd));
   RegisterJITter(TMultAssignFloatExpr,         Tx86OpAssignFloat.Create(Self, xmm_multsd));
   RegisterJITter(TDivideAssignExpr,            Tx86OpAssignFloat.Create(Self, xmm_divsd));

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
   RegisterJITter(TWhileExpr,                   Tx86While.Create(Self));

   RegisterJITter(TForUpwardExpr,               Tx86ForUpward.Create(Self));
   RegisterJITter(TForUpwardStepExpr,           Tx86ForUpward.Create(Self));
   RegisterJITter(TForDownwardExpr,             FInterpretedJITter.IncRefCount);
   RegisterJITter(TForDownwardStepExpr,         FInterpretedJITter.IncRefCount);
   RegisterJITter(TForCharCodeInStrExpr,        FInterpretedJITter.IncRefCount);
   RegisterJITter(TForCharInStrExpr,            FInterpretedJITter.IncRefCount);

   RegisterJITter(TContinueExpr,                Tx86Continue.Create(Self));
   RegisterJITter(TBreakExpr,                   Tx86Break.Create(Self));
   RegisterJITter(TExitExpr,                    Tx86Exit.Create(Self));
   RegisterJITter(TExitValueExpr,               Tx86ExitValue.Create(Self));

   RegisterJITter(TRaiseExpr,                   FInterpretedJITter.IncRefCount);
   RegisterJITter(TExceptExpr,                  FInterpretedJITter.IncRefCount);
   RegisterJITter(TFinallyExpr,                 FInterpretedJITter.IncRefCount);

   RegisterJITter(TAddFloatExpr,                Tx86FloatBinOp.Create(Self, xmm_addsd));
   RegisterJITter(TSubFloatExpr,                Tx86FloatBinOp.Create(Self, xmm_subsd));
   RegisterJITter(TMultFloatExpr,               Tx86FloatBinOp.Create(Self, xmm_multsd));
   RegisterJITter(TSqrFloatExpr,                Tx86SqrFloat.Create(Self));
   RegisterJITter(TDivideExpr,                  Tx86FloatBinOp.Create(Self, xmm_divsd));
   RegisterJITter(TModFloatExpr,                FInterpretedJITter.IncRefCount);// Tx86ModFloat.Create(Self));
   RegisterJITter(TNegFloatExpr,                Tx86NegFloat.Create(Self));

   RegisterJITter(TNegIntExpr,                  Tx86NegInt.Create(Self));
   RegisterJITter(TNotIntExpr,                  Tx86NotInt.Create(Self));
   RegisterJITter(TAddIntExpr,                  Tx86IntegerBinOpExpr.Create(Self, gpOp_add));
   RegisterJITter(TSubIntExpr,                  Tx86IntegerBinOpExpr.Create(Self, gpOp_sub, False));
   RegisterJITter(TMultIntExpr,                 Tx86MultInt.Create(Self));
   RegisterJITter(TSqrIntExpr,                  FInterpretedJITter.IncRefCount);
   RegisterJITter(TDivExpr,                     FInterpretedJITter.IncRefCount);// Tx86DivInt.Create(Self));
   RegisterJITter(TDivConstExpr,                FInterpretedJITter.IncRefCount);// Tx86DivInt.Create(Self));
   RegisterJITter(TModExpr,                     FInterpretedJITter.IncRefCount);// Tx86ModInt.Create(Self));
   RegisterJITter(TModConstExpr,                FInterpretedJITter.IncRefCount);// Tx86ModInt.Create(Self));
   RegisterJITter(TMultIntPow2Expr,             FInterpretedJITter.IncRefCount);// Tx86MultIntPow2.Create(Self));
   RegisterJITter(TIntAndExpr,                  Tx86IntegerBinOpExpr.Create(Self, gpOp_and));
   RegisterJITter(TIntXorExpr,                  Tx86IntegerBinOpExpr.Create(Self, gpOp_xor));
   RegisterJITter(TIntOrExpr,                   Tx86IntegerBinOpExpr.Create(Self, gpOp_or));

   RegisterJITter(TShrExpr,                     Tx86Shift.Create(Self, gpShr));
   RegisterJITter(TShlExpr,                     Tx86Shift.Create(Self, gpShl));
   RegisterJITter(TSarExpr,                     Tx86Shift.Create(Self, gpSar));

   RegisterJITter(TInOpExpr,                    FInterpretedJITter.IncRefCount);
   RegisterJITter(TStringInOpExpr,              FInterpretedJITter.IncRefCount);
   RegisterJITter(TStringInOpStaticSetExpr,     FInterpretedJITter.IncRefCount);
   RegisterJITter(TIntegerInOpExpr,             FInterpretedJITter.IncRefCount);
   RegisterJITter(TBitwiseInOpExpr,             FInterpretedJITter.IncRefCount);

   RegisterJITter(TIncIntVarExpr,               Tx86IncIntVar.Create(Self));
   RegisterJITter(TDecIntVarExpr,               Tx86DecIntVar.Create(Self));
   RegisterJITter(TIncIntVarWithConstExpr,      Tx86IncIntVar.Create(Self));

   RegisterJITter(TIncVarFuncExpr,              Tx86IncVarFunc.Create(Self));
   RegisterJITter(TDecVarFuncExpr,              Tx86DecVarFunc.Create(Self));

   RegisterJITter(TRelEqualIntExpr,             Tx86RelEqualInt.Create(Self));
   RegisterJITter(TRelNotEqualIntExpr,          Tx86RelNotEqualInt.Create(Self));
   RegisterJITter(TRelGreaterIntExpr,           Tx86RelOpInt.Create(Self, flagsG));
   RegisterJITter(TRelGreaterEqualIntExpr,      Tx86RelOpInt.Create(Self, flagsGE));
   RegisterJITter(TRelLessIntExpr,              Tx86RelOpInt.Create(Self, flagsL));
   RegisterJITter(TRelLessEqualIntExpr,         Tx86RelOpInt.Create(Self, flagsLE));

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
   RegisterJITter(TRelNotEqualMetaExpr,         Tx86RelNotEqualInt.Create(Self));

   RegisterJITter(TRelVarEqualNilExpr,          FInterpretedJITter.IncRefCount);
   RegisterJITter(TRelVarNotEqualNilExpr,       FInterpretedJITter.IncRefCount);

   RegisterJITter(TIsOpExpr,                    FInterpretedJITter.IncRefCount);
   RegisterJITter(TImplementsIntfOpExpr,        FInterpretedJITter.IncRefCount);
   RegisterJITter(TObjCmpEqualExpr,             FInterpretedJITter.IncRefCount);
   RegisterJITter(TObjCmpNotEqualExpr,          FInterpretedJITter.IncRefCount);
   RegisterJITter(TIntfCmpExpr,                 FInterpretedJITter.IncRefCount);

   RegisterJITter(TNotBoolExpr,                 Tx86NotExpr.Create(Self));
   RegisterJITter(TBoolOrExpr,                  Tx86BoolOrExpr.Create(Self));
   RegisterJITter(TBoolAndExpr,                 Tx86BoolAndExpr.Create(Self));
   RegisterJITter(TBoolXorExpr,                 FInterpretedJITter.IncRefCount);
   RegisterJITter(TBoolImpliesExpr,             FInterpretedJITter.IncRefCount);

   RegisterJITter(TCoalesceExpr,                FInterpretedJITter.IncRefCount);
   RegisterJITter(TCoalesceStrExpr,             FInterpretedJITter.IncRefCount);
   RegisterJITter(TCoalesceClassExpr,           FInterpretedJITter.IncRefCount);
   RegisterJITter(TCoalesceDynArrayExpr,        FInterpretedJITter.IncRefCount);

   RegisterJITter(TSetOfInExpr,                 FInterpretedJITter.IncRefCount);// Tx86SetOfInExpr.Create(Self));
   RegisterJITter(TSetOfSmallInExpr,            FInterpretedJITter.IncRefCount);// Tx86SetOfInExpr.Create(Self));
   RegisterJITter(TSetOfIncludeExpr,            FInterpretedJITter.IncRefCount);// Tx86SetOfInclude.Create(Self));
   RegisterJITter(TSetOfExcludeExpr,            FInterpretedJITter.IncRefCount);// Tx86SetOfExclude.Create(Self));

   RegisterJITter(TOrdExpr,                     FInterpretedJITter.IncRefCount);
   RegisterJITter(TOrdBoolExpr,                 FInterpretedJITter.IncRefCount);// Tx86OrdBool.Create(Self));
   RegisterJITter(TOrdIntExpr,                  FInterpretedJITter.IncRefCount);// Tx86OrdInt.Create(Self));
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

   RegisterJITter(TAbsIntFunc,                  Tx86AbsIntFunc.Create(Self));
   RegisterJITter(TAbsFloatFunc,                Tx86AbsFloatFunc.Create(Self));

   RegisterJITter(TSqrtFunc,                    Tx86SqrtFunc.Create(Self));
   RegisterJITter(TSqrFloatFunc,                Tx86SqrFloatFunc.Create(Self));
   RegisterJITter(TMaxFunc,                     FInterpretedJITter.IncRefCount);// Tx86MinMaxFloatFunc.Create(Self, xmm_maxsd));
   RegisterJITter(TMinFunc,                     FInterpretedJITter.IncRefCount);// Tx86MinMaxFloatFunc.Create(Self, xmm_minsd));

   RegisterJITter(TExpFunc,                     Tx86DirectCallFunc.Create(Self, @vAddr_Exp));
   RegisterJITter(TLnFunc,                      Tx86DirectCallFunc.Create(Self, @vAddr_Ln));
   RegisterJITter(TLog2Func,                    Tx86DirectCallFunc.Create(Self, @vAddr_Log2));
   RegisterJITter(TLog10Func,                   Tx86DirectCallFunc.Create(Self, @vAddr_Log10));

   RegisterJITter(TPowerFunc,                   Tx86DirectCallFunc.Create(Self, @vAddr_Power));

   RegisterJITter(TRoundFunc,                   Tx86RoundFunc.Create(Self));
   RegisterJITter(TFloorFunc,                   Tx86DirectCallFunc.Create(Self, @vAddr_Floor));
   RegisterJITter(TCeilFunc,                    Tx86DirectCallFunc.Create(Self, @vAddr_Ceil));
   RegisterJITter(TTruncFunc,                   Tx86DirectCallFunc.Create(Self, @vAddr_Trunc));
   RegisterJITter(TFracFunc,                    Tx86DirectCallFunc.Create(Self, @vAddr_Frac));
   RegisterJITter(TIsNaNFunc,                   Tx86DirectCallFunc.Create(Self, @vAddr_IsNaN));
   RegisterJITter(TIsInfiniteFunc,              FInterpretedJITter.IncRefCount);// Tx86DirectCallFunc.Create(Self, @@vAddr_IsInfinite));
   RegisterJITter(TIsFiniteFunc,                FInterpretedJITter.IncRefCount);// Tx86DirectCallFunc.Create(Self, @@vAddr_IsFinite));
   RegisterJITter(TIsPrimeFunc,                 FInterpretedJITter.IncRefCount);// Tx86DirectCallFunc.Create(Self, @@vAddr_IsPrime));

   RegisterJITter(TCosFunc,                     Tx86DirectCallFunc.Create(Self, @vAddr_Cos));
   RegisterJITter(TSinFunc,                     Tx86DirectCallFunc.Create(Self, @vAddr_Sin));
   RegisterJITter(TTanFunc,                     Tx86DirectCallFunc.Create(Self, @vAddr_Tan));

   RegisterJITter(TOddFunc,                     Tx86OddFunc.Create(Self));

   RegisterJITter(TUnsigned32Func,              Tx86Unsigned32Func.Create(Self));

   RegisterJITter(TClampIntFunc,                Tx86ClampIntFunc.Create(Self));
end;

// Destroy
//
destructor TdwsJITx86_64.Destroy;
begin
   inherited;
   FAllocator.Free;
   FInterpretedJITter.Free;
end;

// FlushRegisters
//
procedure TdwsJITx86_64.FlushRegisters(option : TRegisterFlushOption);
var
   xmm : TxmmRegister;
   gpr : TgpRegister64;
begin
   if OutputFailedOn <> nil then Exit;
   case option of
      regFlushAll : begin
         for xmm := xmm0 to High(TxmmRegister) do
            FXMMRegs[xmm].Flush;
         for gpr := gprRAX to High(TgpRegister64) do
            FGPRegs[gpr].Flush;
      end;
      regFlushVolatile : begin
         for xmm := xmm0 to High(TxmmRegister) do
            if xmm in cVolatileXMM then
               FXMMRegs[xmm].Flush;
         for gpr := gprRAX to High(TgpRegister64) do
            if gpr in cVolatileGPR then
               FGPRegs[gpr].Flush;
      end;
      regFlushIntermediateExprs : begin
         for xmm := xmm0 to High(TxmmRegister) do
            if FXMMRegs[xmm].DataSymbol = nil then
               FXMMRegs[xmm].Flush;
         for gpr := gprRAX to High(TgpRegister64) do
            if FGPRegs[gpr].DataSymbol = nil then
               FGPRegs[gpr].Flush;
      end;
      regFlushDataSymbols : begin
         for xmm := xmm0 to High(TxmmRegister) do
            if FXMMRegs[xmm].DataSymbol <> nil then
               FXMMRegs[xmm].Flush;
         for gpr := gprRAX to High(TgpRegister64) do
            if FGPRegs[gpr].DataSymbol <> nil then
               FGPRegs[gpr].Flush;
      end;
   else
      Assert(False);
   end;
end;

// SaveRegisters
//
procedure TdwsJITx86_64.SaveRegisters;
var
   xmm : TxmmRegister;
   gpr : TgpRegister64;
begin
   Assert((FXMMSaved = []) and (FGPRSaved = []));

   for xmm := Low(FXMMRegsStackAddr) to High(FXMMRegsStackAddr) do begin
      if (xmm in cVolatileXMM) and (FXMMRegs[xmm].Lock > 0) then begin
         Include(FXMMSaved, xmm);
         if FXMMRegsStackAddr[xmm] = MaxInt then
            FXMMRegsStackAddr[xmm] := FPreamble.AllocateStackSpace(SizeOf(Double));
         x86._movsd_qword_ptr_reg_reg(gprRBP, FXMMRegsStackAddr[xmm], xmm);
      end;
   end;

   for gpr := Low(FGPRegsStackAddr) to High(FGPRegsStackAddr) do begin
      if (gpr in cVolatileGPR) and (FGPRegs[gpr].Lock > 0) then begin
         Include(FGPRSaved, gpr);
         if FGPRegsStackAddr[gpr] = MaxInt then
            FGPRegsStackAddr[gpr] := FPreamble.AllocateStackSpace(SizeOf(Double));
         x86._mov_qword_ptr_reg_reg(gprRBP, FGPRegsStackAddr[gpr], gpr);
      end;
   end;
end;

// RestoreRegisters
//
procedure TdwsJITx86_64.RestoreRegisters;
var
   xmm : TxmmRegister;
   gpr : TgpRegister64;
begin
   for xmm := Low(FXMMRegsStackAddr) to High(FXMMRegsStackAddr) do begin
      if xmm in FXMMSaved then
         x86._movsd_reg_qword_ptr_reg(xmm, gprRBP, FXMMRegsStackAddr[xmm]);
      FXMMRegs[xmm].DataSymbol := nil;
   end;

   FXMMSaved := [];

   for gpr := Low(FGPRegsStackAddr) to High(FGPRegsStackAddr) do begin
      if gpr in FGPRSaved then
         x86._mov_reg_qword_ptr_reg(gpr, gprRBP, FGPRegsStackAddr[gpr]);
      FGPRegs[gpr].DataSymbol := nil;
   end;

   FGPRSaved := [];
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

// ResetRegisters
//
procedure TdwsJITx86_64.ResetRegisters;
begin
   for var i := Low(FXMMRegs) to High(FXMMRegs) do
      FXMMRegs[i].Reset;
   for var i := Low(FGPRegs) to High(FGPRegs) do
      FGPRegs[i].Reset;
end;

// ResetRegStackAddr
//
procedure TdwsJITx86_64.ResetRegStackAddr;
begin
   for var i := Low(FXMMRegsStackAddr) to High(FXMMRegsStackAddr) do
      FXMMRegsStackAddr[i] := MaxInt;
   for var i := Low(FGPRegsStackAddr) to High(FGPRegsStackAddr) do
      FGPRegsStackAddr[i] := MaxInt;
end;

// CanonicXMMDataSymbol
//
function TdwsJITx86_64.CanonicXMMDataSymbol(expr : TExprBase) : TDataSymbol;
begin
   if (expr is TVarExpr) and IsFloat(TVarExpr(expr).DataSymbol.Typ)  then
      Result := TVarExpr(expr).DataSymbol
   else Result := nil;
end;

// CanonicGPRDataSymbol
//
function TdwsJITx86_64.CanonicGPRDataSymbol(expr : TExprBase) : TDataSymbol;
begin
   if (expr is TVarExpr) and (TVarExpr(expr).DataSymbol.Typ is TBaseIntegerSymbol)  then
      Result := TVarExpr(expr).DataSymbol
   else Result := nil;
end;

// AllocXMMReg
//
function TdwsJITx86_64.AllocXMMReg(expr : TExprBase) : TxmmRegister;
var
   i : TxmmRegister;
   dataSymbol : TDataSymbol;
   p : PRegisterStatus;
begin
   Assert(expr <> nil);
   dataSymbol := CanonicXMMDataSymbol(expr);

   Result := xmm0;

   // find unused register
   for i := xmm1 to High(FXMMRegs) do begin
      p := @FXMMRegs[i];
      if p.Lock = 0 then begin
         if p.DataSymbol = nil then begin
            Result := i;
            Break;
         end else if Result = xmm0 then
            Result := i;
      end;
   end;

   if Result <> xmm0 then begin

      p := @FXMMRegs[Result];
      p.DataSymbol := dataSymbol;
      p.Expr := expr;
      p.Lock := 1;
      FPreamble.NotifyXMMAlteration(Result);

   end else OutputFailedOn := expr;
end;

// ReleaseXMMReg
//
procedure TdwsJITx86_64.ReleaseXMMReg(reg : TxmmRegister);
begin
   Assert((FXMMRegs[reg].Lock > 0) or (OutputFailedOn <> nil));
   Dec(FXMMRegs[reg].Lock);
end;

// CurrentXMMReg
//
function TdwsJITx86_64.CurrentXMMReg(expr : TExprBase) : TxmmRegister;
var
   i : TxmmRegister;
   dataSymbol : TDataSymbol;
begin
   dataSymbol := CanonicXMMDataSymbol(expr);

   if dataSymbol <> nil then begin
      for i := xmm1 to High(FXMMRegs) do begin
         if FXMMRegs[i].DataSymbol = dataSymbol then begin
            Inc(FXMMRegs[i].Lock);
            Exit(i);
         end;
      end;
   end;
   Result := xmmNone;
end;

// IsSymbolXMMReg
//
function TdwsJITx86_64.IsSymbolXMMReg(reg : TxmmRegister) : Boolean;
begin
   Result := FXMMRegs[reg].DataSymbol <> nil;
end;

// SetContainsXMMReg
//
procedure TdwsJITx86_64.SetContainsXMMReg(reg : TxmmRegister; expr : TExprBase);
var
   dataSymbol : TDataSymbol;
begin
   Assert(FXMMRegs[reg].DataSymbol = nil);
   Assert(FXMMRegs[reg].Lock <= 1, IntToStr(FXMMRegs[reg].Lock));

   dataSymbol := CanonicXMMDataSymbol(expr);
   FlushXMMSymbol(dataSymbol);

   FXMMRegs[reg].DataSymbol := dataSymbol;
   FXMMRegs[reg].Expr := expr;
end;

// FlushXMMSymbol
//
procedure TdwsJITx86_64.FlushXMMSymbol(dataSymbol : TDataSymbol);
var
   i : TxmmRegister;
begin
   if dataSymbol <> nil then begin
      for i := xmm1 to High(FXMMRegs) do begin
         if FXMMRegs[i].DataSymbol = dataSymbol then begin
            FXMMRegs[i].DataSymbol := nil;
         end;
      end;
   end;
end;

// AllocGPReg
//
function TdwsJITx86_64.AllocGPReg(expr : TExprBase) : TgpRegister64;
var
   i : TgpRegister64;
   dataSymbol : TDataSymbol;
   p : PRegisterStatus;
begin
   dataSymbol := CanonicGPRDataSymbol(expr);

   Result := gprRAX;

   // find unused register
   for i := gprRCX to High(FGPRegs) do begin
      p := @FGPRegs[i];
      if (i in cAllocatableGPRegs) and (p.Lock = 0) then begin
         if p.DataSymbol = nil then begin
            Result := i;
            Break;
         end else if Result = gprRAX then
            Result := i;
      end;
   end;

   if Result <> gprRAX then begin

      p := @FGPRegs[Result];
      p.DataSymbol := dataSymbol;
      p.Expr := expr;
      p.Lock := 1;
      FPreamble.NotifyGPAlteration(Result);

   end else OutputFailedOn := expr;
end;

// ReleaseGPReg
//
procedure TdwsJITx86_64.ReleaseGPReg(reg : TgpRegister64);
begin
   if OutputFailedOn <> nil then Exit;
   Assert(reg in cAllocatableGPRegs);
   Assert((FGPRegs[reg].Lock > 0) or (OutputFailedOn <> nil));
   Dec(FGPRegs[reg].Lock);
end;

// CurrentGPReg
//
function TdwsJITx86_64.CurrentGPReg(expr : TExprBase; var reg : TgpRegister64) : Boolean;
var
   i : TgpRegister64;
   dataSymbol : TDataSymbol;
begin
   dataSymbol := CanonicGPRDataSymbol(expr);

   if dataSymbol <> nil then begin
      for i := gprRCX to High(FGPRegs) do begin
         if FGPRegs[i].DataSymbol = dataSymbol then begin
            Inc(FGPRegs[i].Lock);
            reg := i;
            Exit(True);
         end;
      end;
   end;
   Result := False;
end;

// IsSymbolGPReg
//
function TdwsJITx86_64.IsSymbolGPReg(reg : TgpRegister64) : Boolean;
begin
   Result := FGPRegs[reg].DataSymbol <> nil;
end;

// SetContainsGPReg
//
procedure TdwsJITx86_64.SetContainsGPReg(reg : TgpRegister64; expr : TExprBase);
var
   dataSymbol : TDataSymbol;
begin
   if OutputFailedOn <> nil then Exit;

   Assert(FGPRegs[reg].DataSymbol = nil);
   Assert(FGPRegs[reg].Lock = 1);

   dataSymbol := CanonicGPRDataSymbol(expr);
   FlushGPRSymbol(dataSymbol);

   FGPRegs[reg].DataSymbol := dataSymbol;
   FGPRegs[reg].Expr := expr;
end;

// FlushGPReg
//
procedure TdwsJITx86_64.FlushGPReg(reg : TgpRegister64);
begin
   FGPRegs[reg].Flush;
end;

// FlushGPRSymbol
//
procedure TdwsJITx86_64.FlushGPRSymbol(dataSymbol : TDataSymbol);
var
   i : TgpRegister64;
begin
   if dataSymbol <> nil then begin
      for i := gprRCX to High(FGPRegs) do begin
         if FGPRegs[i].DataSymbol = dataSymbol then begin
            FGPRegs[i].DataSymbol := nil;
         end;
      end;
   end;
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

// CompileInteger
//
function TdwsJITx86_64.CompileInteger(expr : TTypedExpr) : Integer;
var
   reg : TgpRegister64;
begin
   reg := CompileIntegerToRegister(expr);
   if reg <> gprRAX then
      x86._mov_reg_reg(gprRAX, reg);
   Result := 0;
end;

// CompileIntegerToRegister
//
function TdwsJITx86_64.CompileIntegerToRegister(expr : TTypedExpr) : TgpRegister64;
begin
   Result := TgpRegister64(inherited CompileInteger(expr));
end;

// CompileAssignInteger
//
procedure TdwsJITx86_64.CompileAssignInteger(expr : TTypedExpr; source : TgpRegister64);
begin
   inherited CompileAssignInteger(expr, Ord(source));
end;

// CompileAssignExprToInteger
//
procedure TdwsJITx86_64.CompileAssignExprToInteger(dest, source : TTypedExpr);
var
   gpr : TgpRegister64;
begin
   gpr := CompileIntegerToRegister(source);
   CompileAssignInteger(dest, gpr);
   ReleaseGPReg(gpr);
end;

// CompileScriptObj
//
function TdwsJITx86_64.CompileScriptObj(expr : TTypedExpr) : TgpRegister64;
begin
   Result := TgpRegister64(inherited CompileScriptObj(expr));
end;

// CompileScriptDynArray
//
function TdwsJITx86_64.CompileScriptDynArray(expr : TTypedExpr) : TgpRegister64;
begin
   Result := TgpRegister64(inherited CompileScriptDynArray(expr));
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
   ResetRegisters;
   ResetRegStackAddr;
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
   x86._mov_reg_reg(gprRAX, TgpRegister64(resultHandle));
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
      else Fixups.NewXMMOpRegImm(OP, dest, c.Value);

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

// _cmp_reg_imm
//
procedure TdwsJITx86_64._cmp_reg_imm(reg : TgpRegister64; value : Int64);
begin
   if Int32(value) = value then
      x86._cmp_reg_imm(reg, value)
   else Fixups.NewCmpRegImm(reg, value);
end;

// _op_reg_imm
//
procedure TdwsJITx86_64._op_reg_imm(const op : TgpOp; reg : TgpRegister64; value : Int64);
begin
   if Int32(value) = value then
      if op.SIB = gpOp_add.SIB then
         x86._add_reg_imm(reg, value)
      else if op.SIB = gpOp_sub.SIB then
         x86._sub_reg_imm(reg, value)
      else x86._op_reg_imm(op, reg, value)
   else Fixups.NewGPOpRegImm(op, reg, value);
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

   _mov_reg_execInstance(gprRCX);
   x86._mov_reg_imm(gprRDX, IntPtr(expr));

   x86._call_absmem(cPtr_TdwsExecution_DoStep);

   FlushRegisters(regFlushVolatile);
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
   NotifyGPAlteration(cExecMemGPR);

   if FPreserveExec then
      NotifyGPAlteration(cExecInstanceGPR);

   if NeedRBP then
      NotifyGPAlteration(gprRBP);

   for var reg := Low(TgpRegister64) to High(TgpRegister64) do begin
      if ((1 shl Ord(reg)) and FAlteredGP) <> 0 then
         FUnwind.PushNonVolatile(UNWIND_REG(reg));
   end;

   if NeedRBP then
      FUnwind.AddCustomPrologOp([ $48, $89, $E5 ]); // mov rbp, rsp

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

// NotifyXMMAlteration
//
procedure TFixupPreamble.NotifyXMMAlteration(xmm : TxmmRegister);
begin
   FAlteredXMM := FAlteredXMM or (1 shl Ord(xmm));
end;

// NotifyGPAlteration
//
procedure TFixupPreamble.NotifyGPAlteration(gp : TgpRegister64);
begin
   FAlteredGP := FAlteredGP or (1 shl Ord(gp));
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

// NewCmpRegImm
//
function Tx86_64FixupLogicHelper.NewCmpRegImm(reg : TgpRegister64; const value : Int64) : TStaticDataFixup;
begin
   Result := NewGPOpRegImm(gpOp_cmp, reg, value);
end;

// NewGPOpRegImm
//
function Tx86_64FixupLogicHelper.NewGPOpRegImm(const op : TgpOp; reg : TgpRegister64; const value : Int64) : TStaticDataFixup;
begin
   Result := TStaticDataFixup.Create([
      $48 + 4*Ord(reg >= gprR8), op.LongEAX - 2, $05 + 8*(Ord(reg) and 7)
   ]);
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

// NewXMMOpRegImm
//
function Tx86_64FixupLogicHelper.NewXMMOpRegImm(op : TxmmOp; reg : TxmmRegister; const value : Double) : TStaticDataFixup;
begin
   if reg < xmm8 then
      Result := TStaticDataFixup.Create([$F2, $0F, Ord(op), $05 + 8*(Ord(reg) and 7)])
   else Result := TStaticDataFixup.Create([$F2, $44, $0F, Ord(op), $05 + 8*(Ord(reg) and 7)]);
   Assert(reg in [xmm0..High(TxmmRegister)]);
   Result.Logic := Self;
   Result.DataIndex := AddStaticData(PUInt64(@value)^);
   AddFixup(Result);
end;

// NewXMMOpRegPDImm
//
function Tx86_64FixupLogicHelper.NewXMMOpRegPDImm(op : TxmmOp_pd; reg : TxmmRegister; const value1, value2 : Double) : TStaticDataFixup;
begin
   if reg < xmm8 then
      Result := TStaticDataFixup.Create([$66, $0F, Ord(op), $05 + 8*(Ord(reg) and 7)])
   else Result := TStaticDataFixup.Create([$66, $44, $0F, Ord(op), $05 + 8*(Ord(reg) and 7)]);
   Assert(reg in [xmm0..High(TxmmRegister)]);
   Result.Logic := Self;
   Result.DataIndex := AddStaticData128(PUInt64(@value1)^, PUInt64(@value2)^);
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
var
   gpr : TgpRegister64;
begin
   gpr := jit.CompileIntegerToRegister(expr);

   Result := jit.AllocXMMReg(expr);

   x86._cvtsi2sd(Result, gpr);

   jit.ReleaseGPReg(gpr);
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

// CompileScriptObj
//
function TdwsJITter_x86.CompileScriptObj(expr : TTypedExpr) : Integer;
begin
   Result := Ord(DoCompileScriptObj(expr));
end;

// DoCompileScriptObj
//
function TdwsJITter_x86.DoCompileScriptObj(expr : TTypedExpr) : TgpRegister64;
begin
   jit.OutputFailedOn:=expr;
   Result := gprRAX;
end;

// CompileInteger
//
function TdwsJITter_x86.CompileInteger(expr : TTypedExpr) : Integer;
begin
   Result := Ord(DoCompileInteger(expr));
end;

// DoCompileInteger
//
function TdwsJITter_x86.DoCompileInteger(expr : TTypedExpr) : TgpRegister64;
begin
   jit.OutputFailedOn:=expr;
   Result := gprRAX;
end;

// CompileAssignInteger
//
procedure TdwsJITter_x86.CompileAssignInteger(expr : TTypedExpr; source : Integer);
begin
   DoCompileAssignInteger(expr, TgpRegister64(source));
end;

// DoCompileAssignInteger
//
procedure TdwsJITter_x86.DoCompileAssignInteger(expr : TTypedExpr; source : TgpRegister64);
begin
   jit.OutputFailedOn := expr;
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
   x86._mov_reg_imm(gprRAX, 0);
   jit.Fixups.NewJump(targetDone);

   jit.Fixups.AddFixup(targetTrue);
   x86._mov_reg_imm(gprRAX, 1);

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
   jit.OutputFailedOn := expr;
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
   e := TAssignConstToFloatVarExpr(expr);

   reg := jit.AllocXMMReg(e.Left);

   // check below is necessary as -Nan will be reported equal to zero
   if (not IsNaN(e.Right)) and (e.Right=0)  then
      x86._xorps_reg_reg(reg, reg)
   else jit._movsd_reg_imm(reg, e.Right);

   jit.CompileAssignFloat(e.Left, reg);

   jit.ReleaseXMMReg(reg);
end;

// ------------------
// ------------------ Tx86AssignConstToIntegerVar ------------------
// ------------------

// CompileStatement
//
procedure Tx86AssignConstToIntegerVar.CompileStatement(expr : TExprBase);
var
   e : TAssignConstToIntegerVarExpr;
   reg : TgpRegister64;
begin
   e := TAssignConstToIntegerVarExpr(expr);

   reg := jit.AllocGPReg(e.Left);

   x86._mov_reg_imm(reg, e.Right);

   jit.CompileAssignInteger(e.Left, reg);

   jit.ReleaseGPReg(reg);
end;

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

      x86._mov_reg_imm(gprRAX, Ord(e.Right));
      jit.CompileAssignBoolean(e.Left, Ord(gprRAX));

   end else inherited;
end;

// ------------------
// ------------------ Tx86Assign ------------------
// ------------------

// CompileStatement
//
procedure Tx86Assign.CompileStatement(expr : TExprBase);
var
   e : TAssignExpr;
   xmm : TxmmRegister;
   gpr : TgpRegister64;
begin
   e := TAssignExpr(expr);

   if jit.IsFloat(e.Left) then begin

      xmm := jit.CompileFloat(e.Right);
      jit.CompileAssignFloat(e.Left, xmm);
      jit.ReleaseXMMReg(xmm);

   end else if jit.IsInteger(e.Left) then begin

      gpr := jit.CompileIntegerToRegister(e.Right);
      jit.CompileAssignInteger(e.Left, gpr);
      jit.ReleaseGPReg(gpr);

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
}
// ------------------
// ------------------ Tx86OpAssignFloat ------------------
// ------------------

// Create
//
constructor Tx86OpAssignFloat.Create(jit : TdwsJITx86_64; op : TxmmOp);
begin
   inherited Create(jit);
   Self.OP := op;
end;

// CompileStatement
//
procedure Tx86OpAssignFloat.CompileStatement(expr : TExprBase);
var
   e : TOpAssignExpr;
   regLeft, regRight : TxmmRegister;
begin
   e := TOpAssignExpr(expr);

   regRight := jit.CompileFloat(e.Right);
   regLeft := jit.CompileFloat(e.Left);

   x86._xmm_reg_reg(OP, regLeft, regRight);

   jit.ReleaseXMMReg(regRight);

   jit.CompileAssignFloat(e.Left, regLeft);

   jit.ReleaseXMMReg(regLeft);
end;

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
   for i := 0 to expr.SubExprCount-1 do begin
      if jit.OutputFailedOn<>nil then break;
      subExpr := expr.SubExpr[i];
      jit._DoStep(subExpr);
      jit.CompileStatement(subExpr);
      jit.FlushRegisters(regFlushIntermediateExprs);
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
   e := TIfThenExpr(expr);

   targetTrue := jit.Fixups.NewHangingTarget(False);
   targetFalse := jit.Fixups.NewHangingTarget(False);

   jit.CompileBoolean(e.CondExpr, targetTrue, targetFalse);

   jit.Fixups.AddFixup(targetTrue);

   jit.CompileStatement(e.ThenExpr);
   jit.FlushRegisters(regFlushAll);

   jit.Fixups.AddFixup(targetFalse);
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
   e := TIfThenElseExpr(expr);

   targetTrue := jit.Fixups.NewHangingTarget(False);
   targetFalse := jit.Fixups.NewHangingTarget(True);
   targetDone := jit.Fixups.NewHangingTarget(False);

   jit.CompileBoolean(e.CondExpr, targetTrue, targetFalse);

   jit.Fixups.AddFixup(targetTrue);

   jit.CompileStatement(e.ThenExpr);
   jit.FlushRegisters(regFlushAll);

   jit.Fixups.NewJump(flagsNone, targetDone);

   jit.Fixups.AddFixup(targetFalse);

   jit.CompileStatement(e.ElseExpr);
   jit.FlushRegisters(regFlushAll);

   jit.Fixups.AddFixup(targetDone);
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
   e := TLoopExpr(expr);

   jit.FlushRegisters(regFlushAll);

   targetLoop := jit.Fixups.NewTarget(True);
   targetExit := jit.Fixups.NewHangingTarget(False);

   jit.EnterLoop(targetLoop, targetExit);

   jit._DoStep(e.LoopExpr);
   jit.CompileStatement(e.LoopExpr);

   jit.Fixups.NewJump(flagsNone, targetLoop);

   jit.Fixups.AddFixup(targetExit);

   jit.LeaveLoop;

   jit.FlushRegisters(regFlushAll);
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
   e := TLoopExpr(expr);

   jit.FlushRegisters(regFlushAll);

   targetLoop := jit.Fixups.NewTarget(True);
   targetExit := jit.Fixups.NewHangingTarget(False);

   jit.EnterLoop(targetLoop, targetExit);

   jit._DoStep(e.LoopExpr);
   jit.CompileStatement(e.LoopExpr);

   jit._DoStep(e.CondExpr);
   jit.CompileBoolean(e.CondExpr, targetExit, targetLoop);

   jit.Fixups.AddFixup(targetExit);

   jit.LeaveLoop;

   jit.FlushRegisters(regFlushAll);
end;

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
   e := TLoopExpr(expr);

   jit.FlushRegisters(regFlushAll);

   targetLoop := jit.Fixups.NewTarget(True);
   targetLoopStart := jit.Fixups.NewHangingTarget(False);
   targetExit := jit.Fixups.NewHangingTarget(True);

   jit.EnterLoop(targetLoop, targetExit);

   jit._DoStep(e.CondExpr);
   jit.CompileBoolean(e.CondExpr, targetLoopStart, targetExit);

   jit.Fixups.AddFixup(targetLoopStart);

   jit._DoStep(e.LoopExpr);
   jit.CompileStatement(e.LoopExpr);

   jit.Fixups.NewJump(flagsNone, targetLoop);

   jit.Fixups.AddFixup(targetExit);

   jit.LeaveLoop;

   jit.FlushRegisters(regFlushAll);
end;

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
   toValue, stepValue : Int64;
   toValueIsConstant : Boolean;
   toValueOffset : Integer;
   stepValueIsConstant : Boolean;
   stepValueOffset : Integer;
   tmpReg : TgpRegister64;
begin
   e := TForExpr(expr);

   jit.CompileAssignExprToInteger(e.VarExpr, e.FromExpr);

   toValueIsConstant := (e.ToExpr is TConstIntExpr);
   if toValueIsConstant then begin

      toValue := TConstIntExpr(e.ToExpr).Value;
      toValueOffset := 0;

   end else begin

      toValue := 0;
      toValueOffset := jit.FPreamble.AllocateStackSpace(SizeOf(Int64));
      tmpReg := jit.CompileIntegerToRegister(e.ToExpr);
      x86._mov_qword_ptr_reg_reg(gprRBP, toValueOffset, tmpReg);
      jit.ReleaseGPReg(tmpReg);

   end;

   if e is TForUpwardStepExpr then begin

      if TForUpwardStepExpr(e).StepExpr is TConstIntExpr then begin

         stepValueIsConstant := True;
         stepValue := TConstIntExpr(TForUpwardStepExpr(e).StepExpr).Value;
         stepValueOffset := 0;

      end else begin

         stepValueIsConstant := False;
         stepValue := 0;
         stepValueOffset := jit.FPreamble.AllocateStackSpace(SizeOf(Int64));

         tmpReg := jit.CompileIntegerToRegister(TForUpwardStepExpr(e).StepExpr);
         x86._mov_qword_ptr_reg_reg(gprRBP, stepValueOffset, tmpReg);

         x86._cmp_reg_imm(tmpReg, 0);

         jit.ReleaseGPReg(tmpReg);

         stepCheckPassed := jit.Fixups.NewJump(flagsGE);

         x86._mov_reg_reg(gprR8, tmpReg);
         jit._mov_reg_execInstance(gprRDX);
         x86._mov_reg_imm(gprRCX, QWORD(expr));
         x86._call_absmem(cPtr_TForStepExpr_RaiseForLoopStepShouldBeStrictlyPositive);

         jit.FlushRegisters(regFlushVolatile);

         stepCheckPassed.NewTarget(True);

      end;

   end else begin

      stepValueIsConstant := True;
      stepValue := 1;
      stepValueOffset := 0;

   end;

   jit.FlushRegisters(regFlushAll);

   loopStart := jit.Fixups.NewTarget(True);
   loopContinue := jit.Fixups.NewHangingTarget(False);
   loopAfter := jit.Fixups.NewHangingTarget(True);

   jit.EnterLoop(loopContinue, loopAfter);

   tmpReg := jit.CompileIntegerToRegister(e.VarExpr);
   if toValueIsConstant then begin
      jit._cmp_reg_imm(tmpReg, toValue)
   end else begin
      x86._cmp_reg_qword_ptr_reg(tmpReg, gprRBP, toValueOffset);
   end;
   jit.ReleaseGPReg(tmpReg);
   jit.Fixups.NewJump(flagsG, loopAfter);

   jit._DoStep(e.DoExpr);
   jit.CompileStatement(e.DoExpr);

   jit.Fixups.AddFixup(loopContinue);

   if jit.CurrentGPReg(e.VarExpr, tmpReg) then begin

      if stepValueIsConstant then begin
         jit._op_reg_imm(gpOp_add, tmpReg, stepValue);
      end else begin
         x86._op_reg_qword_ptr_reg(gpOp_add, tmpReg, gprRBP, stepValueOffset);
      end;

      x86._mov_execmem_reg(e.VarExpr.StackAddr, tmpReg);
      jit.ReleaseGPReg(tmpReg);

   end else begin

      if stepValueIsConstant then begin
         x86._add_execmem_imm(e.VarExpr.StackAddr, stepValue);
      end else begin
         x86._mov_reg_qword_ptr_reg(gprRAX, gprRBP, stepValueOffset);
         x86._add_execmem_reg(e.VarExpr.StackAddr, gprRAX);
      end;

   end;

   if stepValue <> 1 then
      jit.Fixups.NewJump(flagsO, loopAfter);

   jit.Fixups.NewJump(flagsNone, loopStart);

   //if JIT.LoopContext.Exited then
      jit.FlushRegisters(regFlushAll);

   jit.LeaveLoop;

   jit.Fixups.AddFixup(loopAfter);
end;

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
end;

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
   left, right : TTypedExpr;
   leftReg : TxmmRegister;
begin
   e := TFloatBinOpExpr(expr);

   left := e.Left;
   if (OP in [ xmm_addsd, xmm_multsd ]) and (left is TConstExpr) then begin
      right := left;
      left := e.Right;
   end else right := e.Right;

   leftReg := jit.CompileFloat(left);
   if jit.IsSymbolXMMReg(leftReg) then begin
      Result := jit.AllocXMMReg(expr);
      x86._movsd_reg_reg(Result, leftReg);
      jit.ReleaseXMMReg(leftReg);
   end else begin
      Result := leftReg;
      jit.SetContainsXMMReg(Result, expr);
   end;
   jit._xmm_reg_expr(OP, Result, right);
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
   reg := jit.CompileFloat(operand);

   if jit.IsSymbolXMMReg(reg) then begin

      Result := jit.AllocXMMReg(sqrExpr);

      if x86.SupportsAVX then begin
         x86._vmulsd(Result, reg, reg);
      end else begin
         x86._movsd_reg_reg(Result, reg);
         x86._xmm_reg_reg(xmm_multsd, Result, reg);
      end;

      jit.ReleaseXMMReg(reg);

   end else begin

      Result := reg;

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
   reg : TxmmRegister;
begin
   e := TNegFloatExpr(expr);

   reg := jit.CompileFloat(e.Expr);

   if jit.IsSymbolXMMReg(reg) then begin

      Result := jit.AllocXMMReg(expr);
      x86._movsd_reg_reg(Result, reg);
      jit.ReleaseXMMReg(reg);

   end else begin

      Result := reg;
      jit.SetContainsXMMReg(Result, expr);

   end;

   jit.Fixups.NewNegRegImm(Result);
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

   Result := jit.AllocXMMReg(e);

   jit.Fixups.NewMovsdRegImm(Result, e.Value);
end;

// DoCompileInteger
//
function Tx86ConstInt.DoCompileInteger(expr : TTypedExpr) : TgpRegister64;
var
   e : TConstIntExpr;
begin
   e := TConstIntExpr(expr);

   Result := jit.AllocGPReg(e);

   x86._mov_reg_imm(Result, e.Value);
end;

// ------------------
// ------------------ Tx86ConstBoolean ------------------
// ------------------

// DoCompileInteger
//
function Tx86ConstBoolean.DoCompileInteger(expr : TTypedExpr) : TgpRegister64;
begin
   Result := jit.AllocGPReg(expr);
   x86._mov_reg_imm(Result, Ord(TConstBooleanExpr(expr).Value));
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
   jit.SaveRegisters;

   jit._mov_reg_execInstance(gprRDX);
   x86._mov_reg_qword(gprRCX, QWORD(expr));
   x86._mov_reg_qword_ptr_reg(gprRAX, gprRCX);

   x86._call_reg(gprRAX, vmt);

   jit.RestoreRegisters;

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

   jit.FlushRegisters(regFlushAll);
end;

// DoCompileFloat
//
function Tx86InterpretedExpr.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
begin
   if jit.IsInteger(expr) then
      Exit(inherited DoCompileFloat(expr));

   DoCallEval(expr, vmt_TExprBase_EvalAsFloat);

   Result := jit.AllocXMMReg(expr);
   x86._movsd_reg_reg(Result, xmm0);

   jit.QueueGreed(expr);
end;

// DoCompileAssignFloat
//
procedure Tx86InterpretedExpr.DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister);
begin
   x86._movsd_reg_reg(xmm2, source);   // passed as 3rd param (first is self, second is exec)

   DoCallEval(expr, vmt_TExprBase_AssignValueAsFloat);
end;

// DoCompileInteger
//
function Tx86InterpretedExpr.DoCompileInteger(expr : TTypedExpr) : TgpRegister64;
begin
   DoCallEval(expr, vmt_TExprBase_EvalAsInteger);

   jit.QueueGreed(expr);

   Result := jit.AllocGPReg(expr);
   x86._mov_reg_reg(Result, gprRAX);
end;

// DoCompileAssignInteger
//
procedure Tx86InterpretedExpr.DoCompileAssignInteger(expr : TTypedExpr; source : TgpRegister64);
begin
   x86._mov_reg_reg(gprR8, source);

   DoCallEval(expr, vmt_TExprBase_AssignValueAsInteger);
end;

// DoCompileBoolean
//
procedure Tx86InterpretedExpr.DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
begin
   DoCallEval(expr, vmt_TExprBase_EvalAsBoolean);

   x86._test_al_al;
   jit.Fixups.NewConditionalJumps(flagsNZ, targetTrue, targetFalse);

   jit.QueueGreed(expr);
end;

// CompileBooleanValue
//
function Tx86InterpretedExpr.CompileBooleanValue(expr : TTypedExpr) : Integer;
begin
   DoCallEval(expr, vmt_TExprBase_EvalAsBoolean);

   x86._op_reg_imm(gpOp_and, gprRAX, 255);

   jit.QueueGreed(expr);

   Result := 0;
end;

// CompileAssignBoolean
//
procedure Tx86InterpretedExpr.CompileAssignBoolean(expr : TTypedExpr; source : Integer);
begin
   x86._mov_reg_reg(gprR8, TgpRegister64(source));

   DoCallEval(expr, vmt_TExprBase_AssignValueAsBoolean);
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
   e := TFloatVarExpr(expr);

   x86._movsd_execmem_reg(e.StackAddr, source);

   if not jit.IsSymbolXMMReg(source) then
      jit.SetContainsXMMReg(source, e)
   else jit.FlushXMMSymbol(e.DataSymbol);
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

// DoCompileInteger
//
function Tx86IntVar.DoCompileInteger(expr : TTypedExpr) : TgpRegister64;
var
   e : TIntVarExpr;
begin
   e := TIntVarExpr(expr);

   if not jit.CurrentGPReg(e, Result) then begin

      Result := jit.AllocGPReg(expr);
      x86._mov_reg_execmem(Result, e.StackAddr);

   end;
end;

// DoCompileAssignInteger
//
procedure Tx86IntVar.DoCompileAssignInteger(expr : TTypedExpr; source : TgpRegister64);
var
   e : TIntVarExpr;
begin
   e := TIntVarExpr(expr);

   x86._mov_execmem_reg(e.StackAddr, source);

   if not jit.IsSymbolGPReg(source) then
      jit.SetContainsGPReg(source, e)
   else jit.FlushGPRSymbol(e.DataSymbol);
end;

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

   x86._cmp_execmem_imm(e.StackAddr, 0);
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

   x86._mov_qword_ptr_reg_reg(cExecMemGPR, StackAddrToOffset(e.StackAddr), TgpRegister64(source));
end;

// ------------------
// ------------------ Tx86ObjectVar ------------------
// ------------------

// DoCompileScriptObj
//
function Tx86ObjectVar.DoCompileScriptObj(expr : TTypedExpr) : TgpRegister64;
begin
   Result := jit.AllocGPReg(expr);
   x86._mov_reg_execmem(Result, TObjectVarExpr(expr).StackAddr);
end;
{
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
}
// ------------------
// ------------------ Tx86VarParam ------------------
// ------------------
{
// CompileAsPVariant
//
class procedure Tx86VarParam.CompileAsPVariant(x86 : Tx86_64_WriteOnlyStream; expr : TByRefParamExpr);
begin
   x86._mov_reg_execmem(gprRCX, expr.StackAddr);
   x86._xor_reg_reg(gprRDX, gprRDX);
   x86._mov_reg_qword_ptr_reg(gprRAX, gprRCX);
   x86._call_reg(gprRAX, vmt_IDataContext_AsPVariant);
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
// ------------------ Tx86StaticArray ------------------
// ------------------

// CompileToItemPtr
//
function Tx86StaticArray.CompileToItemPtr(expr : TStaticArrayExpr; var ptrReg : TgpRegister64; var offset : Integer) : Boolean;
var
   absPointer : IntPtr;
begin
   absPointer := 0;

   if expr.BaseExpr.ClassType = TVarExpr then begin

      ptrReg := cExecMemGPR;
      offset := StackAddrToOffset( TVarExpr(expr.BaseExpr).DataSymbol.StackAddr );

   end else if expr.BaseExpr is TConstExpr then begin

      absPointer := IntPtr(@TConstExpr(expr.BaseExpr).Data[0]);
      offset := cVariant_DataOffset;

(*   end else if e.BaseExpr is TByRefParamExpr then begin

         index := TConstIntExpr(e.IndexExpr).Value-e.LowBound;

         Tx86VarParam.CompileAsPVariant(x86, TByRefParamExpr(e.BaseExpr));
                                                                        *)
(*   end else if expr.BaseExpr is TFieldExpr then  begin

      jit.CompileScriptObj(TFieldExpr(expr.BaseExpr).ObjectExpr);
      // TODO object check
      x86._mov_reg_qword_ptr_reg(gprRAX, gprRAX, vmt_ScriptObjInstance_IScriptObj_To_FData);
      offsetBase := TFieldExpr(e.BaseExpr).FieldSym.Offset*SizeOf(Variant);
      *)

   end else Exit(False);

   if expr.IndexExpr is TConstIntExpr then begin

      Inc(offset, (TConstIntExpr(expr.IndexExpr).Value - expr.LowBound) * SizeOf(Variant));
      if absPointer <> 0 then begin
         x86._mov_reg_imm(gprRAX, absPointer + offset);
         ptrReg := gprRAX;
         offset := 0;
      end;

   end else begin

      var reg := jit.CompileIntegerToRegister(expr.IndexExpr);
      x86._imul_reg_reg_imm(gprRAX, reg, SizeOf(Variant));
      jit.ReleaseGPReg(reg);

      offset := offset - expr.LowBound*SizeOf(Variant);

      if absPointer = 0 then begin
         x86._add_reg_reg(gprRAX, ptrReg);
      end else begin
         jit.Fixups.NewGPOpRegImm(gpOp_add, gprRAX, absPointer + offset);
         offset := 0;
      end;
      ptrReg := gprRAX;

   end;

   Result := True;
end;

// DoCompileFloat
//
function Tx86StaticArray.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
var
   e : TStaticArrayExpr;
   offset : Integer;
   regPtr : TgpRegister64;
begin
   e := TStaticArrayExpr(expr);

   if CompileToItemPtr(e, regPtr, offset) then begin

      Result := jit.AllocXMMReg(expr);
      if jit.IsFloat(e) then
         x86._movsd_reg_qword_ptr_reg(Result, regPtr, offset)
      else begin
         Assert(jit.IsInteger(e));
         x86._mov_reg_qword_ptr_reg(gprRAX, regPtr, offset);
         x86._cvtsi2sd(Result, gprRAX);
      end;

   end else Exit(inherited);
end;

// DoCompileInteger
//
function Tx86StaticArray.DoCompileInteger(expr : TTypedExpr) : TgpRegister64;
var
   e : TStaticArrayExpr;
   offset : Integer;
   regPtr : TgpRegister64;
begin
   e := TStaticArrayExpr(expr);

   if CompileToItemPtr(e, regPtr, offset) then begin

      Result := jit.AllocGPReg(expr);
      x86._mov_reg_qword_ptr_reg(Result, regPtr, offset);

   end else Exit(inherited);
end;

// DoCompileAssignFloat
//
procedure Tx86StaticArray.DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister);
var
   e : TStaticArrayExpr;
   offset : Integer;
   regPtr : TgpRegister64;
begin
   e:=TStaticArrayExpr(expr);

   if jit.IsFloat(e) then begin

      if CompileToItemPtr(e, regPtr, offset) then begin

         x86._movsd_qword_ptr_reg_reg(regPtr, offset, source);

      end else inherited;

   end else inherited;
end;
{
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
}

// ------------------
// ------------------ Tx86DynamicArrayBase ------------------
// ------------------

// CompileAsData
//
function Tx86DynamicArrayBase.CompileAsData(expr : TTypedExpr; elementType : TVarType) : TgpRegister64;
var
   regDyn : TgpRegister64;
begin
   regDyn := jit.CompileScriptDynArray(expr);
   jit.ReleaseGPReg(regDyn);
   Result := jit.AllocGPReg(nil);
   case elementType of
      varDouble :
         x86._mov_reg_qword_ptr_reg(Result, regDyn, vmt_ScriptDynamicFloatArray_IScriptDynArray_To_DataPointer);
      varInt64 :
         x86._mov_reg_qword_ptr_reg(Result, regDyn, vmt_ScriptDynamicIntegerArray_IScriptDynArray_To_DataPointer);
      varUnknown :
         x86._mov_reg_qword_ptr_reg(Result, regDyn, vmt_ScriptDynamicInterfaceArray_IScriptDynArray_To_DataPointer);
   else
      Assert(False);
   end;
end;

// CompileAsItemPtr
//
function Tx86DynamicArrayBase.CompileAsItemPtr(base, index : TTypedExpr; var offset : Integer; elementType : TVarType) : TgpRegister64;
var
   indexClass : TClass;
   elementSize : Integer;
begin
   case elementType of
      varDouble  : elementSize := SizeOf(Double);
      varInt64   : elementSize := SizeOf(Int64);
      varUnknown : elementSize := SizeOf(IUnknown);
   else
      elementSize := 0;
      Assert(False);
   end;

   indexClass := index.ClassType;
   if indexClass = TAddIntExpr then begin
      if TAddIntExpr(index).Right.ClassType = TConstIntExpr then begin
         offset := offset + TConstIntExpr(TAddIntExpr(index).Right).Value * elementSize;
         Result := CompileAsItemPtr(base, TAddIntExpr(index).Left, offset, elementType);
         Exit;
      end;
   end else if indexClass = TSubIntExpr then begin
      if TSubIntExpr(index).Right.ClassType = TConstIntExpr then begin
         offset := offset - TConstIntExpr(TSubIntExpr(index).Right).Value * elementSize;
         Result := CompileAsItemPtr(base, TSubIntExpr(index).Left, offset, elementType);
         Exit;
      end;
   end;

   Result := CompileAsData(base, elementType);

   if index is TConstIntExpr then begin

      x86._add_reg_imm(Result, TConstIntExpr(index).Value * elementSize);

   end else begin

      var regIdx := jit.CompileIntegerToRegister(index);
      if (elementSize in [ 1, 2, 4, 8 ]) and not (regIdx in [ gprRSP, gprR12 ]) then begin
         x86._lea_reg_ptr_indexed_reg(Result, Result, regIdx, elementSize, offset);
         jit.ReleaseGPReg(regIdx);
         offset := 0;
      end else begin
         x86._imul_reg_reg_imm(gprRAX, regIdx, elementSize);
         jit.ReleaseGPReg(regIdx);
         x86._add_reg_reg(Result, gprRAX);
      end;

   end;
end;

// ------------------
// ------------------ Tx86DynamicArray ------------------
// ------------------

// DoCompileFloat
//
function Tx86DynamicArray.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
var
   e : TDynamicArrayExpr;
   regPtr : TgpRegister64;
   offset : Integer;
begin
   e := TDynamicArrayExpr(expr);

   if jit.IsFloat(e) then begin

      offset := 0;
      regPtr := CompileAsItemPtr(e.BaseExpr, e.IndexExpr, offset, varDouble);

      Result := jit.AllocXMMReg(e);
      x86._movsd_reg_qword_ptr_reg(Result, regPtr, offset);

      jit.ReleaseGPReg(regPtr);

   end else Result:=inherited;
end;


// DoCompileInteger
//
function Tx86DynamicArray.DoCompileInteger(expr : TTypedExpr) : TgpRegister64;
var
   e : TDynamicArrayExpr;
   regPtr : TgpRegister64;
   offset : Integer;
begin
   e := TDynamicArrayExpr(expr);

   if jit.IsInteger(e) then begin

      offset := 0;
      regPtr := CompileAsItemPtr(e.BaseExpr, e.IndexExpr, offset, varInt64);

      Result := jit.AllocGPReg(e);
      x86._mov_reg_qword_ptr_reg(Result, regPtr, offset);

      jit.ReleaseGPReg(regPtr);

   end else Result := inherited;
end;

// DoCompileScriptObj
//
function Tx86DynamicArray.DoCompileScriptObj(expr : TTypedExpr) : TgpRegister64;
var
   e : TDynamicArrayExpr;
   regPtr : TgpRegister64;
   offset : Integer;
begin
   e := TDynamicArrayExpr(expr);

   if jit.IsDynamicArray(e) then begin

      offset := 0;
      regPtr := CompileAsItemPtr(e.BaseExpr, e.IndexExpr, offset, varUnknown);

      Result := jit.AllocGPReg(e);
      x86._mov_reg_qword_ptr_reg(Result, regPtr, offset);

      jit.ReleaseGPReg(regPtr);

   end else Result := inherited;
end;

// ------------------
// ------------------ Tx86DynamicArraySet ------------------
// ------------------

// CompileStatement
//
procedure Tx86DynamicArraySet.CompileStatement(expr : TExprBase);
var
   e : TDynamicArraySetExpr;
   regValueFloat : TxmmRegister;
   regValueGP : TgpRegister64;
   regPtr : TgpRegister64;
   offset : Integer;
   elementTypeClass : TClass;
begin
   e:=TDynamicArraySetExpr(expr);

   elementTypeClass := e.ArrayExpr.Typ.Typ.UnAliasedType.ClassType;

   if elementTypeClass = TBaseIntegerSymbol then begin

      offset := 0;
      regPtr := CompileAsItemPtr(e.ArrayExpr, e.IndexExpr, offset, varInt64);
      regValueGP := jit.CompileIntegerToRegister(e.ValueExpr);
      x86._mov_qword_ptr_reg_reg(regPtr, offset, regValueGP);
      jit.ReleaseGPReg(regValueGP);
      jit.ReleaseGPReg(regPtr);

   end else if elementTypeClass = TBaseFloatSymbol then begin

      offset := 0;
      regPtr := CompileAsItemPtr(e.ArrayExpr, e.IndexExpr, offset, varDouble);
      regValueFloat := jit.CompileFloat(e.ValueExpr);
      x86._movsd_qword_ptr_reg_reg(regPtr, offset, regValueFloat);
      jit.ReleaseXMMReg(regValueFloat);
      jit.ReleaseGPReg(regPtr);

   end else inherited;
end;

// ------------------
// ------------------ Tx86NegInt ------------------
// ------------------

// DoCompileInteger
//
function Tx86NegInt.DoCompileInteger(expr : TTypedExpr) : TgpRegister64;
var
   reg : TgpRegister64;
begin
   reg := jit.CompileIntegerToRegister(TNegIntExpr(expr).Expr);

   Result := jit.AllocGPReg(expr);
   x86._mov_reg_reg(Result, reg);
   jit.ReleaseGPReg(reg);
   x86._neg_reg(Result);
end;

// ------------------
// ------------------ Tx86NotInt ------------------
// ------------------

// DoCompileInteger
//
function Tx86NotInt.DoCompileInteger(expr : TTypedExpr) : TgpRegister64;
var
   reg : TgpRegister64;
begin
   reg := jit.CompileIntegerToRegister(TNegIntExpr(expr).Expr);

   Result := jit.AllocGPReg(expr);
   x86._mov_reg_reg(Result, reg);
   jit.ReleaseGPReg(reg);
   x86._not_reg(Result);
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

// DoCompileInteger
//
function Tx86IntegerBinOpExpr.DoCompileInteger(expr : TTypedExpr) : TgpRegister64;

   function CompileConstantOperand(expr, operand : TTypedExpr; const val : Int64) : TgpRegister64;
   var
      reg : TgpRegister64;
   begin
      Result := jit.AllocGPReg(expr);
      reg := jit.CompileIntegerToRegister(operand);
      x86._mov_reg_reg(Result, reg);
      jit._op_reg_imm(FOp, Result, val);
      jit.ReleaseGPReg(reg);
   end;

var
   e : TIntegerBinOpExpr;
   leftReg, rightReg : TgpRegister64;
begin
   e:=TIntegerBinOpExpr(expr);

   if e.Right is TConstIntExpr then

      Result := CompileConstantOperand(expr, e.Left, TConstIntExpr(e.Right).Value)

   else if FCommutative and (e.Left is TConstIntExpr) then

      Result := CompileConstantOperand(expr, e.Right, TConstIntExpr(e.Left).Value)

   else begin

      leftReg := jit.CompileIntegerToRegister(e.Left);
      Result := jit.AllocGPReg(expr);
      x86._mov_reg_reg(Result, leftReg);
      jit.ReleaseGPReg(leftReg);

      rightReg := jit.CompileIntegerToRegister(e.Right);
      x86._op_reg_reg(FOp, Result, rightReg);
      jit.ReleaseGPReg(rightReg);

   end;
end;

// ------------------
// ------------------ Tx86MultInt ------------------
// ------------------

// DoCompileInteger
//
function Tx86MultInt.DoCompileInteger(expr : TTypedExpr) : TgpRegister64;

   function CompileMultByConstant(destReg : TgpRegister64; operand : TTypedExpr; value : Int64) : Boolean;
   var
      reg : TgpRegister64;
   begin
      Result := (Int32(value) = value);
      if Result then begin
         reg := jit.CompileIntegerToRegister(operand);
         x86._imul_reg_reg_imm(destReg, reg, value);
         jit.ReleaseGPReg(reg);
      end;
   end;

var
   e : TMultIntExpr;
   leftReg, rightReg : TgpRegister64;
begin
   e := TMultIntExpr(expr);

   Result := jit.AllocGPReg(expr);
   if e.Right is TConstIntExpr then begin
      if CompileMultByConstant(Result, e.Left, TConstIntExpr(e.Right).Value) then Exit;
   end else if e.Left is TConstIntExpr then begin
      if CompileMultByConstant(Result, e.Right, TConstIntExpr(e.Left).Value) then Exit;
   end;

   leftReg := jit.CompileIntegerToRegister(e.Left);
   x86._mov_reg_reg(Result, leftReg);
   jit.ReleaseGPReg(leftReg);

   rightReg := jit.CompileIntegerToRegister(e.Right);
   x86._imul_reg_reg(Result, rightReg);
   jit.ReleaseGPReg(rightReg);
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
// ------------------ Tx86Shift ------------------
// ------------------

// Create
//
constructor Tx86Shift.Create(jit : TdwsJITx86_64; const shiftOp : TgpShift);
begin
   inherited Create(jit);
   FShiftOp := shiftOp;
end;

// DoCompileInteger
//
function Tx86Shift.DoCompileInteger(expr : TTypedExpr) : TgpRegister64;
var
   e : TShiftExpr;
begin
   e := TShiftExpr(expr);

   if e.Right is TConstIntExpr then begin

      Result := jit.CompileIntegerToRegister(e.Left);
      x86._shift_reg_imm(FShiftOp, Result, TConstIntExpr(e.Right).Value);

   end else Result := inherited;
end;

// ------------------
// ------------------ Tx86Inc ------------------
// ------------------

// DoCompileStatement
//
procedure Tx86Inc.DoCompileStatement(v : TIntVarExpr; i : TTypedExpr);
var
   reg, operand : TgpRegister64;
begin
   reg := jit.CompileIntegerToRegister(v);
   if (i is TConstIntExpr) and TConstIntExpr(i).ValueIsInt32 then
      x86._add_reg_imm(reg, TConstIntExpr(i).Value)
   else begin
      operand := jit.CompileIntegerToRegister(i);
      x86._add_reg_reg(reg, operand);
      jit.ReleaseGPReg(operand);
   end;
   x86._mov_execmem_reg(v.StackAddr, reg);
   jit.ReleaseGPReg(reg);
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

// ------------------
// ------------------ Tx86IncVarFunc ------------------
// ------------------

// CompileStatement
//
procedure Tx86IncVarFunc.CompileStatement(expr : TExprBase);
var
   e : TIncVarFuncExpr;
begin
   e := TIncVarFuncExpr(expr);

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
var
   reg, operand : TgpRegister64;
begin
   reg := jit.CompileIntegerToRegister(v);
   if (i is TConstIntExpr) and TConstIntExpr(i).ValueIsInt32 then
      x86._sub_reg_imm(reg, TConstIntExpr(i).Value)
   else begin
      operand := jit.CompileIntegerToRegister(i);
      x86._sub_reg_reg(reg, operand);
      jit.ReleaseGPReg(operand);
   end;
   x86._mov_execmem_reg(v.StackAddr, reg);
   jit.ReleaseGPReg(reg);
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
   e := TDecIntVarExpr(expr);
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
   e := TDecVarFuncExpr(expr);

   if e.Args[0] is TIntVarExpr then
      DoCompileStatement(TIntVarExpr(e.Args[0]), e.Args[1] as TTypedExpr)
   else inherited;
end;

// ------------------
// ------------------ Tx86RelOpInt ------------------
// ------------------

// Create
//
constructor Tx86RelOpInt.Create(jit : TdwsJITx86_64; flagsPass : TboolFlags);
begin
   inherited Create(jit);
   Self.FlagsPass := flagsPass;
end;

// DoCompileBoolean
//
procedure Tx86RelOpInt.DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
var
   e : TIntegerRelOpExpr;
//   addr : Integer;
   leftReg, rightReg : TgpRegister64;
begin
   e := TIntegerRelOpExpr(expr);

   leftReg := jit.CompileIntegerToRegister(e.Left);

   if e.Right is TConstIntExpr then begin

      jit._cmp_reg_imm(leftReg, TConstIntExpr(e.Right).Value);

   end {else if e.Right is TIntVarExpr then begin

      addr := TIntVarExpr(e.Right).StackAddr;

      x86._cmp_reg_execmem(leftReg, addr);
      jit.Fixups.NewConditionalJumps(FlagsPass, targetTrue, targetFalse);

   end} else begin

      rightReg := jit.CompileIntegerToRegister(e.Right);
      x86._cmp_reg_reg(leftReg, rightReg);
      jit.ReleaseGPReg(rightReg);

   end;
   jit.ReleaseGPReg(leftReg);

   jit.Fixups.NewConditionalJumps(FlagsPass, targetTrue, targetFalse);
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
   reg : TgpRegister64;
begin
   e := TUnaryOpBoolExpr(expr);

   reg := jit.CompileIntegerToRegister(e.Expr);

   x86._cmp_reg_imm(reg, 0);

   jit.Fixups.NewConditionalJumps(flagsZ, targetTrue, targetFalse);

   jit.ReleaseGPReg(reg);
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

   regLeft := jit.CompileFloat(e.Left);

   jit._comisd_reg_expr(regLeft, e.Right);

   jit.ReleaseXMMReg(regLeft);

   jit.Fixups.NewConditionalJumps(Flags, targetTrue, targetFalse);
end;

// ------------------
// ------------------ Tx86NotExpr ------------------
// ------------------

// DoCompileBoolean
//
procedure Tx86NotExpr.DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
var
   e : TNotBoolExpr;
begin
   e := TNotBoolExpr(expr);

   jit.CompileBoolean(e.Expr, targetFalse, targetTrue);
end;

// ------------------
// ------------------ Tx86BoolOrExpr ------------------
// ------------------

// DoCompileBoolean
//
procedure Tx86BoolOrExpr.DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
var
   e : TBooleanBinOpExpr;
begin
   e := TBooleanBinOpExpr(expr);

   jit.CompileBoolean(e.Left, targetTrue, nil);
   jit.CompileBoolean(e.Right, targetTrue, targetFalse);
end;

// ------------------
// ------------------ Tx86BoolAndExpr ------------------
// ------------------

// JumpSafeCompile
//
procedure Tx86BoolAndExpr.DoCompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
var
   e : TBooleanBinOpExpr;
begin
   e := TBooleanBinOpExpr(expr);

   jit.CompileBoolean(e.Left, nil, targetFalse);
   jit.CompileBoolean(e.Right, targetTrue, targetFalse);
end;
{
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


// DoCompileInteger
//
function Tx86MagicFunc.DoCompileInteger(expr : TTypedExpr) : TgpRegister64;
var
   jitter : TdwsJITter_x86;
   e : TMagicIntFuncExpr;
begin
   e:=(expr as TMagicIntFuncExpr);

   jitter := TdwsJITter_x86(jit.FindJITter(TMagicFuncSymbol(e.FuncSym).InternalFunction.ClassType));
   if jitter <> nil then

      Result := jitter.DoCompileInteger(expr)

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

// DoCompileInteger
//
function Tx86MagicBoolFunc.DoCompileInteger(expr : TTypedExpr) : TgpRegister64;
begin
   CompileBooleanValue(expr);
   Result := jit.AllocGPReg(expr);
   x86._mov_reg_reg(Result, gprRAX);
end;

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
   i : Integer;
   p : TParamSymbol;
   paramReg : array of TxmmRegister;
begin
   Result:=False;

   Assert(funcSym.Params.Count <= 2); // TODO support different number of parameters

   SetLength(paramReg, funcSym.Params.Count);

   for i:=0 to funcSym.Params.Count-1 do begin
      p:=funcSym.Params[i];
      if jit.IsFloat(p.Typ) then begin
         paramReg[i] := jit.CompileFloat(args[i] as TTypedExpr);
      end else Exit;
   end;

   for i:=0 to High(paramReg) do begin
      if paramReg[i] <> xmmNone then begin
         x86._movsd_reg_reg(TxmmRegister(i), paramReg[i]);
         jit.ReleaseXMMReg(paramReg[i]);
      end;
   end;

   jit.SaveRegisters;

   x86._call_absmem(addrPtr);

   jit.RestoreRegisters;

   Result := True;
end;

// DoCompileFloat
//
function Tx86DirectCallFunc.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
var
   e : TMagicFuncExpr;
begin
   e:=TMagicFuncExpr(expr);

   if not CompileCall(e.FuncSym, e.Args) then begin

      jit.OutputFailedOn := expr;
      Result := xmm0;

   end else if jit.IsFloat(e.FuncSym.Typ) then begin

      Result := jit.AllocXMMReg(expr);
      x86._movsd_reg_reg(Result, xmm0);

   end else begin

      jit.OutputFailedOn := expr;
      Result := xmm0;

   end;
end;

// DoCompileInteger
//
function Tx86DirectCallFunc.DoCompileInteger(expr : TTypedExpr) : TgpRegister64;
var
   e : TMagicFuncExpr;
begin
   e:=TMagicFuncExpr(expr);

   if not CompileCall(e.FuncSym, e.Args) then
      jit.OutputFailedOn:=expr;

   Result := jit.AllocGPReg(expr);
   x86._mov_reg_reg(Result, gprRAX);
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

   x86._op_reg_imm(gpOp_and, gprRAX, 1);
   Result:=0;
end;

// ------------------
// ------------------ Tx86AbsIntFunc ------------------
// ------------------

// DoCompileInteger
//
function Tx86AbsIntFunc.DoCompileInteger(expr : TTypedExpr) : TgpRegister64;
var
   jump : TFixupJump;
   opReg : TgpRegister64;
begin
   opReg := jit.CompileIntegerToRegister(TMagicFuncExpr(expr).Args[0] as TTypedExpr);

   x86._test_reg_reg(opReg, opReg);

   if jit.IsSymbolGPReg(opReg) then begin

      Result := jit.AllocGPReg(expr);
      x86._mov_reg_reg(Result, opReg);
      jit.ReleaseGPReg(opReg);

   end else begin

      Result := opReg;
      jit.SetContainsGPReg(Result, expr);

   end;
   jump := jit.Fixups.NewJump(flagsNL);

   x86._neg_reg(Result);

   jump.NewTarget(False);
end;

// ------------------
// ------------------ Tx86AbsFloatFunc ------------------
// ------------------

// DoCompileFloat
//
function Tx86AbsFloatFunc.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
const
   cAbsMask : array [0..SizeOf(Double)-1] of Byte = ( $FF, $FF, $FF, $FF, $FF, $FF, $FF, $7F );
var
   opReg : TxmmRegister;
begin
   opReg := jit.CompileFloat(TMagicFuncExpr(expr).Args[0] as TTypedExpr);

   if jit.IsSymbolXMMReg(opReg) then begin

      Result := jit.AllocXMMReg(expr);
      x86._movsd_reg_reg(Result, opReg);
      jit.ReleaseXMMReg(opReg);

   end else begin

      Result := opReg;
      jit.SetContainsXMMReg(Result, expr);

   end;

   // andpd Result, dqword ptr [AbsMask]
   jit.Fixups.NewXMMOpRegPDImm(xmm_andpd, Result, PDouble(@cAbsMask)^, PDouble(@cAbsMask)^);
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
}
// ------------------
// ------------------ Tx86RoundFunc ------------------
// ------------------

// DoCompileInteger
//
function Tx86RoundFunc.DoCompileInteger(expr : TTypedExpr) : TgpRegister64;
var
   reg : TxmmRegister;
begin
   reg := jit.CompileFloat(TMagicFuncExpr(expr).Args[0] as TTypedExpr);

   Result := jit.AllocGPReg(expr);

   x86._cvtsd2si(Result, reg);

   jit.ReleaseXMMReg(reg);
end;

// DoCompileFloat
//
function Tx86RoundFunc.DoCompileFloat(expr : TTypedExpr) : TxmmRegister;
var
   regArg : TxmmRegister;
begin
   regArg := jit.CompileFloat(TMagicFuncExpr(expr).Args[0] as TTypedExpr);

   x86._cvtsd2si(gprRAX, regArg);

   Result := jit.AllocXMMReg(expr);

   x86._cvtsi2sd(Result, gprRAX);

   jit.ReleaseXMMReg(regArg);
end;

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

// ------------------
// ------------------ Tx86Unsigned32Func ------------------
// ------------------

// DoCompileInteger
//
function Tx86Unsigned32Func.DoCompileInteger(expr : TTypedExpr) : TgpRegister64;
var
   reg : TgpRegister64;
begin
   reg := jit.CompileIntegerToRegister(TMagicFuncExpr(expr).Args[0] as TTypedExpr);

   Result := jit.AllocGPReg(expr);

   x86._mov_reg32_reg32(Result, reg);

   jit.ReleaseGPReg(reg);
end;

// ------------------
// ------------------ Tx86ClampIntFunc ------------------
// ------------------

// DoCompileInteger
//
function Tx86ClampIntFunc.DoCompileInteger(expr : TTypedExpr) : TgpRegister64;
var
   e : TMagicFuncExpr;
   minExpr, maxExpr : TTypedExpr;
   doneTarget : TFixupTarget;
   reg : TgpRegister64;
begin
   e := TMagicFuncExpr(expr);

   minExpr := e.Args[1] as TTypedExpr;
   maxExpr := e.Args[2] as TTypedExpr;
   if (minExpr is TConstIntExpr) and (maxExpr is TConstIntExpr) then begin

      doneTarget := jit.Fixups.NewHangingTarget(False);

      reg := jit.CompileIntegerToRegister(e.Args[0] as TTypedExpr);
      Result := jit.AllocGPReg(expr);
      x86._mov_reg_reg(Result, reg);
      jit.ReleaseGPReg(reg);

      x86._mov_reg_imm(gprRAX, TConstIntExpr(minExpr).Value);
      x86._cmp_reg_reg(Result, gprRAX);
      x86._cmov(flagsL, Result, gprRAX);
      jit.Fixups.NewJump(flagsLE, doneTarget);

      x86._mov_reg_imm(gprRAX, TConstIntExpr(maxExpr).Value);
      x86._cmp_reg_reg(Result, gprRAX);
      x86._cmov(flagsG, Result, gprRAX);

      jit.Fixups.AddFixup(doneTarget);

   end else Result := inherited;
end;

{$else}
implementation
{$endif}

end.
