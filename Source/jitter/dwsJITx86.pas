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
unit dwsJITx86;

{$I ../dws.inc}

interface

{
   TODO:
   - boolean jumps in addition to flags (to be validated)
   - stack allocations
   - call stub for functions
   - var params sopport
}


uses
   Classes, SysUtils, Math,
   dwsExprs, dwsSymbols, dwsErrors, dwsUtils, dwsCoreExprs, dwsRelExprs, dwsMagicExprs,
   dwsMathFunctions, dwsDataContext,
   dwsJIT, dwsJITFixups, dwsJITAllocatorWin, dwsJITx86Intrinsics, dwsVMTOffsets;

type

   TRegisterStatus = record
      Contains : TObject;
      Lock : Integer;
   end;

   TFixupJump = class(TFixupTargeting)
      private
         FFlags : TboolFlags;

      protected
         function NearJump : Boolean;

      public
         constructor Create(flags : TboolFlags);

         function  GetSize : Integer; override;
         procedure Write(output : TWriteOnlyBlockStream); override;
   end;

   TFixupPreamble = class(TFixup)
      private
         FPreserveExecInEDI : Boolean;
         FReserve16OnStack : Boolean;

      public
         function  GetSize : Integer; override;
         procedure Write(output : TWriteOnlyBlockStream); override;

         property PreserveExecInEDI : Boolean read FPreserveExecInEDI write FPreserveExecInEDI;
         property Reserve16OnStack : Boolean read FReserve16OnStack write FReserve16OnStack;
   end;

   TFixupPostamble = class(TFixup)
      private
         FPreamble : TFixupPreamble;

      public
         constructor Create(preamble : TFixupPreamble);

         function  GetSize : Integer; override;
         procedure Write(output : TWriteOnlyBlockStream); override;
   end;


   Tx86FixupLogicHelper = class helper for TFixupLogic
      function NewJump(flags : TboolFlags) : TFixupJump; overload;
      function NewJump(flags : TboolFlags; target : TFixup) : TFixupJump; overload;
      procedure NewConditionalJumps(flagsTrue : TboolFlags; targetTrue, targetFalse : TFixup);
   end;

   TdwsJITx86 = class (TdwsJIT)
      private
         FRegs : array [TxmmRegister] of TRegisterStatus;
         FXMMIter : TxmmRegister;
         FSavedXMM : TxmmRegisters;

         FPreamble : TFixupPreamble;
         FPostamble : TFixupPostamble;
         x86 : Tx86WriteOnlyStream;

         FAllocator : TdwsJITAllocatorWin;

         FAbsMaskPD : TdwsJITCodeBlock;
         FSignMaskPD : TdwsJITCodeBlock;
         FBufferBlock : TdwsJITCodeBlock;

      protected
         function CreateOutput : TWriteOnlyBlockStream; override;

         procedure StartJIT(expr : TExprBase; exitable : Boolean); override;
         procedure EndJIT; override;
         procedure EndFloatJIT(resultHandle : Integer); override;
         procedure EndIntegerJIT(resultHandle : Integer); override;

      public
         constructor Create; override;
         destructor Destroy; override;

         function  AllocXMMReg(expr : TExprBase; contains : TObject = nil) : TxmmRegister;
         procedure ReleaseXMMReg(reg : TxmmRegister);
         function  CurrentXMMReg(contains : TObject) : TxmmRegister;
         procedure ContainsXMMReg(reg : TxmmRegister; contains : TObject);
         procedure ResetXMMReg;
         procedure SaveXMMRegs;
         procedure RestoreXMMRegs;

         function StackAddrOfFloat(expr : TTypedExpr) : Integer;

         property Allocator : TdwsJITAllocatorWin read FAllocator write FAllocator;
         property AbsMaskPD : Pointer read FAbsMaskPD.Code;
         property SignMaskPD : Pointer read FSignMaskPD.Code;

         function CompileFloat(expr : TTypedExpr) : TxmmRegister; inline;
         procedure CompileAssignFloat(expr : TTypedExpr; source : TxmmRegister); inline;

         procedure CompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);

         function CompiledOutput : TdwsJITCodeBlock; override;

         procedure _xmm_reg_expr(op : TxmmOp; dest : TxmmRegister; expr : TTypedExpr);
         procedure _comisd_reg_expr(dest : TxmmRegister; expr : TTypedExpr);

         procedure _DoStep(expr : TExprBase);
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
         FJIT : TdwsJITx86;
         Fx86 : Tx86WriteOnlyStream;

      protected
         property jit : TdwsJITx86 read FJIT;
         property x86 : Tx86WriteOnlyStream read Fx86;

      public
         constructor Create(jit : TdwsJITx86);

         function CompileFloat(expr : TExprBase) : Integer; override; final;
         function DoCompileFloat(expr : TExprBase) : TxmmRegister; virtual;

         procedure CompileAssignFloat(expr : TTypedExpr; source : Integer); override; final;
         procedure DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister); virtual;

         procedure CompileBoolean(expr : TExprBase; targetTrue, targetFalse : TFixup); override; final;
         procedure DoCompileBoolean(expr : TExprBase; targetTrue, targetFalse : TFixup); virtual;
   end;

   Tx86ConstFloat = class (TdwsJITter_x86)
      function DoCompileFloat(expr : TExprBase) : TxmmRegister; override;
   end;
   Tx86ConstInt = class (TdwsJITter_x86)
      function DoCompileFloat(expr : TExprBase) : TxmmRegister; override;
      function CompileInteger(expr : TExprBase) : Integer; override;
   end;

   Tx86FloatVar = class (TdwsJITter_x86)
      function DoCompileFloat(expr : TExprBase) : TxmmRegister; override;
      procedure DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister); override;
   end;
   Tx86IntVar = class (TdwsJITter_x86)
      function DoCompileFloat(expr : TExprBase) : TxmmRegister; override;
      function CompileInteger(expr : TExprBase) : Integer; override;
   end;
   Tx86RecordVar = class (TdwsJITter_x86)
      function DoCompileFloat(expr : TExprBase) : TxmmRegister; override;
      procedure DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister); override;
   end;
   Tx86VarParam = class (TdwsJITter_x86)
      class procedure CompileAsPVariant(x86 : Tx86WriteOnlyStream; expr : TVarParamExpr);
      function DoCompileFloat(expr : TExprBase) : TxmmRegister; override;
      procedure DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister); override;
   end;

   Tx86StaticArray = class (TdwsJITter_x86)
      function DoCompileFloat(expr : TExprBase) : TxmmRegister; override;
      procedure DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister); override;
   end;

   Tx86InterpretedExpr = class (TdwsJITter_x86)
      procedure DoCallEval(expr : TExprBase; vmt : Integer);
      procedure CompileStatement(expr : TExprBase); override;
      function DoCompileFloat(expr : TExprBase) : TxmmRegister; override;
      function CompileInteger(expr : TExprBase) : Integer; override;
   end;

   Tx86AssignConstToFloatVar = class (Tx86InterpretedExpr)
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

   Tx86OpAssignFloat = class (TdwsJITter_x86)
      public
         OP : TxmmOp;
         constructor Create(jit : TdwsJITx86; op : TxmmOp);
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
      procedure CompileStartCondition(expr : TLoopExpr; targetLoop, targetExit : TFixupTarget); virtual;
      procedure CompileFinalCondition(expr : TLoopExpr; targetLoop, targetExit : TFixupTarget); virtual;
   end;
   Tx86Repeat = class (Tx86Loop)
      procedure CompileFinalCondition(expr : TLoopExpr; targetLoop, targetExit : TFixupTarget); override;
   end;
   Tx86While = class (Tx86Loop)
      procedure CompileStartCondition(expr : TLoopExpr; targetLoop, targetExit : TFixupTarget); override;
   end;

   Tx86ForUpward = class (TdwsJITter_x86)
      procedure CompileStatement(expr : TExprBase); override;
   end;

   Tx86Continue = class (TdwsJITter_x86)
      procedure CompileStatement(expr : TExprBase); override;
   end;
   Tx86Exit = class (TdwsJITter_x86)
      procedure CompileStatement(expr : TExprBase); override;
   end;

   Tx86FloatBinOp = class (TdwsJITter_x86)
      public
         OP : TxmmOp;
         constructor Create(jit : TdwsJITx86; op : TxmmOp);
         function DoCompileFloat(expr : TExprBase) : TxmmRegister; override;
   end;
   Tx86SqrFloat = class (TdwsJITter_x86)
      function DoCompileFloat(expr : TExprBase) : TxmmRegister; override;
   end;
   Tx86AbsFloat = class (TdwsJITter_x86)
      function DoCompileFloat(expr : TExprBase) : TxmmRegister; override;
   end;
   Tx86NegFloat = class (TdwsJITter_x86)
      function DoCompileFloat(expr : TExprBase) : TxmmRegister; override;
   end;

   Tx86AddInt = class (TdwsJITter_x86)
      function CompileInteger(expr : TExprBase) : Integer; override;
   end;

   Tx86IncIntVar = class (TdwsJITter_x86)
      procedure CompileStatement(expr : TExprBase); override;
   end;
   Tx86DecIntVar = class (TdwsJITter_x86)
      procedure CompileStatement(expr : TExprBase); override;
   end;

   Tx86RelEqualInt = class (TdwsJITter_x86)
      procedure TestEqual(addr : Integer; value : Int64; targetTrue, targetFalse : TFixup);
      procedure DoCompileBoolean(expr : TExprBase; targetTrue, targetFalse : TFixup); override;
   end;
   Tx86RelNotEqualInt = class (Tx86RelEqualInt)
      procedure DoCompileBoolean(expr : TExprBase; targetTrue, targetFalse : TFixup); override;
   end;
   Tx86RelIntIsZero = class (Tx86RelEqualInt)
      procedure DoCompileBoolean(expr : TExprBase; targetTrue, targetFalse : TFixup); override;
   end;
   Tx86RelIntIsNotZero = class (Tx86RelIntIsZero)
      procedure DoCompileBoolean(expr : TExprBase; targetTrue, targetFalse : TFixup); override;
   end;

   Tx86RelOpFloat = class (TdwsJITter_x86)
      public
         Flags : TboolFlags;
         constructor Create(jit : TdwsJITx86; flags : TboolFlags);
         procedure DoCompileBoolean(expr : TExprBase; targetTrue, targetFalse : TFixup); override;
   end;

   Tx86NotExpr = class (TdwsJITter_x86)
      procedure DoCompileBoolean(expr : TExprBase; targetTrue, targetFalse : TFixup); override; final;
   end;
   Tx86BoolOrExpr = class (TdwsJITter_x86)
      procedure DoCompileBoolean(expr : TExprBase; targetTrue, targetFalse : TFixup); override;
   end;
   Tx86BoolAndExpr = class (TdwsJITter_x86)
      procedure DoCompileBoolean(expr : TExprBase; targetTrue, targetFalse : TFixup); override;
   end;

   Tx86MagicFloatFunc = class (Tx86InterpretedExpr)
      function DoCompileFloat(expr : TExprBase) : TxmmRegister; override;
   end;

   Tx86SqrtFunc = class (Tx86MagicFloatFunc)
      function DoCompileFloat(expr : TExprBase) : TxmmRegister; override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsJITx86 ------------------
// ------------------

// Create
//
constructor TdwsJITx86.Create;
begin
   inherited;

   FAllocator:=TdwsJITAllocatorWin.Create;

   FAbsMaskPD:=FAllocator.Allocate(TBytes.Create($FF, $FF, $FF, $FF, $FF, $FF, $FF, $7F,
                                                 $FF, $FF, $FF, $FF, $FF, $FF, $FF, $7F));
   FSignMaskPD:=FAllocator.Allocate(TBytes.Create($00, $00, $00, $00, $00, $00, $00, $80,
                                                  $00, $00, $00, $00, $00, $00, $00, $80));
   FBufferBlock:=FAllocator.Allocate(TBytes.Create($66, $66, $66, $90, $66, $66, $66, $90,
                                                   $66, $66, $66, $90, $66, $66, $66, $90));

   JITTedProgramExprClass:=TProgramExpr86;
   JITTedFloatExprClass:=TFloatExpr86;
   JITTedIntegerExprClass:=TIntegerExpr86;

   RegisterJITter(TConstFloatExpr,              Tx86ConstFloat.Create(Self));
   RegisterJITter(TConstIntExpr,                Tx86ConstInt.Create(Self));

   RegisterJITter(TFloatVarExpr,                Tx86FloatVar.Create(Self));
   RegisterJITter(TIntVarExpr,                  Tx86IntVar.Create(Self));

   RegisterJITter(TRecordVarExpr,               Tx86RecordVar.Create(Self));

   RegisterJITter(TVarParamExpr,                Tx86VarParam.Create(Self));

   RegisterJITter(TStaticArrayExpr,             Tx86StaticArray.Create(Self));

   RegisterJITter(TBlockExprNoTable,            Tx86BlockExprNoTable.Create(Self));
   RegisterJITter(TBlockExprNoTable2,           Tx86BlockExprNoTable.Create(Self));
   RegisterJITter(TBlockExprNoTable3,           Tx86BlockExprNoTable.Create(Self));
   RegisterJITter(TBlockExprNoTable4,           Tx86BlockExprNoTable.Create(Self));

   RegisterJITter(TAssignConstToIntegerVarExpr, Tx86AssignConstToIntegerVar.Create(Self));
   RegisterJITter(TAssignConstToFloatVarExpr,   Tx86AssignConstToFloatVar.Create(Self));
   RegisterJITter(TAssignConstToBoolVarExpr,    Tx86AssignConstToBoolVar.Create(Self));
   RegisterJITter(TAssignExpr,                  Tx86Assign.Create(Self));

   RegisterJITter(TPlusAssignFloatExpr,         Tx86OpAssignFloat.Create(Self, xmm_addsd));
   RegisterJITter(TMinusAssignFloatExpr,        Tx86OpAssignFloat.Create(Self, xmm_subsd));
   RegisterJITter(TMultAssignFloatExpr,         Tx86OpAssignFloat.Create(Self, xmm_multsd));
   RegisterJITter(TDivideAssignExpr,            Tx86OpAssignFloat.Create(Self, xmm_divsd));

   RegisterJITter(TIfThenExpr,                  Tx86IfThen.Create(Self));
   RegisterJITter(TIfThenElseExpr,              Tx86IfThenElse.Create(Self));

   RegisterJITter(TLoopExpr,                    Tx86Loop.Create(Self));
   RegisterJITter(TRepeatExpr,                  Tx86Repeat.Create(Self));
   RegisterJITter(TWhileExpr,                   Tx86While.Create(Self));

   RegisterJITter(TForUpwardExpr,               Tx86ForUpward.Create(Self));

   RegisterJITter(TContinueExpr,                Tx86Continue.Create(Self));
   RegisterJITter(TExitExpr,                    Tx86Exit.Create(Self));

   RegisterJITter(TAddFloatExpr,                Tx86FloatBinOp.Create(Self, xmm_addsd));
   RegisterJITter(TSubFloatExpr,                Tx86FloatBinOp.Create(Self, xmm_subsd));
   RegisterJITter(TMultFloatExpr,               Tx86FloatBinOp.Create(Self, xmm_multsd));
   RegisterJITter(TSqrFloatExpr,                Tx86SqrFloat.Create(Self));
   RegisterJITter(TDivideExpr,                  Tx86FloatBinOp.Create(Self, xmm_divsd));
   RegisterJITter(TAbsFloatExpr,                Tx86AbsFloat.Create(Self));
   RegisterJITter(TNegFloatExpr,                Tx86NegFloat.Create(Self));

   RegisterJITter(TAddIntExpr,                  Tx86AddInt.Create(Self));

   RegisterJITter(TIncIntVarExpr,               Tx86IncIntVar.Create(Self));
   RegisterJITter(TDecIntVarExpr,               Tx86DecIntVar.Create(Self));

   RegisterJITter(TRelEqualIntExpr,             Tx86RelEqualInt.Create(Self));
   RegisterJITter(TRelNotEqualIntExpr,          Tx86RelNotEqualInt.Create(Self));

   RegisterJITter(TRelIntIsZeroExpr,            Tx86RelIntIsZero.Create(Self));
   RegisterJITter(TRelIntIsNotZeroExpr,         Tx86RelIntIsNotZero.Create(Self));

   RegisterJITter(TRelEqualFloatExpr,           Tx86RelOpFloat.Create(Self, flagsE));
   RegisterJITter(TRelNotEqualFloatExpr,        Tx86RelOpFloat.Create(Self, flagsNE));
   RegisterJITter(TRelGreaterFloatExpr,         Tx86RelOpFloat.Create(Self, flagsNBE));
   RegisterJITter(TRelGreaterEqualFloatExpr,    Tx86RelOpFloat.Create(Self, flagsNB));
   RegisterJITter(TRelLessFloatExpr,            Tx86RelOpFloat.Create(Self, flagsB));
   RegisterJITter(TRelLessEqualFloatExpr,       Tx86RelOpFloat.Create(Self, flagsBE));

   RegisterJITter(TNotBoolExpr,                 Tx86NotExpr.Create(Self));
   RegisterJITter(TBoolOrExpr,                  Tx86BoolOrExpr.Create(Self));
   RegisterJITter(TBoolAndExpr,                 Tx86BoolAndExpr.Create(Self));

   RegisterJITter(TFuncExpr,                    Tx86InterpretedExpr.Create(Self));
   RegisterJITter(TMagicProcedureExpr,          Tx86InterpretedExpr.Create(Self));
   RegisterJITter(TMagicFloatFuncExpr,          Tx86MagicFloatFunc.Create(Self));

   RegisterJITter(TSqrtFunc,                    Tx86SqrtFunc.Create(Self));
   RegisterJITter(TMaxFunc,                     Tx86FloatBinOp.Create(Self, xmm_maxsd));
   RegisterJITter(TMinFunc,                     Tx86FloatBinOp.Create(Self, xmm_minsd));
end;

// Destroy
//
destructor TdwsJITx86.Destroy;
begin
   inherited;
   FAllocator.Free;
   FSignMaskPD.Free;
   FAbsMaskPD.Free;
   FBufferBlock.Free;
end;

// CreateOutput
//
function TdwsJITx86.CreateOutput : TWriteOnlyBlockStream;
begin
   x86:=Tx86WriteOnlyStream.Create;
   Result:=x86;
end;

// AllocXMMReg
//
function TdwsJITx86.AllocXMMReg(expr : TExprBase; contains : TObject = nil) : TxmmRegister;
var
   i, avail : TxmmRegister;
begin
   avail:=xmmNone;
   if contains=nil then
      contains:=expr;

   for i:=xmm0 to High(TxmmRegister) do begin
      if FXMMIter=High(TxmmRegister) then
         FXMMIter:=xmm0
      else Inc(FXMMIter);
      if FRegs[FXMMIter].Contains=nil then begin
         FRegs[FXMMIter].Contains:=contains;
         FRegs[FXMMIter].Lock:=1;
         Exit(FXMMIter);
      end else if (avail=xmmNone) and (FRegs[FXMMIter].Lock=0) then
         avail:=FXMMIter;
   end;
   if avail=xmmNone then begin
      OutputFailedOn:=expr;
      Result:=xmm0;
   end else begin
      FRegs[avail].Contains:=contains;
      FRegs[avail].Lock:=1;
      FXMMIter:=avail;
      Result:=avail;
   end;
end;

// ReleaseXMMReg
//
procedure TdwsJITx86.ReleaseXMMReg(reg : TxmmRegister);
begin
   Assert(reg in [xmm0..xmm7]);

   if FRegs[reg].Lock>0 then
      Dec(FRegs[reg].Lock);
end;

// CurrentXMMReg
//
function TdwsJITx86.CurrentXMMReg(contains : TObject) : TxmmRegister;
var
   i : TxmmRegister;
begin
   for i:=xmm0 to High(TxmmRegister) do begin
      if FRegs[i].Contains=contains then begin
         Inc(FRegs[i].Lock);
         Exit(i);
      end;
   end;
   Result:=xmmNone;
end;

// ContainsXMMReg
//
procedure TdwsJITx86.ContainsXMMReg(reg : TxmmRegister; contains : TObject);
var
   i : TxmmRegister;
begin
   for i:=xmm0 to High(FRegs) do begin
      if FRegs[i].Contains=contains then begin
         if i<>reg then begin
            FRegs[i].Contains:=nil;
            FRegs[i].Lock:=0;
         end;
      end;
   end;
   FRegs[reg].Contains:=contains;
end;

// ResetXMMReg
//
procedure TdwsJITx86.ResetXMMReg;
var
   i : TxmmRegister;
begin
   FXMMIter:=High(TxmmRegister);
   for i:=xmm0 to High(FRegs) do begin
      FRegs[i].Contains:=nil;
      FRegs[i].Lock:=0;
   end;
end;

// SaveXMMRegs
//
procedure TdwsJITx86.SaveXMMRegs;
var
   i : TxmmRegister;
   n : Integer;
begin
   Assert(FSavedXMM=[]);

   n:=0;
   for i:=xmm0 to High(FRegs) do begin
      if FRegs[i].Lock>0 then begin
         Include(FSavedXMM, i);
         Inc(n);
      end;
   end;

   if n=0 then Exit;

   x86._sub_reg_int32(gprESP, n*SizeOf(Double));
   for i:=xmm0 to High(FRegs) do begin
      if i in FSavedXMM then begin
         Dec(n);
         x86._movsd_esp_reg(n*SizeOf(Double), i);
      end;
   end;
end;

// RestoreXMMRegs
//
procedure TdwsJITx86.RestoreXMMRegs;
var
   i : TxmmRegister;
   n : Integer;
begin
   if FSavedXMM=[] then Exit;

   n:=0;
   for i:=High(FRegs) downto xmm0 do begin
      if i in FSavedXMM then begin
         x86._movsd_reg_esp(i, n*SizeOf(Double));
         Inc(n);
      end;
   end;
   x86._add_reg_int32(gprESP, n*SizeOf(Double));

   FSavedXMM:=[];
end;

// StackAddrOfFloat
//
function TdwsJITx86.StackAddrOfFloat(expr : TTypedExpr) : Integer;
begin
   if (expr.ClassType=TFloatVarExpr) and (CurrentXMMReg(TFloatVarExpr(expr).DataSym)=xmmNone) then
      Result:=TFloatVarExpr(expr).StackAddr
   else Result:=-1;
end;

// CompileFloat
//
function TdwsJITx86.CompileFloat(expr : TTypedExpr) : TxmmRegister;
begin
   Result:=TxmmRegister(inherited CompileFloat(expr));
end;

// CompileAssignFloat
//
procedure TdwsJITx86.CompileAssignFloat(expr : TTypedExpr; source : TxmmRegister);
begin
   inherited CompileAssignFloat(expr, Ord(source));
end;

// CompileBoolean
//
procedure TdwsJITx86.CompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
begin
   inherited CompileBoolean(expr, targetTrue, targetFalse);
end;

// CompiledOutput
//
function TdwsJITx86.CompiledOutput : TdwsJITCodeBlock;
begin
   Fixups.FlushFixups(Output.ToBytes, Output);

   Result:=Allocator.Allocate(Output.ToBytes);
   Result.Steppable:=(jitoDoStep in Options);
end;

// StartJIT
//
procedure TdwsJITx86.StartJIT(expr : TExprBase; exitable : Boolean);
begin
   inherited;
   ResetXMMReg;
   Fixups.ClearFixups;
   FPreamble:=TFixupPreamble.Create;
   Fixups.AddFixup(FPreamble);
   FPostamble:=TFixupPostamble.Create(FPreamble);
   FPostamble.Logic:=Fixups;
end;

// EndJIT
//
procedure TdwsJITx86.EndJIT;
begin
   inherited;
   Fixups.AddFixup(FPostamble);
end;

// EndFloatJIT
//
procedure TdwsJITx86.EndFloatJIT(resultHandle : Integer);
begin
   FPreamble.Reserve16OnStack:=True;

   x86._movsd_esp_reg(TxmmRegister(resultHandle));
   x86._fld_esp;

   inherited;
end;

// EndIntegerJIT
//
procedure TdwsJITx86.EndIntegerJIT(resultHandle : Integer);
begin
   inherited;
end;

// _xmm_reg_expr
//
procedure TdwsJITx86._xmm_reg_expr(op : TxmmOp; dest : TxmmRegister; expr : TTypedExpr);
var
   addrRight : Integer;
   regRight : TxmmRegister;
begin
   if expr.ClassType=TConstFloatExpr then begin

      x86._xmm_reg_absmem(OP, dest, @TConstFloatExpr(expr).Value);

   end else begin

      addrRight:=StackAddrOfFloat(expr);
      if addrRight>=0 then begin

         x86._xmm_reg_bpmem(OP, dest, addrRight);

      end else begin

         regRight:=CompileFloat(expr);
         x86._xmm_reg_reg(OP, dest, regRight);
         ReleaseXMMReg(regRight);

      end;

   end;
end;

// _comisd_reg_expr
//
procedure TdwsJITx86._comisd_reg_expr(dest : TxmmRegister; expr : TTypedExpr);
var
   addrRight : Integer;
   regRight : TxmmRegister;
begin
   if expr.ClassType=TConstFloatExpr then begin

      x86._comisd_reg_absmem(dest, @TConstFloatExpr(expr).Value);

   end else begin

      addrRight:=StackAddrOfFloat(expr);
      if addrRight>=0 then begin

         x86._comisd_reg_bpmem(dest, addrRight);

      end else begin

         regRight:=CompileFloat(expr);
         x86._comisd_reg_reg(dest, regRight);
         ReleaseXMMReg(regRight);

      end;

   end;
end;

// _DoStep
//
var
   cPtr_TdwsExecution_DoStep : Pointer = @TdwsExecution.DoStep;
procedure TdwsJITx86._DoStep(expr : TExprBase);
begin
   if jitoDoStep in Options then begin

      FPreamble.PreserveExecInEDI:=True;

      x86._mov_reg_reg(gprEAX, gprEDI);
      x86._mov_reg_dword(gprEDX, DWORD(expr));

      // call [cPtr_TdwsExecution_DoStep]
      x86.WriteBytes([$FF, $15]);
      x86.WritePointer(@cPtr_TdwsExecution_DoStep);

      ResetXMMReg;

   end;
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

// GetSize
//
function TFixupJump.GetSize : Integer;
begin
   if (Next=Target) and (Location=Next.Location) then
      Result:=0
   else if NearJump then
      Result:=2
   else if FFlags=flagsNone then
      Result:=5
   else Result:=6;
end;

// NearJump
//
function TFixupJump.NearJump : Boolean;
begin
   Result:=(Abs(FixedLocation-Target.FixedLocation)<120);
end;

// Write
//
procedure TFixupJump.Write(output : TWriteOnlyBlockStream);
var
   offset : Integer;
begin
   if (Next=Target) and (Location=Next.Location) then
      Exit;

   offset:=Target.FixedLocation-FixedLocation;
   if NearJump then begin

      if FFlags=flagsNone then
         output.WriteByte($EB)
      else output.WriteByte(Ord(FFlags));
      output.WriteByte(offset-2);

   end else begin

      if FFlags=flagsNone then begin
         output.WriteByte($E9);
         output.WriteInt32(offset-5);
      end else begin
         output.WriteByte($0F);
         output.WriteByte(Ord(FFlags)+$10);
         output.WriteInt32(offset-6);
      end;

   end;
end;

// ------------------
// ------------------ TFixupPreamble ------------------
// ------------------

// GetSize
//
function TFixupPreamble.GetSize : Integer;
begin
   Result:=1;
   if FPreserveExecInEDI then
      Inc(Result, 3);
   Inc(Result, 3);
   if FReserve16OnStack then
      Inc(Result, 3);
end;

// Write
//
procedure TFixupPreamble.Write(output : TWriteOnlyBlockStream);
var
   x86 : Tx86WriteOnlyStream;
begin
   x86:=(output as Tx86WriteOnlyStream);

   x86._push_reg(gprEBP);

   if FPreserveExecInEDI then begin
      x86._push_reg(gprEDI);
      x86._mov_reg_reg(gprEDI, gprEDX);
   end;

   // mov ebp, [edx+cStackMixinBaseDataOffset]
   x86.WriteBytes([$8B, $6A, cStackMixinBaseDataOffset]);

   if FReserve16OnStack then
      x86.WriteBytes([$83, $EC, $10]); // sub esp, 16
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
   Result:=0;
   if FPreamble.FPreserveExecInEDI then
      Inc(Result, 3);
   if FPreamble.FPreserveExecInEDI then
      Inc(Result, 1);
   Inc(Result, 2);

   Inc(Result, ($10-(FixedLocation and $F)) and $F);
end;

// Write
//
procedure TFixupPostamble.Write(output : TWriteOnlyBlockStream);
var
   x86 : Tx86WriteOnlyStream;
begin
   x86:=(output as Tx86WriteOnlyStream);

   if FPreamble.FReserve16OnStack then
      x86.WriteBytes([$83, $C4, $10]); // add esp, 16

   if FPreamble.FPreserveExecInEDI then
      x86._pop_reg(gprEDI);
   x86._pop_reg(gprEBP);
   x86._ret;

   // pad to multiple of 16 for alignment
   x86._nop(($10-(output.Position and $F)) and $F);
end;

// ------------------
// ------------------ Tx86FixupLogicHelper ------------------
// ------------------

// NewJump
//
function Tx86FixupLogicHelper.NewJump(flags : TboolFlags) : TFixupJump;
begin
   Result:=TFixupJump.Create(flags);
   AddFixup(Result);
end;

// NewJump
//
function Tx86FixupLogicHelper.NewJump(flags : TboolFlags; target : TFixup) : TFixupJump;
begin
   if target<>nil then begin
      Result:=NewJump(flags);
      Result.Target:=target;
   end else Result:=nil;
end;

// NewConditionalJumps
//
procedure Tx86FixupLogicHelper.NewConditionalJumps(flagsTrue : TboolFlags; targetTrue, targetFalse : TFixup);
begin
   if (targetTrue<>nil) and (targetTrue.Location<>0) then begin
      NewJump(flagsTrue, targetTrue);
      NewJump(NegateBoolFlags(flagsTrue), targetFalse);
   end else begin
      NewJump(NegateBoolFlags(flagsTrue), targetFalse);
      NewJump(flagsTrue, targetTrue);
   end;
end;

// ------------------
// ------------------ TdwsJITter_x86 ------------------
// ------------------

// Create
//
constructor TdwsJITter_x86.Create(jit : TdwsJITx86);
begin
   inherited Create(jit);
   FJIT:=jit;
   Fx86:=jit.x86;
end;

// CompileFloat
//
function TdwsJITter_x86.CompileFloat(expr : TExprBase) : Integer;
begin
   Result:=Ord(DoCompileFloat(expr));
end;

// DoCompileFloat
//
function TdwsJITter_x86.DoCompileFloat(expr : TExprBase) : TxmmRegister;
begin
   jit.OutputFailedOn:=expr;
   Result:=xmm0;
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

// CompileBoolean
//
procedure TdwsJITter_x86.CompileBoolean(expr : TExprBase; targetTrue, targetFalse : TFixup);
begin
   DoCompileBoolean(TBooleanBinOpExpr(expr), targetTrue, targetFalse);
end;

// DoCompileBoolean
//
procedure TdwsJITter_x86.DoCompileBoolean(expr : TExprBase; targetTrue, targetFalse : TFixup);
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
   jmp [eax+FCode]
end;

// ------------------
// ------------------ TFloatExpr86 ------------------
// ------------------

// EvalAsFloat
//
function TFloatExpr86.EvalAsFloat(exec : TdwsExecution) : Double;
asm
   jmp [eax+FCode]
end;

// ------------------
// ------------------ TIntegerExpr86 ------------------
// ------------------

// EvalAsInteger
//
function TIntegerExpr86.EvalAsInteger(exec : TdwsExecution) : Int64;
asm
   jmp [eax+FCode]
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

   reg:=jit.AllocXMMReg(expr);

   // check below is necessary as -Nan will be reported equal to zero
   if (e.Right=0) and not IsNaN(e.Right) then
      x86._xorps_reg_reg(reg, reg)
   else x86._movsd_reg_absmem(reg, @e.Right);

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
   reg : TxmmRegister;
begin
   e:=TAssignConstToIntegerVarExpr(expr);

   if e.Left.ClassType=TIntVarExpr then begin

      reg:=jit.AllocXMMReg(expr);

      if e.Right=0 then
         x86._xorps_reg_reg(reg, reg)
      else x86._movq_reg_absmem(reg, @e.Right);

      x86._movq_bpmem_reg(TVarExpr(e.Left).StackAddr, reg);

   end else inherited;
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

      if e.Right then
         x86._mov_reg_dword(gprEAX, 1)
      else x86._xor_reg_reg(gprEAX, gprEAX);
      x86._mov_bpmem_reg(TVarExpr(e.Left).StackAddr, 0, gprEAX);

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
   reg : TxmmRegister;
begin
   e:=TAssignExpr(expr);

   if jit.IsFloat(e.Left) then begin

      reg:=jit.CompileFloat(e.Right);
      jit.CompileAssignFloat(e.Left, reg);

   end else inherited;
end;

// ------------------
// ------------------ Tx86OpAssignFloat ------------------
// ------------------

// Create
//
constructor Tx86OpAssignFloat.Create(jit : TdwsJITx86; op : TxmmOp);
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
begin
   e:=TOpAssignExpr(expr);

   regRight:=jit.CompileFloat(e.Right);
   reg:=jit.CompileFloat(e.Left);

   x86._xmm_reg_reg(OP, reg, regRight);

   jit.CompileAssignFloat(e.Left, reg);

   if regRight<>reg then
      jit.ReleaseXMMReg(regRight);
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

   targetTrue:=jit.Fixups.NewHangingTarget;
   targetFalse:=jit.Fixups.NewHangingTarget;

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

   targetTrue:=jit.Fixups.NewHangingTarget;
   targetFalse:=jit.Fixups.NewHangingTarget;
   targetDone:=jit.Fixups.NewHangingTarget;

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

   jit.ResetXMMReg;

   targetExit:=jit.Fixups.NewHangingTarget;
   targetLoop:=jit.Fixups.NewHangingTarget;

   CompileStartCondition(e, targetLoop, targetExit);

   jit.Fixups.AddFixup(targetLoop);

   jit._DoStep(e.LoopExpr);
   jit.CompileStatement(e.LoopExpr);

   CompileFinalCondition(e, targetLoop, targetExit);

   jit.Fixups.AddFixup(targetExit);

   jit.ResetXMMReg;
end;

// CompileStartCondition
//
procedure Tx86Loop.CompileStartCondition(expr : TLoopExpr; targetLoop, targetExit : TFixupTarget);
begin
   // nothing
end;

// CompileFinalCondition
//
procedure Tx86Loop.CompileFinalCondition(expr : TLoopExpr; targetLoop, targetExit : TFixupTarget);
begin
   // nothing
end;

// ------------------
// ------------------ Tx86Repeat ------------------
// ------------------

// CompileFinalCondition
//
procedure Tx86Repeat.CompileFinalCondition(expr : TLoopExpr; targetLoop, targetExit : TFixupTarget);
begin
   jit._DoStep(expr.CondExpr);
   jit.CompileBoolean(expr.CondExpr, targetExit, targetLoop);
end;

// ------------------
// ------------------ Tx86While ------------------
// ------------------

// CompileStartCondition
//
procedure Tx86While.CompileStartCondition(expr : TLoopExpr; targetLoop, targetExit : TFixupTarget);
begin
   jit._DoStep(expr.CondExpr);
   jit.CompileBoolean(expr.CondExpr, targetLoop, targetExit);
end;

// ------------------
// ------------------ Tx86ForUpward ------------------
// ------------------

// CompileStatement
//
procedure Tx86ForUpward.CompileStatement(expr : TExprBase);
var
   e : TForUpwardExpr;
   jumpIfHiLower : TFixupJump;
   loopStart : TFixupTarget;
   loopContinue : TFixupTarget;
   loopAfter : TFixupTarget;
   fromValue, toValue : Int64;
   is32bit : Boolean;
begin
   e:=TForUpwardExpr(expr);

   jit.ResetXMMReg;

   if e.ToExpr is TConstIntExpr then begin

      toValue:=TConstIntExpr(e.ToExpr).Value;

      if e.FromExpr is TConstIntExpr then begin

         fromValue:=TConstIntExpr(e.FromExpr).Value;

         x86._mov_bpmem_imm(e.VarExpr.StackAddr, fromValue);

         is32bit:=(fromValue>=0) and (Integer(toValue)=toValue) and (Integer(fromValue)=fromValue);

      end else begin

         jit.CompileInteger(e.FromExpr);
         x86._mov_bpmem_eaxedx(e.VarExpr.StackAddr);

         is32bit:=False;

      end;

      loopStart:=jit.Fixups.NewTarget;
      loopContinue:=jit.Fixups.NewHangingTarget;
      loopAfter:=jit.Fixups.NewHangingTarget;

      jit.EnterLoop(loopContinue, loopAfter);

      if is32bit then begin

         x86._cmp_bpmem_int32(e.VarExpr.StackAddr, 0, toValue);
         jit.Fixups.NewJump(flagsG, loopAfter);

         jit._DoStep(e.DoExpr);
         jit.CompileStatement(e.DoExpr);

         jit.Fixups.AddFixup(loopContinue);

         x86._add_bpmem_int32(e.VarExpr.StackAddr, 0, 1);

         jit.Fixups.NewJump(flagsNone, loopStart);

      end else begin

         x86._cmp_bpmem_int32(e.VarExpr.StackAddr, 4, toValue shr 32);
         jit.Fixups.NewJump(flagsG, loopAfter);
         jumpIfHiLower:=jit.Fixups.NewJump(flagsB);

         x86._cmp_bpmem_int32(e.VarExpr.StackAddr, 0, toValue);
         jit.Fixups.NewJump(flagsG, loopAfter);

         jumpIfHiLower.NewTarget;

         jit._DoStep(e.DoExpr);
         jit.CompileStatement(e.DoExpr);

         jit.Fixups.AddFixup(loopContinue);

         x86._int64_inc(e.VarExpr.StackAddr, 1);

         jit.Fixups.NewJump(flagsNone, loopStart);

      end;

      if JIT.LoopContext.Exited then
         jit.ResetXMMReg;

      jit.LeaveLoop;

      jit.Fixups.AddFixup(loopAfter);

   end else inherited;
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
// ------------------ Tx86FloatBinOp ------------------
// ------------------

// Create
//
constructor Tx86FloatBinOp.Create(jit : TdwsJITx86; op : TxmmOp);
begin
   inherited Create(jit);
   Self.OP:=op;
end;

// CompileFloat
//
function Tx86FloatBinOp.DoCompileFloat(expr : TExprBase) : TxmmRegister;
var
   e : TFloatBinOpExpr;
begin
   e:=TFloatBinOpExpr(expr);

   Result:=jit.CompileFloat(e.Left);

   jit._xmm_reg_expr(OP, Result, e.Right);

   jit.ContainsXMMReg(Result, expr);
end;

// ------------------
// ------------------ Tx86SqrFloat ------------------
// ------------------

// CompileFloat
//
function Tx86SqrFloat.DoCompileFloat(expr : TExprBase) : TxmmRegister;
var
   e : TSqrFloatExpr;
begin
   e:=TSqrFloatExpr(expr);

   Result:=jit.CompileFloat(e.Expr);

   x86._xmm_reg_reg(xmm_multsd, Result, Result);

   jit.ContainsXMMReg(Result, expr);
end;

// ------------------
// ------------------ Tx86AbsFloat ------------------
// ------------------

// DoCompileFloat
//
function Tx86AbsFloat.DoCompileFloat(expr : TExprBase) : TxmmRegister;
var
   e : TAbsFloatExpr;
begin
   e:=TAbsFloatExpr(expr);

   Result:=jit.CompileFloat(e.Expr);

   // andpd Result, dqword ptr [AbsMask]
   x86.WriteBytes([$66, $0F, $54, $05+Ord(Result)*8]);
   x86.WritePointer(jit.AbsMaskPD);

   jit.ContainsXMMReg(Result, expr);
end;

// ------------------
// ------------------ Tx86NegFloat ------------------
// ------------------

// DoCompileFloat
//
function Tx86NegFloat.DoCompileFloat(expr : TExprBase) : TxmmRegister;
var
   e : TNegFloatExpr;
begin
   e:=TNegFloatExpr(expr);

   Result:=jit.CompileFloat(e.Expr);

   // xorpd Result, dqword ptr [SignMask]
   x86.WriteBytes([$66, $0F, $57, $05+Ord(Result)*8]);
   x86.WritePointer(jit.SignMaskPD);

   jit.ContainsXMMReg(Result, expr);
end;

// ------------------
// ------------------ Tx86ConstFloat ------------------
// ------------------

// CompileFloat
//
function Tx86ConstFloat.DoCompileFloat(expr : TExprBase) : TxmmRegister;
var
   e : TConstFloatExpr;
begin
   e:=TConstFloatExpr(expr);

   Result:=jit.AllocXMMReg(e);

   x86._movsd_reg_absmem(Result, @e.Value);
end;

// ------------------
// ------------------ Tx86ConstInt ------------------
// ------------------

// CompileFloat
//
function Tx86ConstInt.DoCompileFloat(expr : TExprBase) : TxmmRegister;
var
   e : TConstIntExpr;
begin
   e:=TConstIntExpr(expr);

   if (e.Value>-MaxInt) and (e.Value<MaxInt) then begin

      Result:=jit.AllocXMMReg(e, e);

      x86._xmm_reg_absmem(xmm_cvtsi2sd, Result, @e.Value);

   end else Result:=inherited;
end;

// CompileInteger
//
function Tx86ConstInt.CompileInteger(expr : TExprBase) : Integer;
var
   e : TConstIntExpr;
begin
   e:=TConstIntExpr(expr);

   x86._mov_eaxedx_imm(e.Value);

   Result:=0;
end;

// ------------------
// ------------------ Tx86FloatVar ------------------
// ------------------

// CompileFloat
//
function Tx86FloatVar.DoCompileFloat(expr : TExprBase) : TxmmRegister;
var
   e : TFloatVarExpr;
begin
   e:=TFloatVarExpr(expr);

   Result:=jit.CurrentXMMReg(e.DataSym);

   if Result=xmmNone then begin

      Result:=jit.AllocXMMReg(e, e.DataSym);
      x86._movsd_reg_bpmem(Result, e.StackAddr);

   end;
end;

// DoCompileAssignFloat
//
procedure Tx86FloatVar.DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister);
var
   e : TFloatVarExpr;
begin
   e:=TFloatVarExpr(expr);

   x86._movsd_bpmem_reg(e.StackAddr, source);

   jit.ContainsXMMReg(source, e.DataSym);
end;

// ------------------
// ------------------ Tx86IntVar ------------------
// ------------------

// CompileFloat
//
function Tx86IntVar.DoCompileFloat(expr : TExprBase) : TxmmRegister;
var
   e : TIntVarExpr;
begin
   e:=TIntVarExpr(expr);

   Result:=jit.AllocXMMReg(e);

   jit.FPreamble.Reserve16OnStack:=True;

   x86._fild_bpmem(e.StackAddr);
   x86._fstp_esp;

   x86._movsd_reg_esp(Result);
end;

// CompileInteger
//
function Tx86IntVar.CompileInteger(expr : TExprBase) : Integer;
var
   e : TIntVarExpr;
begin
   e:=TIntVarExpr(expr);

   x86._mov_eaxedx_bpmem(e.StackAddr);

   Result:=0;
end;

// ------------------
// ------------------ Tx86RecordVar ------------------
// ------------------

// CompileFloat
//
function Tx86RecordVar.DoCompileFloat(expr : TExprBase) : TxmmRegister;
var
   e : TRecordVarExpr;
begin
   e:=TRecordVarExpr(expr);

   if jit.IsFloat(e) then begin

      Result:=jit.AllocXMMReg(e);
      x86._movsd_reg_bpmem(Result, e.VarPlusMemberOffset);

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

      x86._movsd_bpmem_reg(e.VarPlusMemberOffset, source);
      jit.ReleaseXMMReg(source);

   end else inherited;
end;

// ------------------
// ------------------ Tx86VarParam ------------------
// ------------------

// CompileAsPVariant
//
class procedure Tx86VarParam.CompileAsPVariant(x86 : Tx86WriteOnlyStream; expr : TVarParamExpr);
begin
   x86._mov_reg_bpmem(gprEAX, expr.StackAddr);
   x86._xor_reg_reg(gprEDX, gprEDX);
   x86._mov_reg_dword_ptr_reg(gprECX, gprEAX);
   x86._call_reg(gprECX, vmt_IDataContext_AsPVariant);
end;

// DoCompileFloat
//
function Tx86VarParam.DoCompileFloat(expr : TExprBase) : TxmmRegister;
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

// DoCompileAssignFloat
//
procedure Tx86VarParam.DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister);
var
   e : TVarParamExpr;
begin
   e:=TVarParamExpr(expr);

   CompileAsPVariant(x86, e);

   if jit.IsFloat(e) then begin

      x86._movsd_qword_ptr_reg_reg(gprEAX, cVariant_DataOffset, source);

   end else inherited;
end;

// ------------------
// ------------------ Tx86StaticArray ------------------
// ------------------

// DoCompileFloat
//
function Tx86StaticArray.DoCompileFloat(expr : TExprBase) : TxmmRegister;
var
   e : TStaticArrayExpr;
   index : Integer;
begin
   e:=TStaticArrayExpr(expr);

   if (e.BaseExpr.ClassType=TVarParamExpr) and (e.IndexExpr is TConstIntExpr) then begin

      index:=TConstIntExpr(e.IndexExpr).Value;

      if jit.IsFloat(e) then begin

         Tx86VarParam.CompileAsPVariant(x86, TVarParamExpr(e.BaseExpr));
         x86._mov_reg_dword(gprECX, index*SizeOf(Variant));
         Result:=jit.AllocXMMReg(e);
         x86._movsd_reg_qword_ptr_indexed(Result, gprEAX, gprECX, 1, cVariant_DataOffset);

      end else Result:=inherited;

   end else if (e.BaseExpr.ClassType=TVarExpr) and (e.IndexExpr is TConstIntExpr) then begin

      index:=TConstIntExpr(e.IndexExpr).Value;

      if jit.IsFloat(e) then begin

         Result:=jit.AllocXMMReg(e);
         x86._movsd_reg_bpmem(Result, TVarExpr(e.BaseExpr).StackAddr+index);

      end else Result:=inherited;

   end else Result:=inherited;
end;

// DoCompileAssignFloat
//
procedure Tx86StaticArray.DoCompileAssignFloat(expr : TTypedExpr; source : TxmmRegister);
var
   e : TStaticArrayExpr;
   index : Integer;
begin
   e:=TStaticArrayExpr(expr);

   if (e.BaseExpr.ClassType=TVarParamExpr) and (e.IndexExpr is TConstIntExpr) then begin

      index:=TConstIntExpr(e.IndexExpr).Value;

      if jit.IsFloat(e) then begin

         Tx86VarParam.CompileAsPVariant(x86, TVarParamExpr(e.BaseExpr));
         x86._mov_reg_dword(gprECX, index*SizeOf(Variant));
         x86._movsd_qword_ptr_indexed_reg(gprEAX, gprECX, 1, cVariant_DataOffset, source);

      end else inherited;

   end else if (e.BaseExpr.ClassType=TVarExpr) and (e.IndexExpr is TConstIntExpr) then begin

      index:=TConstIntExpr(e.IndexExpr).Value;

      if jit.IsFloat(e) then begin

         x86._movsd_bpmem_reg(TVarExpr(e.BaseExpr).StackAddr+index, source);

      end else inherited;

   end else inherited;
end;

// ------------------
// ------------------ Tx86AddInt ------------------
// ------------------

// CompileInteger
//
function Tx86AddInt.CompileInteger(expr : TExprBase) : Integer;
var
   e : TAddIntExpr;
begin
   e:=TAddIntExpr(expr);

   if (e.Left.ClassType=TIntVarExpr) and (e.Right is TConstIntExpr) then begin

      jit.CompileInteger(e.Left);
      x86._inc_eaxedx_imm(TConstIntExpr(e.Right).Value);
      Result:=0;

   end else Result:=inherited;
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
   e:=TIncIntVarExpr(expr);

   if e.Right is TConstIntExpr then begin

      x86._int64_inc((e.Left as TVarExpr).StackAddr, TConstIntExpr(e.Right).Value);

   end else inherited;
end;

// ------------------
// ------------------ Tx86DecIntVar ------------------
// ------------------

// CompileStatement
//
procedure Tx86DecIntVar.CompileStatement(expr : TExprBase);
var
   e : TIncIntVarExpr;
begin
   e:=TIncIntVarExpr(expr);

   if e.Right is TConstIntExpr then begin

      x86._int64_dec((e.Left as TVarExpr).StackAddr, TConstIntExpr(e.Right).Value);

   end else inherited;
end;

// ------------------
// ------------------ Tx86RelEqualInt ------------------
// ------------------

// TestEqual
//
procedure Tx86RelEqualInt.TestEqual(addr : Integer; value : Int64; targetTrue, targetFalse : TFixup);
begin
   x86._cmp_bpmem_int32(addr, 0, value);
   jit.Fixups.NewJump(flagsNZ, targetFalse);
   x86._cmp_bpmem_int32(addr, 4, value shr 32);
   jit.Fixups.NewConditionalJumps(flagsZ, targetTrue, targetFalse);
end;

// DoCompileBoolean
//
procedure Tx86RelEqualInt.DoCompileBoolean(expr : TExprBase; targetTrue, targetFalse : TFixup);
var
   e : TRelEqualIntExpr;
begin
   e:=TRelEqualIntExpr(expr);

   if (e.Left is TIntVarExpr) and (e.Right is TConstIntExpr) then begin

      TestEqual(TIntVarExpr(e.Left).StackAddr, TConstIntExpr(e.Right).Value,
                targetTrue, targetFalse);

   end else inherited;
end;

// ------------------
// ------------------ Tx86RelNotEqualInt ------------------
// ------------------

// DoCompileBoolean
//
procedure Tx86RelNotEqualInt.DoCompileBoolean(expr : TExprBase; targetTrue, targetFalse : TFixup);
begin
   inherited DoCompileBoolean(expr, targetFalse, targetTrue);
end;

// ------------------
// ------------------ Tx86RelIntIsZero ------------------
// ------------------

// DoCompileBoolean
//
procedure Tx86RelIntIsZero.DoCompileBoolean(expr : TExprBase; targetTrue, targetFalse : TFixup);
var
   e : TUnaryOpBoolExpr;
begin
   e:=TUnaryOpBoolExpr(expr);

   if e.Expr is TIntVarExpr then begin

      TestEqual(TIntVarExpr(e.Expr).StackAddr, 0, targetTrue, targetFalse);

   end else inherited;
end;

// ------------------
// ------------------ Tx86RelIntIsNotZero ------------------
// ------------------

// DoCompileBoolean
//
procedure Tx86RelIntIsNotZero.DoCompileBoolean(expr : TExprBase; targetTrue, targetFalse : TFixup);
begin
   inherited DoCompileBoolean(expr, targetFalse, targetTrue);
end;

// ------------------
// ------------------ Tx86RelOpFloat ------------------
// ------------------

// Create
//
constructor Tx86RelOpFloat.Create(jit : TdwsJITx86; flags : TboolFlags);
begin
   inherited Create(jit);
   Self.Flags:=flags;
end;

// DoCompileBoolean
//
procedure Tx86RelOpFloat.DoCompileBoolean(expr : TExprBase; targetTrue, targetFalse : TFixup);
var
   e : TRelGreaterFloatExpr;
   regLeft : TxmmRegister;
begin
   e:=TRelGreaterFloatExpr(expr);

   regLeft:=jit.CompileFloat(e.Left);

   jit._comisd_reg_expr(regLeft, e.Right);

   jit.Fixups.NewConditionalJumps(Flags, targetTrue, targetFalse);
end;

// ------------------
// ------------------ Tx86NotExpr ------------------
// ------------------

// DoCompileBoolean
//
procedure Tx86NotExpr.DoCompileBoolean(expr : TExprBase; targetTrue, targetFalse : TFixup);
var
   e : TNotBoolExpr;
begin
   e:=TNotBoolExpr(expr);

   jit.CompileBoolean(e.Expr, targetFalse, targetTrue);
end;

// ------------------
// ------------------ Tx86BoolOrExpr ------------------
// ------------------

// DoCompileBoolean
//
procedure Tx86BoolOrExpr.DoCompileBoolean(expr : TExprBase; targetTrue, targetFalse : TFixup);
var
   e : TBooleanBinOpExpr;
begin
   e:=TBooleanBinOpExpr(expr);

   jit.CompileBoolean(e.Left, targetTrue, nil);
   jit.CompileBoolean(e.Right, targetTrue, targetFalse);
end;

// ------------------
// ------------------ Tx86BoolAndExpr ------------------
// ------------------

// JumpSafeCompile
//
procedure Tx86BoolAndExpr.DoCompileBoolean(expr : TExprBase; targetTrue, targetFalse : TFixup);
var
   e : TBooleanBinOpExpr;
begin
   e:=TBooleanBinOpExpr(expr);

   jit.CompileBoolean(e.Left, nil, targetFalse);
   jit.CompileBoolean(e.Right, targetTrue, targetFalse);
end;

// ------------------
// ------------------ Tx86InterpretedExpr ------------------
// ------------------

// DoCallEval
//
procedure Tx86InterpretedExpr.DoCallEval(expr : TExprBase; vmt : Integer);
begin
   jit.FPreamble.PreserveExecInEDI:=True;

   x86._mov_reg_reg(gprEDX, gprEDI);
   x86._mov_reg_dword(gprEAX, DWORD(expr));
   x86._mov_reg_dword_ptr_reg(gprECX, gprEAX);

   x86._call_reg(gprECX, vmt);

   if jit.FSavedXMM=[] then
      jit.ResetXMMReg;

   jit.QueueGreed(expr);
end;

// CompileStatement
//
procedure Tx86InterpretedExpr.CompileStatement(expr : TExprBase);
begin
   DoCallEval(expr, vmt_TExprBase_EvalNoResult);
end;

// DoCompileFloat
//
function Tx86InterpretedExpr.DoCompileFloat(expr : TExprBase) : TxmmRegister;
begin
   jit.SaveXMMRegs;

   DoCallEval(expr, vmt_TExprBase_EvalAsFloat);

   jit.RestoreXMMRegs;

   jit.FPreamble.Reserve16OnStack:=True;
   Result:=jit.AllocXMMReg(expr);
   x86._fstp_esp;
   x86._movsd_reg_esp(Result);

   jit.QueueGreed(expr);
end;

// CompileInteger
//
function Tx86InterpretedExpr.CompileInteger(expr : TExprBase) : Integer;
begin
   jit.SaveXMMRegs;

   DoCallEval(expr, vmt_TExprBase_EvalAsInteger);

   jit.RestoreXMMRegs;

   jit.QueueGreed(expr);

   Result:=0;
end;

// ------------------
// ------------------ Tx86MagicFloatFunc ------------------
// ------------------

// DoCompileFloat
//
function Tx86MagicFloatFunc.DoCompileFloat(expr : TExprBase) : TxmmRegister;
var
   jitter : TdwsJITter_x86;
   e : TMagicFloatFuncExpr;
begin
   e:=TMagicFloatFuncExpr(expr);

   jitter:=TdwsJITter_x86(jit.FindJITter(TMagicFuncSymbol(e.FuncSym).InternalFunction.ClassType));
   if jitter<>nil then

      Result:=jitter.DoCompileFloat(expr)

   else Result:=inherited;
end;

// ------------------
// ------------------ Tx86SqrtFunc ------------------
// ------------------

// DoCompileFloat
//
function Tx86SqrtFunc.DoCompileFloat(expr : TExprBase) : TxmmRegister;
begin
   Result:=jit.AllocXMMReg(expr);

   jit._xmm_reg_expr(xmm_sqrtsd, Result, TMagicFuncExpr(expr).Args[0] as TTypedExpr);
end;

end.
