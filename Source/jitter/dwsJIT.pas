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
unit dwsJIT;

{$I ../dws.inc}

interface

uses
   Classes, SysUtils,
   dwsExprs, dwsExprList, dwsSymbols, dwsErrors, dwsUtils, dwsCoreExprs, dwsXPlatform,
   dwsRelExprs, dwsMagicExprs, dwsJITFixups, dwsScriptSource;

type

   DWORD = Cardinal;

   TdwsJIT = class;

   TdwsJITOption = (jitoDoStep, jitoRangeCheck,
                    jitoNoBranchAlignment);
   TdwsJITOptions = set of TdwsJITOption;

   TdwsJITter = class (TRefCountedObject)
      private
         Fjit : TdwsJIT;

      protected
         property jit : TdwsJIT read Fjit;

      public
         constructor Create(aJIT : TdwsJIT);

         function IncRefCount : TdwsJITter; inline;

         procedure CompileStatement(expr : TExprBase); virtual;

         function  CompileFloat(expr : TTypedExpr) : Integer; virtual;

         function  CompileInteger(expr : TTypedExpr) : Integer; virtual;

         function  CompileBooleanValue(expr : TTypedExpr) : Integer; virtual;
         procedure CompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup); virtual;

         function  CompileScriptObj(expr : TTypedExpr) : Integer; virtual;

         procedure CompileAssignFloat(expr : TTypedExpr; source : Integer); virtual;
         procedure CompileAssignInteger(expr : TTypedExpr; source : Integer); virtual;
         procedure CompileAssignBoolean(expr : TTypedExpr; source : Integer); virtual;
   end;

   TdwsRegisteredJITter = class (TRefCountedObject)
      public
         Expr : TClass;
         JIT : TdwsJITter;

         destructor Destroy; override;
   end;

   TdwsRegisteredJITterList = class(TSortedList<TdwsRegisteredJITter>)
      protected
         function Compare(const item1, item2 : TdwsRegisteredJITter) : Integer; override;
   end;

   TdwsJITCodeBlock = record
      public
         Code : Pointer;
         SubAllocator : TRefCountedObject;
         Steppable : Boolean;

         procedure Free;
   end;

   TJITTedProgramExpr = class (TNoResultExpr)
      private
         FOriginal : TProgramExpr;

      protected
         FCode : TdwsJITCodeBlock;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(original : TProgramExpr; const code : TdwsJITCodeBlock); virtual;
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;

         property Original : TProgramExpr read FOriginal write FOriginal;
   end;
   TJITTedProgramExprClass = class of TJITTedProgramExpr;

   TJITTedTypedExpr = class (TTypedExpr)
      private
         FOriginal : TTypedExpr;

      protected
         FCode : TdwsJITCodeBlock;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(original : TTypedExpr; const code : TdwsJITCodeBlock); virtual;
         destructor Destroy; override;

         function ScriptPos : TScriptPos; override;

         property Original : TTypedExpr read FOriginal write FOriginal;
   end;

   TJITTedFloatExpr = class (TJITTedTypedExpr)
      public
         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
   end;
   TJITTedFloatExprClass = class of TJITTedFloatExpr;

   TJITTedIntegerExpr = class (TJITTedTypedExpr)
      public
         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
   end;
   TJITTedIntegerExprClass = class of TJITTedIntegerExpr;

   TQueuedJITGreed = class
      public
         Next : TQueuedJITGreed;
         Expr : TExprBase;
         Prog : TdwsProgram;
   end;

   TJITLoopContext = class
      public
         TargetContinue : TFixup;
         TargetExit : TFixup;
         Exited : Boolean;
         Prev : TJITLoopContext;
   end;

   TdwsJIT = class
      private
         FRegistered : TdwsRegisteredJITterList;
         FTempReg : TdwsRegisteredJITter;
         FOutput : TWriteOnlyBlockStream;
         FOutputFailedOn : TExprBase;
         FSeenByGreedy : TSimpleObjectHash<TSymbol>;
         FQueuedGreed : TQueuedJITGreed;

         FFixups : TFixupLogic;
         FLoopContext : TJITLoopContext;
         FExitTarget : TFixupTarget;

         FJITTedProgramExprClass : TJITTedProgramExprClass;
         FJITTedFloatExprClass : TJITTedFloatExprClass;
         FJITTedIntegerExprClass : TJITTedIntegerExprClass;

         FOptions : TdwsJITOptions;

      protected
         function CreateOutput : TWriteOnlyBlockStream; virtual;
         function CreateFixupLogic : TFixupLogic; virtual;

         procedure SetOutputFailedOn(e : TExprBase);

         procedure StartJIT(expr : TExprBase; exitable : Boolean); virtual;
         procedure EndJIT; virtual;
         procedure EndFloatJIT(resultHandle : Integer); virtual;
         procedure EndIntegerJIT(resultHandle : Integer); virtual;

         function GetLocation : Integer;

         property JITTedProgramExprClass : TJITTedProgramExprClass read FJITTedProgramExprClass write FJITTedProgramExprClass;
         property JITTedFloatExprClass : TJITTedFloatExprClass read FJITTedFloatExprClass write FJITTedFloatExprClass;
         property JITTedIntegerExprClass : TJITTedIntegerExprClass read FJITTedIntegerExprClass write FJITTedIntegerExprClass;

      public
         constructor Create; virtual;
         destructor Destroy; override;

         procedure RegisterJITter(exprClass : TClass; jitter : TdwsJITter);
         function FindJITter(exprClass : TClass) : TdwsJITter; overload;
         function FindJITter(expr : TExprBase) : TdwsJITter; overload;

         function JITStatement(expr : TProgramExpr; exitable : Boolean) : TJITTedProgramExpr;
         function JITFloat(expr : TTypedExpr) : TJITTedFloatExpr;
         function JITInteger(expr : TTypedExpr) : TJITTedIntegerExpr;

         procedure CompileStatement(expr : TExprBase);
         function  CompileFloat(expr : TTypedExpr) : Integer;
         function  CompileInteger(expr : TTypedExpr) : Integer;
         function  CompileBooleanValue(expr : TTypedExpr) : Integer;
         procedure CompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
         function  CompileScriptObj(expr : TTypedExpr) : Integer;

         procedure CompileAssignFloat(expr : TTypedExpr; source : Integer);
         procedure CompileAssignInteger(expr : TTypedExpr; source : Integer);
         procedure CompileAssignBoolean(expr : TTypedExpr; source : Integer);

         function IsFloat(expr : TTypedExpr) : Boolean; overload; inline;
         function IsFloat(typ : TTypeSymbol) : Boolean; overload;
         function IsInteger(expr : TTypedExpr) : Boolean; overload;
         function IsBoolean(expr : TTypedExpr) : Boolean; overload;

         property LoopContext : TJITLoopContext read FLoopContext;
         property ExitTarget : TFixupTarget read FExitTarget;

         procedure EnterLoop(targetContinue, targetExit : TFixup);
         procedure LeaveLoop;

         property Fixups : TFixupLogic read FFixups;

         property Options : TdwsJITOptions read FOptions write FOptions;

         function CompiledOutput : TdwsJITCodeBlock; virtual; abstract;

         procedure Clear;
         procedure GreedyJIT(expr : TExprBase); overload;
         procedure GreedyJIT(prog : TdwsProgram); overload;
         procedure GreedyJITParameters(funcExpr : TFuncExprBase);
         procedure DeQueueGreed;
         procedure QueueGreed(expr : TExprBase); overload;
         procedure QueueGreed(prog : TdwsProgram); overload;

         property Output : TWriteOnlyBlockStream read FOutput;
         property OutputFailedOn : TExprBase read FOutputFailedOn write SetOutputFailedOn;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsJITter ------------------
// ------------------

// Create
//
constructor TdwsJITter.Create(aJIT : TdwsJIT);
begin
   Fjit:=aJIT;
end;

// IncRefCount
//
function TdwsJITter.IncRefCount : TdwsJITter;
begin
   inherited IncRefCount;
   Result:=Self;
end;

// CompileFloat
//
function TdwsJITter.CompileFloat(expr : TTypedExpr) : Integer;
begin
   jit.OutputFailedOn:=expr;
   Result:=0;
end;

// CompileInteger
//
function TdwsJITter.CompileInteger(expr : TTypedExpr) : Integer;
begin
   jit.OutputFailedOn:=expr;
   Result:=0;
end;

// CompileBooleanValue
//
function TdwsJITter.CompileBooleanValue(expr : TTypedExpr) : Integer;
begin
   jit.OutputFailedOn:=expr;
   Result:=0;
end;

// CompileBoolean
//
procedure TdwsJITter.CompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
begin
   jit.OutputFailedOn:=expr;
end;

// CompileScriptObj
//
function TdwsJITter.CompileScriptObj(expr : TTypedExpr) : Integer;
begin
   jit.OutputFailedOn:=expr;
   Result:=0;
end;

// CompileAssignFloat
//
procedure TdwsJITter.CompileAssignFloat(expr : TTypedExpr; source : Integer);
begin
   jit.OutputFailedOn:=expr;
end;

// CompileAssignInteger
//
procedure TdwsJITter.CompileAssignInteger(expr : TTypedExpr; source : Integer);
begin
   jit.OutputFailedOn:=expr;
end;

// CompileAssignBoolean
//
procedure TdwsJITter.CompileAssignBoolean(expr : TTypedExpr; source : Integer);
begin
   jit.OutputFailedOn:=expr;
end;

// CompileStatement
//
procedure TdwsJITter.CompileStatement(expr : TExprBase);
begin
   jit.OutputFailedOn:=expr;
end;

// ------------------
// ------------------ TdwsRegisteredJITter ------------------
// ------------------

// Destroy
//
destructor TdwsRegisteredJITter.Destroy;
begin
   inherited;
   JIT.Free;
end;

// ------------------
// ------------------ TdwsRegisteredJITterList ------------------
// ------------------

// Compare
//
function TdwsRegisteredJITterList.Compare(const item1, item2 : TdwsRegisteredJITter) : Integer;
var
   i1, i2 : Integer;
begin
   i1:=NativeInt(item1.Expr);
   i2:=NativeInt(item2.Expr);
   if i1<i2 then
      Result:=-1
   else if i1=i2 then
      Result:=0
   else Result:=1;
end;

// ------------------
// ------------------ TdwsJITCodeBlock ------------------
// ------------------

// Free
//
procedure TdwsJITCodeBlock.Free;
begin
   SubAllocator.DecRefCount;
   Code:=nil;
   SubAllocator:=nil;
end;

// ------------------
// ------------------ TdwsJITter ------------------
// ------------------

// Create
//
constructor TdwsJIT.Create;
begin
   inherited;
   FRegistered:=TdwsRegisteredJITterList.Create;
   FTempReg:=TdwsRegisteredJITter.Create;
   FOutput:=CreateOutput;
   FSeenByGreedy:=TSimpleObjectHash<TSymbol>.Create;

   FFixups:=CreateFixupLogic;
   FFixups.OnNeedLocation:=GetLocation;
end;

// Destroy
//
destructor TdwsJIT.Destroy;
begin
   inherited;
   FFixups.Free;
   FRegistered.Clean;
   FRegistered.Free;
   FTempReg.Free;
   FOutput.Free;
   FSeenByGreedy.Free;
end;

// CreateOutput
//
function TdwsJIT.CreateOutput : TWriteOnlyBlockStream;
begin
   Result:=TWriteOnlyBlockStream.Create;
end;

// CreateFixupLogic
//
function TdwsJIT.CreateFixupLogic : TFixupLogic;
begin
   Result:=TFixupLogic.Create;
end;

// SetOutputFailedOn
//
procedure TdwsJIT.SetOutputFailedOn(e : TExprBase);
begin
   FOutputFailedOn:=e;
end;

// RegisterJITter
//
procedure TdwsJIT.RegisterJITter(exprClass : TClass; jitter : TdwsJITter);
var
   reg : TdwsRegisteredJITter;
begin
   reg:=TdwsRegisteredJITter.Create;
   reg.Expr:=exprClass;
   reg.JIT:=jitter;
   FRegistered.Add(reg);
end;

// FindJITter
//
function TdwsJIT.FindJITter(exprClass : TClass) : TdwsJITter;
var
   i : Integer;
begin
   if exprClass.InheritsFrom(TJITTedProgramExpr) then
      FTempReg.Expr:=exprClass;
   FTempReg.Expr:=exprClass;
   if FRegistered.Find(FTempReg, i) then
      Result:=FRegistered.Items[i].JIT
   else Result:=nil;
end;

// FindJITter
//
function TdwsJIT.FindJITter(expr : TExprBase) : TdwsJITter;
begin
   Result:=FindJITter(expr.ClassType);
end;

// JITStatement
//
function TdwsJIT.JITStatement(expr : TProgramExpr; exitable : Boolean) : TJITTedProgramExpr;
var
   jit : TdwsJITter;
begin
   Result:=nil;

   jit:=FindJITter(expr);
   if jit=nil then begin
      OutputDebugString(expr.ClassName);
      Exit;
   end;

   StartJIT(expr, exitable);
   jit.CompileStatement(expr);
   EndJIT;

   if OutputFailedOn=nil then
      Result:=JITTedProgramExprClass.Create(expr, CompiledOutput)
   else begin
      OutputDebugString(OutputFailedOn.ClassName);
      GreedyJIT(expr);
   end;
end;

// JITFloat
//
function TdwsJIT.JITFloat(expr : TTypedExpr) : TJITTedFloatExpr;
var
   outcome : Integer;
begin
   Result:=nil;

   StartJIT(expr, False);
   outcome:=CompileFloat(expr);
   EndFloatJIT(outcome);

   if (OutputFailedOn=nil) then
      Result:=JITTedFloatExprClass.Create(expr, CompiledOutput)
   else OutputDebugString(OutputFailedOn.ClassName);
end;

// JITInteger
//
function TdwsJIT.JITInteger(expr : TTypedExpr) : TJITTedIntegerExpr;
var
   outcome : Integer;
begin
   Result:=nil;

   StartJIT(expr, False);
   outcome:=CompileInteger(expr);
   EndIntegerJIT(outcome);

   if (OutputFailedOn=nil) then
      Result:=JITTedIntegerExprClass.Create(expr, CompiledOutput)
   else OutputDebugString(OutputFailedOn.ClassName);
end;

// CompileStatement
//
procedure TdwsJIT.CompileStatement(expr : TExprBase);
var
   jit : TdwsJITter;
begin
   jit:=FindJITter(expr);
   if jit<>nil then
      jit.CompileStatement(expr)
   else OutputFailedOn:=expr;
end;

// CompileFloat
//
function TdwsJIT.CompileFloat(expr : TTypedExpr) : Integer;
var
   jit : TdwsJITter;
begin
   jit:=FindJITter(expr);
   if jit=nil then begin
      OutputFailedOn:=expr;
      Result:=0;
   end else begin
      Result:=jit.CompileFloat(expr);
   end;
end;

// CompileInteger
//
function TdwsJIT.CompileInteger(expr : TTypedExpr) : Integer;
var
   jit : TdwsJITter;
begin
   jit:=FindJITter(expr);
   if jit=nil then begin
      OutputFailedOn:=expr;
      Result:=0;
   end else begin
      Result:=jit.CompileInteger(expr);
   end;
end;

// CompileBooleanValue
//
function TdwsJIT.CompileBooleanValue(expr : TTypedExpr) : Integer;
var
   jit : TdwsJITter;
begin
   jit:=FindJITter(expr);
   if jit=nil then begin
      OutputFailedOn:=expr;
      Result:=0;
   end else Result:=jit.CompileBooleanValue(expr);
end;

// CompileBoolean
//
procedure TdwsJIT.CompileBoolean(expr : TTypedExpr; targetTrue, targetFalse : TFixup);
var
   jit : TdwsJITter;
begin
   jit:=FindJITter(expr);
   if jit=nil then
      OutputFailedOn:=expr
   else jit.CompileBoolean(expr, targetTrue, targetFalse);
end;

// CompileScriptObj
//
function TdwsJIT.CompileScriptObj(expr : TTypedExpr) : Integer;
var
   jit : TdwsJITter;
begin
   jit:=FindJITter(expr);
   if jit=nil then begin
      OutputFailedOn:=expr;
      Result:=0;
   end else Result:=jit.CompileScriptObj(expr);
end;

// CompileAssignFloat
//
procedure TdwsJIT.CompileAssignFloat(expr : TTypedExpr; source : Integer);
var
   jit : TdwsJITter;
begin
   jit:=FindJITter(expr);
   if jit=nil then
      OutputFailedOn:=expr
   else jit.CompileAssignFloat(expr, source);
end;

// CompileAssignInteger
//
procedure TdwsJIT.CompileAssignInteger(expr : TTypedExpr; source : Integer);
var
   jit : TdwsJITter;
begin
   jit:=FindJITter(expr);
   if jit=nil then
      OutputFailedOn:=expr
   else jit.CompileAssignInteger(expr, source);
end;

// CompileAssignBoolean
//
procedure TdwsJIT.CompileAssignBoolean(expr : TTypedExpr; source : Integer);
var
   jit : TdwsJITter;
begin
   jit:=FindJITter(expr);
   if jit=nil then
      OutputFailedOn:=expr
   else jit.CompileAssignBoolean(expr, source);
end;

// IsFloat
//
function TdwsJIT.IsFloat(expr : TTypedExpr) : Boolean;
begin
   Result:=IsFloat(expr.Typ);
end;

// IsFloat
//
function TdwsJIT.IsFloat(typ : TTypeSymbol) : Boolean;
begin
   Result:=(typ.UnAliasedType.ClassType=TBaseFloatSymbol);
end;

// IsInteger
//
function TdwsJIT.IsInteger(expr : TTypedExpr) : Boolean;
begin
   Result:=(expr.Typ.UnAliasedType.ClassType=TBaseIntegerSymbol);
end;

// IsBoolean
//
function TdwsJIT.IsBoolean(expr : TTypedExpr) : Boolean;
begin
   Result:=(expr.Typ.UnAliasedType.ClassType=TBaseBooleanSymbol);
end;

// EnterLoop
//
procedure TdwsJIT.EnterLoop(targetContinue, targetExit : TFixup);
var
   context : TJITLoopContext;
begin
   context:=TJITLoopContext.Create;
   context.TargetContinue:=targetContinue;
   context.TargetExit:=targetExit;
   context.Prev:=FLoopContext;
   FLoopContext:=context;
end;

// LeaveLoop
//
procedure TdwsJIT.LeaveLoop;
var
   context : TJITLoopContext;
begin
   context:=FLoopContext;
   FLoopContext:=context.Prev;
   context.Free;
end;

// StartJIT
//
procedure TdwsJIT.StartJIT(expr : TExprBase; exitable : Boolean);
begin
   FOutput.Clear;
   FOutputFailedOn:=nil;
   if exitable then
      FExitTarget:=Fixups.NewHangingTarget(False);
end;

// EndJIT
//
procedure TdwsJIT.EndJIT;
begin
   if FExitTarget<>nil then begin
      Fixups.AddFixup(FExitTarget);
      FExitTarget:=nil;
   end;
   // nothing
end;

// EndFloatJIT
//
procedure TdwsJIT.EndFloatJIT(resultHandle : Integer);
begin
   EndJIT;
end;

// EndIntegerJIT
//
procedure TdwsJIT.EndIntegerJIT(resultHandle : Integer);
begin
   EndJIT;
end;

// GetLocation
//
function TdwsJIT.GetLocation : Integer;
begin
   Result:=Output.Position;
end;

// Clear
//
procedure TdwsJIT.Clear;
begin
   FSeenByGreedy.Clear;
end;

// GreedyJIT
//
procedure TdwsJIT.GreedyJIT(expr : TExprBase);
var
   i : Integer;
   block : TBlockExprBase;
   subExpr : TExprBase;
   statementJIT : TJITTedProgramExpr;
   floatJIT : TJITTedFloatExpr;
   funcSym : TFuncSymbol;
   funcExpr : TFuncExprBase;
   executable : IExecutable;
   execObject : TObject;
   assignExpr : TAssignExpr;
   composite : TCompositeTypeSymbol;
begin
   if expr is TBlockExprBase then begin
      block:=TBlockExprBase(expr);
      for i:=0 to block.SubExprCount-1 do begin
         subExpr:=block.SubExpr[i];
         if subExpr is TFuncExprBase then
            GreedyJIT(subExpr)
         else if subExpr is TProgramExpr then begin
            statementJIT:=JITStatement(TProgramExpr(subExpr), False);
            if statementJIT<>nil then begin
               block.ReplaceStatement(i, statementJIT);
               continue;
            end;
            GreedyJIT(subExpr);
         end;
      end;
   end else if expr.ClassType=TAssignExpr then begin
      assignExpr:=TAssignExpr(expr);
      if assignExpr.Left.Typ.UnAliasedType.ClassType=TBaseFloatSymbol then begin
         floatJIT:=JITFloat(assignExpr.Right);
         if floatJIT<>nil then begin
            assignExpr.Right.Free;
            assignExpr.Right:=floatJIT;
         end;
      end;
   end else if expr is TFuncExprBase then begin
      funcExpr:=TFuncExprBase(expr);
      funcSym:=funcExpr.FuncSym;
      if (funcSym is TSourceFuncSymbol) and not FSeenByGreedy.Contains(funcSym) then begin
         FSeenByGreedy.Add(funcSym);
         executable:=funcSym.Executable;
         if executable<>nil then begin
            execObject:=executable.GetSelf;
            if execObject is TdwsProgram then
               QueueGreed(TdwsProgram(execObject));
         end;
      end else if funcSym is TSourceMethodSymbol then begin
         composite:=TSourceMethodSymbol(funcSym).StructSymbol;
         while (composite<>nil) and not FSeenByGreedy.Contains(composite) do begin
            FSeenByGreedy.Add(composite);
            for i:=0 to composite.Members.Count-1 do begin
               if composite.Members[i] is TSourceMethodSymbol then begin
                  funcSym:=TSourceMethodSymbol(composite.Members[i]);
                  executable:=funcSym.Executable;
                  if executable<>nil then begin
                     execObject:=executable.GetSelf;
                     if execObject is TdwsProgram then
                        QueueGreed(TdwsProgram(execObject));
                  end;
               end;
            end;
            composite:=composite.Parent;
         end;
      end;
      GreedyJITParameters(funcExpr);
   end else begin
      for i:=0 to expr.SubExprCount-1 do begin
         subExpr:=expr.SubExpr[i];
         if subExpr<>nil then
            GreedyJIT(subExpr);
      end;
   end;
end;

// GreedyJITParameters
//
procedure TdwsJIT.GreedyJITParameters(funcExpr : TFuncExprBase);
var
   i : Integer;
   jitted : TJITTedTypedExpr;
   funcSym : TFuncSymbol;
   p : TParamSymbol;
   paramTyp : TTypeSymbol;
   argExpr : TExprBase;
begin
   funcSym:=funcExpr.FuncSym;
   if funcSym=nil then Exit;
   if funcExpr is TMagicFuncExpr then begin
      for i:=0 to funcExpr.Args.Count-1 do begin
         p:=funcSym.Params[i];
         if p.ClassType<>TParamSymbol then continue;
         argExpr:=funcExpr.Args[i];
         if not (argExpr is TJITTedTypedExpr) then begin
            paramTyp:=p.Typ.UnAliasedType;
            if paramTyp is TBaseIntegerSymbol then
               jitted:=JITInteger(argExpr as TTypedExpr)
            else if paramTyp is TBaseFloatSymbol then
               jitted:=JITFloat(argExpr as TTypedExpr)
            else jitted:=nil;
            if jitted<>nil then begin
               funcExpr.Args[i].Free;
               funcExpr.Args[i]:=jitted;
            end else GreedyJIT(argExpr);
         end;
      end;
   end else begin
      for i:=0 to funcExpr.SubExprCount-1 do begin
         argExpr:=funcExpr.SubExpr[i];
         if argExpr is TFuncExprBase then
            QueueGreed(argExpr);
      end;
   end;
end;

// DeQueueGreed
//
procedure TdwsJIT.DeQueueGreed;
var
   greed : TQueuedJITGreed;
begin
   while FQueuedGreed<>nil do begin
      greed:=FQueuedGreed;
      FQueuedGreed:=greed.Next;
      try
         if greed.Expr<>nil then
            GreedyJIT(greed.Expr)
         else GreedyJIT(greed.Prog);
      finally
         greed.Free;
      end;
   end;
end;

// QueueGreed
//
procedure TdwsJIT.QueueGreed(expr : TExprBase);
var
   greed : TQueuedJITGreed;
begin
   greed:=TQueuedJITGreed.Create;
   greed.Expr:=expr;
   greed.Next:=FQueuedGreed;
   FQueuedGreed:=greed;
end;

// QueueGreed
//
procedure TdwsJIT.QueueGreed(prog : TdwsProgram);
var
   greed : TQueuedJITGreed;
begin
   greed:=TQueuedJITGreed.Create;
   greed.Prog:=prog;
   greed.Next:=FQueuedGreed;
   FQueuedGreed:=greed;
end;

// GreedyJIT
//
procedure TdwsJIT.GreedyJIT(prog : TdwsProgram);
var
   statementJIT : TJITTedProgramExpr;
begin
   if prog.Expr is TProgramExpr then begin
      statementJIT:=JITStatement(TProgramExpr(prog.Expr), True);
      if statementJIT<>nil then begin
         prog.Expr.Free;
         prog.Expr:=statementJIT;
      end;
   end;
   DeQueueGreed;
end;

// ------------------
// ------------------ TJITTedProgramExpr ------------------
// ------------------

// Create
//
constructor TJITTedProgramExpr.Create(original : TProgramExpr; const code : TdwsJITCodeBlock);
begin
   inherited Create(original.ScriptPos);
   FCode:=code;
   FOriginal:=original;
   FOriginal.IncRefCount;
end;

// Destroy
//
destructor TJITTedProgramExpr.Destroy;
begin
   inherited;
   FOriginal.Free;
   FCode.Free;
end;

// EvalNoResult
//
procedure TJITTedProgramExpr.EvalNoResult(exec : TdwsExecution);
begin
   FOriginal.EvalNoResult(exec);
end;

// GetSubExpr
//
function TJITTedProgramExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FOriginal.SubExpr[i];
end;

// GetSubExprCount
//
function TJITTedProgramExpr.GetSubExprCount : Integer;
begin
   if FCode.Steppable then
      Result:=FOriginal.SubExprCount
   else Result:=0;
end;

// ------------------
// ------------------ TJITTedTypedExpr ------------------
// ------------------

// Create
//
constructor TJITTedTypedExpr.Create(original : TTypedExpr; const code : TdwsJITCodeBlock);
begin
   inherited Create;
   FCode:=code;
   FOriginal:=original;
   FOriginal.IncRefCount;
end;

// Destroy
//
destructor TJITTedTypedExpr.Destroy;
begin
   inherited;
   FOriginal.Free;
   FCode.Free;
end;

// ScriptPos
//
function TJITTedTypedExpr.ScriptPos : TScriptPos;
begin
   Result:=Original.ScriptPos;
end;

// GetSubExpr
//
function TJITTedTypedExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FOriginal.SubExpr[i];
end;

// GetSubExprCount
//
function TJITTedTypedExpr.GetSubExprCount : Integer;
begin
   if FCode.Steppable then
      Result:=FOriginal.SubExprCount
   else Result:=0;
end;

// ------------------
// ------------------ TJITTedFloatExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TJITTedFloatExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   result:=EvalAsFloat(exec);
end;

// ------------------
// ------------------ TJITTedIntegerExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TJITTedIntegerExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   Result:=EvalAsInteger(exec);
end;

end.
