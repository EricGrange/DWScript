{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. For other initial contributors, see contributors.txt   }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsConnectorExprs;

{$I dws.inc}

interface

uses
   Variants, SysUtils,
   dwsUtils, dwsDataContext, dwsStack, dwsXPlatform, dwsErrors, dwsStrings,
   dwsExprs, dwsExprList, dwsSymbols, dwsUnitSymbols, dwsCoreExprs;

type

   TConnectorCallExpr = class(TPosDataExpr)
      private
         FArgs: TTightList;
         FBaseExpr: TTypedExpr;
         FConnectorArgs: TConnectorArgs;
         FConnectorCall: IConnectorCall;
         FConnectorParams: TConnectorParamArray;
         FIsInstruction: Boolean;
         FIsWritable: Boolean;
         FIsIndex: Boolean;
         FName: UnicodeString;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const aScriptPos: TScriptPos; const Name: UnicodeString;
                            BaseExpr: TTypedExpr; IsWrite: Boolean = True; IsIndex: Boolean = False);
         destructor Destroy; override;

         function AssignConnectorSym(prog : TdwsProgram; const connectorType : IConnectorType) : Boolean;
         procedure AddArg(expr : TTypedExpr);
         function Eval(exec : TdwsExecution) : Variant; override;
         function IsWritable : Boolean; override;

         procedure GetDataPtr(exec : TdwsExecution; var result : IDataContext); override;

         property BaseExpr : TTypedExpr read FBaseExpr write FBaseExpr;
         property IsWrite : Boolean read FIsWritable write FIsWritable;
         property IsIndex : Boolean read FIsIndex write FIsIndex;
         property ConnectorCall : IConnectorCall read FConnectorCall write FConnectorCall;
   end;

   TConnectorReadExpr = class sealed (TPosDataExpr)
      private
         FBaseExpr: TTypedExpr;
         FConnectorMember: IConnectorMember;
         FName: UnicodeString;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const aScriptPos: TScriptPos; const Name: UnicodeString;
                            BaseExpr: TTypedExpr);
         destructor Destroy; override;

         function AssignConnectorSym(ConnectorType : IConnectorType) : Boolean;

         procedure GetDataPtr(exec : TdwsExecution; var result : IDataContext); override;

         property BaseExpr : TTypedExpr read FBaseExpr write FBaseExpr;

         property ConnectorMember : IConnectorMember read FConnectorMember write FConnectorMember;
  end;

   TConnectorWriteExpr = class sealed (TTypedExpr)
      private
         FBaseExpr: TTypedExpr;
         FValueExpr: TTypedExpr;
         FConnectorMember: IConnectorMember;
         FName: UnicodeString;
         FScriptPos : TScriptPos;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const scriptPos: TScriptPos; const Name: UnicodeString;
                            BaseExpr, ValueExpr: TTypedExpr);
         destructor Destroy; override;

         function ScriptPos : TScriptPos; override;
         function AssignConnectorSym(prog : TdwsProgram; const connectorType : IConnectorType) : Boolean;
         function  Eval(exec : TdwsExecution) : Variant; override;
         procedure EvalNoResult(exec : TdwsExecution); override;

         property ConnectorMember : IConnectorMember read FConnectorMember write FConnectorMember;
         property BaseExpr : TTypedExpr read FBaseExpr write FBaseExpr;
         property ValueExpr : TTypedExpr read FValueExpr write FValueExpr;
   end;

   TConnectorForInExpr = class sealed (TNoResultExpr)
      private
         FInExpr : TTypedExpr;
         FLoopVarExpr : TTypedExpr;
         FDoExpr : TProgramExpr;
         FConnectorEnumerator : IConnectorEnumerator;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(const scriptPos : TScriptPos;
                            const enumerator : IConnectorEnumerator;
                            loopVarExpr, inExpr : TTypedExpr; doExpr : TProgramExpr);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;

         property ConnectorEnumerator : IConnectorEnumerator read FConnectorEnumerator write FConnectorEnumerator;
         property InExpr : TTypedExpr read FInExpr write FInExpr;
         property LoopVarExpr : TTypedExpr read FLoopVarExpr write FLoopVarExpr;
         property DoExpr : TProgramExpr read FDoExpr write FDoExpr;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TConnectorExpr ------------------
// ------------------

constructor TConnectorCallExpr.Create(Prog: TdwsProgram; const aScriptPos: TScriptPos;
  const Name: UnicodeString; BaseExpr: TTypedExpr; IsWrite: Boolean; IsIndex: Boolean);
begin
  inherited Create(Prog, aScriptPos, nil);
  FName := Name;
  FBaseExpr := BaseExpr;
  FIsInstruction := IsWrite;
  FIsWritable := IsWrite;
  FIsIndex := IsIndex;
end;

destructor TConnectorCallExpr.Destroy;
begin
  FBaseExpr.Free;
  FArgs.Clean;
  inherited;
end;

// AddArg
//
procedure TConnectorCallExpr.AddArg(expr : TTypedExpr);
begin
   FArgs.Add(expr);
end;

// AssignConnectorSym
//
function TConnectorCallExpr.AssignConnectorSym(prog : TdwsProgram; const connectorType : IConnectorType): Boolean;
var
   i : Integer;
   typSym, paramTyp : TTypeSymbol;
   arg : TTypedExpr;
begin
  // Prepare the parameter information array to query the connector symbol
   if FArgs.Count>64 then
      prog.CompileMsgs.AddCompilerErrorFmt(ScriptPos, CPE_ConnectorTooManyArguments, [FArgs.Count]);

   SetLength(FConnectorParams, FArgs.Count);
   for i:=0 to FArgs.Count-1 do begin
      arg:=TTypedExpr(FArgs.List[i]);
      FConnectorParams[i].IsVarParam:=(arg is TDataExpr) and TDataExpr(arg).IsWritable
                                      and not (arg.Typ is TArraySymbol);
      FConnectorParams[i].TypSym:=arg.Typ;
   end;

   if not connectorType.AcceptsParams(FConnectorParams) then begin
      if FName<>'' then begin
         prog.CompileMsgs.AddCompilerErrorFmt(ScriptPos, CPE_MethodConnectorParams,
                                              [FName, connectorType.ConnectorCaption])
      end else begin
         prog.CompileMsgs.AddCompilerErrorFmt(ScriptPos, CPE_ConnectorParams,
                                              [connectorType.ConnectorCaption]);
      end;
   end;

   // Ask the connector symbol if such a method exists
   try
      if FIsIndex then
         FConnectorCall := ConnectorType.HasIndex(FName, FConnectorParams, typSym, FIsWritable)
      else begin
         FIsWritable := False;
         FConnectorCall := ConnectorType.HasMethod(FName, FConnectorParams, typSym);
      end;
   except
      on E: ECompileException do begin
         prog.CompileMsgs.AddCompilerError(ScriptPos, E.Message);
         Exit(False);
      end else raise;
   end;

   Result := Assigned(FConnectorCall);
   if Result then begin
      // Prepare the arguments for the method call
      SetLength(FConnectorArgs, FArgs.Count);
      for i:=0 to FArgs.Count-1 do begin
         paramTyp:=FConnectorParams[i].TypSym;
         if paramTyp<>nil then
            SetLength(FConnectorArgs[i], paramTyp.Size);
      end;
      FTyp:=typSym;
   end else begin
      prog.CompileMsgs.AddCompilerErrorFmt(ScriptPos, CPE_ConnectorCall,
                                           [FName, connectorType.ConnectorCaption])
   end;
end;

// Eval
//
function TConnectorCallExpr.Eval(exec : TdwsExecution): Variant;
var
   sourcePtr : IDataContext;
   x : Integer;
   arg : TTypedExpr;
   argTyp : TTypeSymbol;
   buf : Variant;
   obj : IScriptObj;
   locData : IDataContext;
   resultData : TData;
begin
   if exec.IsDebugging then
      exec.Debugger.EnterFunc(exec, Self);

   // Call function
   try
      for x:=0 to High(FConnectorArgs) do begin
         arg:=TTypedExpr(FArgs.List[x]);
         argTyp:=FConnectorParams[x].TypSym;
         if argTyp.Size = 1 then begin
            if argTyp is TDynamicArraySymbol then begin
               arg.EvalAsScriptObj(exec, obj);
               FConnectorArgs[x][0]:=VarArrayOf(TScriptDynamicArray(obj.GetSelf).AsData);
            end else arg.EvalAsVariant(exec, FConnectorArgs[x][0]);
         end else begin
            sourcePtr := TDataExpr(arg).DataPtr[exec];
            sourcePtr.CopyData(FConnectorArgs[x], 0, argTyp.Size);
         end;
      end;

      try
         // The call itself
         if FConnectorCall.NeedDirectReference then
            resultData := FConnectorCall.Call(TDataExpr(FBaseExpr).DataPtr[exec].AsPVariant(0)^, FConnectorArgs)
         else begin
            FBaseExpr.EvalAsVariant(exec, buf);
            resultData := FConnectorCall.Call(buf, FConnectorArgs);
         end;
      except
         on e: EScriptError do begin
            EScriptError(e).ScriptPos:=ScriptPos;
            raise;
         end
      else
         exec.SetScriptError(Self);
         raise;
      end;

      for x:=0 to High(FConnectorArgs) do begin
         if FConnectorParams[x].IsVarParam then begin
            exec.DataContext_Create(FConnectorArgs[x], 0, locData);
            TDataExpr(FArgs.List[x]).AssignData(exec, locData);
         end;
      end;

   finally
      if exec.IsDebugging then
         exec.Debugger.LeaveFunc(exec, Self);
   end;

   if Length(resultData)>0 then
      Result := resultData[0]
   else VarClear(Result);
end;

// GetDataPtr
//
procedure TConnectorCallExpr.GetDataPtr(exec : TdwsExecution; var result : IDataContext);
begin
   result.AsVariant[0]:=Eval(exec);
end;

// GetSubExpr
//
function TConnectorCallExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i=0 then
      Result:=BaseExpr
   else Result:=TExprBase(FArgs.List[i-1]);
end;

// IsWritable
//
function TConnectorCallExpr.IsWritable : Boolean;
begin
   Result:=FIsWritable;
end;

// GetSubExprCount
//
function TConnectorCallExpr.GetSubExprCount : Integer;
begin
   Result:=FArgs.Count+1;
end;

// ------------------
// ------------------ TConnectorReadExpr ------------------
// ------------------

constructor TConnectorReadExpr.Create(Prog: TdwsProgram; const aScriptPos: TScriptPos;
  const Name: UnicodeString; BaseExpr: TTypedExpr);
begin
  inherited Create(Prog, aScriptPos, nil);
  FName := Name;
  FBaseExpr := BaseExpr;
end;

destructor TConnectorReadExpr.Destroy;
begin
  FBaseExpr.Free;
  inherited;
end;

function TConnectorReadExpr.AssignConnectorSym(
  ConnectorType: IConnectorType): Boolean;
begin
  FConnectorMember := ConnectorType.HasMember(FName, FTyp,False);
  Result := Assigned(FConnectorMember);
end;

// GetDataPtr
//
procedure TConnectorReadExpr.GetDataPtr(exec : TdwsExecution; var result : IDataContext);
var
   base : Variant;
   resultData : TData;
begin
   try
      FBaseExpr.EvalAsVariant(exec, base);
      resultData:=FConnectorMember.Read(base);
   except
      on e: EScriptError do begin
         EScriptError(e).ScriptPos:=ScriptPos;
         raise;
      end
   else
      exec.SetScriptError(Self);
      raise;
   end;

   exec.DataContext_Create(resultData, 0, result);
end;

// GetSubExpr
//
function TConnectorReadExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FBaseExpr
end;

// GetSubExprCount
//
function TConnectorReadExpr.GetSubExprCount : Integer;
begin
   Result:=1;
end;

// ------------------
// ------------------ TConnectorWriteExpr ------------------
// ------------------

constructor TConnectorWriteExpr.Create(prog : TdwsProgram; const scriptPos: TScriptPos;
  const Name: UnicodeString; BaseExpr, ValueExpr: TTypedExpr);
begin
   inherited Create;
   FScriptPos:=scriptPos;
   FName:=Name;
   FBaseExpr:=BaseExpr;
   FValueExpr:=ValueExpr;
end;

// Destroy
//
destructor TConnectorWriteExpr.Destroy;
begin
   FBaseExpr.Free;
   FValueExpr.Free;
   inherited;
end;

// ScriptPos
//
function TConnectorWriteExpr.ScriptPos : TScriptPos;
begin
   Result:=FScriptPos;
end;

// AssignConnectorSym
//
function TConnectorWriteExpr.AssignConnectorSym(prog : TdwsProgram; const connectorType : IConnectorType) : Boolean;
var
   memberTyp : TTypeSymbol;
begin
   FConnectorMember := ConnectorType.HasMember(FName, memberTyp, True);
   Result := Assigned(FConnectorMember);
   if Result and not (Assigned(memberTyp) and Assigned(FValueExpr.Typ) and memberTyp.IsCompatible(FValueExpr.Typ)) then
      Prog.CompileMsgs.AddCompilerError(FScriptPos, CPE_ConnectorTypeMismatch);
end;

// Eval
//
function TConnectorWriteExpr.Eval(exec : TdwsExecution) : Variant;
begin
   EvalNoResult(exec);
end;

// EvalNoResult
//
procedure TConnectorWriteExpr.EvalNoResult(exec : TdwsExecution);
var
   dat : TData;
   tmp : Variant;
   base : PVariant;
begin
   if (FBaseExpr is TVarExpr) or (FBaseExpr.Typ.Size>1) then
      base:=TDataExpr(FBaseExpr).DataPtr[exec].AsPVariant(0)
   else begin
      FBaseExpr.EvalAsVariant(exec, tmp);
      base:=@tmp;
   end;

//  if FValueExpr is TDataExpr then
//    dat := TDataExpr(FValueExpr).GetData(exec)
//  else
//  begin
    SetLength(dat, 1);
    FValueExpr.EvalAsVariant(exec, dat[0]);
//  end;

   try
      FConnectorMember.Write(base^, dat);
   except
      on e: EScriptError do begin
         EScriptError(e).ScriptPos:=ScriptPos;
         raise;
      end
   else
      exec.SetScriptError(Self);
      raise;
   end;
end;

// GetSubExpr
//
function TConnectorWriteExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i=0 then
      Result:=FBaseExpr
   else Result:=FValueExpr;
end;

// GetSubExprCount
//
function TConnectorWriteExpr.GetSubExprCount : Integer;
begin
   Result:=2;
end;

// ------------------
// ------------------ TConnectorForInExpr ------------------
// ------------------

// Create
//
constructor TConnectorForInExpr.Create(const scriptPos : TScriptPos;
             const enumerator : IConnectorEnumerator;
             loopVarExpr, inExpr : TTypedExpr; doExpr : TProgramExpr);
begin
   inherited Create(scriptPos);
   Assert(loopVarExpr is TVarExpr);
   FConnectorEnumerator:=enumerator;
   FLoopVarExpr:=loopVarExpr;
   FInExpr:=inExpr;
   FDoExpr:=doExpr;
end;

// Destroy
//
destructor TConnectorForInExpr.Destroy;
begin
   inherited;
   FLoopVarExpr.Free;
   FInExpr.Free;
   FDoExpr.Free;
end;

// GetSubExpr
//
function TConnectorForInExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   case i of
      0 : Result:=LoopVarExpr;
      1 : Result:=InExpr;
   else
      Result:=DoExpr;
   end;
end;

// GetSubExprCount
//
function TConnectorForInExpr.GetSubExprCount : Integer;
begin
   Result:=3;
end;

// EvalNoResult
//
procedure TConnectorForInExpr.EvalNoResult(exec : TdwsExecution);
var
   base : Variant;
   item : TData;
   enumData : IUnknown;
begin
   FInExpr.EvalAsVariant(exec, base);
   enumData:=ConnectorEnumerator.NewEnumerator(base, nil);
   if enumData<>nil then begin
      SetLength(item, 1);
      while ConnectorEnumerator.Step(enumData, item) do begin
         TVarExpr(FLoopVarExpr).AssignValue(exec, item[0]);
         exec.DoStep(DoExpr);
         DoExpr.EvalNoResult(exec);
         if exec.Status<>esrNone then begin
            case exec.Status of
               esrBreak : begin
                  exec.Status:=esrNone;
                  break;
               end;
               esrContinue :
                  exec.Status:=esrNone;
               esrExit : Exit;
            end;
         end;
         exec.DoStep(Self);
      end;
   end;
end;

end.
