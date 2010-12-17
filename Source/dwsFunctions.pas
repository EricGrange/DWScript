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
{                                   }
{**********************************************************************}
{$I dws.inc}
unit dwsFunctions;

interface

uses
  Classes, SysUtils, dwsExprs, dwsSymbols, dwsStack, dwsStrings;

type
  TEmptyFunc = class(TInterfacedObject, ICallable)
    procedure Call(Caller: TdwsProgram; Func: TFuncSymbol);
    procedure InitSymbol(Symbol: TSymbol);
    procedure InitExpression(Expr: TExprBase);
  end;

  TFunctionPrototype = class(TInterfacedObject)
  protected
    FInfo: TProgramInfo;
  public
    destructor Destroy; override;
    procedure InitSymbol(Symbol: TSymbol); virtual;
    procedure InitExpression(Expr: TExprBase); virtual;
    property Info: TProgramInfo read FInfo;
  end;

  TAnonymousFunction = class(TFunctionPrototype, IUnknown, ICallable)
    constructor Create(FuncSym: TFuncSymbol);
    procedure Call(Caller: TdwsProgram; Func: TFuncSymbol);
    procedure Execute; virtual; abstract;
  end;

  TInternalFunction = class(TFunctionPrototype, IUnknown, ICallable)
  public
    constructor Create(Table: TSymbolTable; const FuncName: string;
                       const FuncParams: array of string; const FuncType: string;
                       const isStateLess : Boolean = False); virtual;
    procedure Call(Caller: TdwsProgram; Func: TFuncSymbol);
    procedure Execute; virtual; abstract;
  end;
  TInternalFunctionClass = class of TInternalFunction;

  TInternalMagicFunction = class(TInternalFunction)
  public
    constructor Create(Table: TSymbolTable; const FuncName: string;
                       const FuncParams: array of string; const FuncType: string;
                       const isStateLess : Boolean = False); override;
    procedure Execute; override;
    function DoEval(args : TExprBaseList) : Variant; virtual; abstract;
  end;

  TInternalMagicProcedure = class(TInternalMagicFunction)
  public
    function DoEval(args : TExprBaseList) : Variant; override;
    procedure DoEvalProc(args : TExprBaseList); virtual; abstract;
  end;

  TInternalMagicIntFunction = class(TInternalMagicFunction)
  public
    function DoEval(args : TExprBaseList) : Variant; override;
    function DoEvalAsInteger(args : TExprBaseList) : Int64; virtual; abstract;
  end;
  TInternalMagicIntFunctionClass = class of TInternalMagicIntFunction;

  TInternalMagicBoolFunction = class(TInternalMagicFunction)
  public
    function DoEval(args : TExprBaseList) : Variant; override;
    function DoEvalAsBoolean(args : TExprBaseList) : Boolean; virtual; abstract;
  end;
  TInternalMagicBoolFunctionClass = class of TInternalMagicBoolFunction;

  TInternalMagicFloatFunction = class(TInternalMagicFunction)
  public
    function DoEval(args : TExprBaseList) : Variant; override;
    procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); virtual; abstract;
  end;
  TInternalMagicFloatFunctionClass = class of TInternalMagicFloatFunction;

  TInternalMagicStringFunction = class(TInternalMagicFunction)
  public
    function DoEval(args : TExprBaseList) : Variant; override;
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); virtual; abstract;
  end;
  TInternalMagicStringFunctionClass = class of TInternalMagicStringFunction;

  TAnonymousMethod = class(TFunctionPrototype, IUnknown, ICallable)
    constructor Create(MethSym: TMethodSymbol);
    procedure Call(Caller: TdwsProgram; Func: TFuncSymbol);
    procedure Execute(var ExternalObject: TObject); virtual; abstract;
  end;

  TInternalMethod = class(TFunctionPrototype, IUnknown, ICallable)
  public
    constructor Create(MethKind: TMethodKind; Attributes: TMethodAttributes;
      bugFix: Integer; const methName: string; const MethParams: array of string;
      const MethType: string; Cls: TClassSymbol; Table: TSymbolTable);
    procedure Call(Caller: TdwsProgram; Func: TFuncSymbol);
    procedure Execute(var ExternalObject: TObject); virtual; abstract;
  end;

  TInternalInitProc = procedure (SystemTable, UnitSyms, UnitTable: TSymbolTable);

  TInternalUnit = class(TObject, IUnknown, IUnit)
  private
    FDependencies: TStrings;
    FInitProcs: TList;
    FRegisteredInternalFunctions: TList;
    FStaticSymbols: Boolean;
    FStaticTable: TStaticSymbolTable; // static symbols
  protected
    procedure SetStaticSymbols(const Value: Boolean);
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function GetDependencies: TStrings;
    function GetUnitName: string;
    procedure InitUnitTable(SystemTable, UnitSyms, UnitTable: TSymbolTable);
    function GetUnitTable(SystemTable, UnitSyms: TSymbolTable): TUnitSymbolTable;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddInternalFunction(rif: Pointer);
    procedure AddInitProc(Proc: TInternalInitProc);
    function InitStaticSymbols(SystemTable, UnitSyms: TSymbolTable): Boolean;
    procedure ReleaseStaticSymbols;
    property StaticTable: TStaticSymbolTable read FStaticTable;
    property StaticSymbols: Boolean read FStaticSymbols write SetStaticSymbols;
  end;

procedure RegisterInternalFunction(InternalFunctionClass: TInternalFunctionClass;
      const FuncName: string; const FuncParams: array of string;
      const FuncType: string; const isStateLess : Boolean = False);
procedure RegisterInternalIntFunction(InternalFunctionClass: TInternalMagicIntFunctionClass;
      const FuncName: string; const FuncParams: array of string; const isStateLess : Boolean = False);
procedure RegisterInternalBoolFunction(InternalFunctionClass: TInternalMagicBoolFunctionClass;
      const FuncName: string; const FuncParams: array of string; const isStateLess : Boolean = False);
procedure RegisterInternalFloatFunction(InternalFunctionClass: TInternalMagicFloatFunctionClass;
      const FuncName: string; const FuncParams: array of string; const isStateLess : Boolean = False);
procedure RegisterInternalStringFunction(InternalFunctionClass: TInternalMagicStringFunctionClass;
      const FuncName: string; const FuncParams: array of string; const isStateLess : Boolean = False);
procedure RegisterInternalProcedure(InternalFunctionClass: TInternalFunctionClass;
      const FuncName: string; const FuncParams: array of string);

procedure RegisterInternalInitProc(Proc: TInternalInitProc);

function dwsInternalUnit : TInternalUnit;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
   vInternalUnit : TInternalUnit;

// dwsInternalUnit
//
function dwsInternalUnit : TInternalUnit;
begin
   if not Assigned(vInternalUnit) then
      vInternalUnit:=TInternalUnit.Create;
   Result:=vInternalUnit;
end;

procedure RegisterInternalInitProc(Proc: TInternalInitProc);
begin
   dwsInternalUnit.AddInitProc(Proc);
end;

type
   TRegisteredInternalFunction = record
      InternalFunctionClass : TInternalFunctionClass;
      FuncName : String;
      FuncParams : array of String;
      FuncType : String;
      StateLess : Boolean;
   end;
   PRegisteredInternalFunction = ^TRegisteredInternalFunction;

// RegisterInternalFunction
//
procedure RegisterInternalFunction(InternalFunctionClass: TInternalFunctionClass;
                                   const FuncName: string;
                                   const FuncParams: array of string;
                                   const FuncType: string;
                                   const isStateLess : Boolean = False);
var
   i : Integer;
   rif : PRegisteredInternalFunction;
begin
   New(rif);
   rif.InternalFunctionClass := InternalFunctionClass;
   rif.FuncName := FuncName;
   rif.StateLess:=isStateLess;

   SetLength(rif.FuncParams, Length(FuncParams));

   for i := 0 to Length(FuncParams) - 1 do
      rif.FuncParams[i] := FuncParams[i];
   rif.FuncType := FuncType;

   dwsInternalUnit.AddInternalFunction(rif);
end;

// RegisterInternalIntFunction
//
procedure RegisterInternalIntFunction(InternalFunctionClass: TInternalMagicIntFunctionClass;
      const FuncName: string; const FuncParams: array of string; const isStateLess : Boolean = False);
begin
   RegisterInternalFunction(InternalFunctionClass, FuncName, FuncParams, 'Integer', isStateLess);
end;

// RegisterInternalBoolFunction
//
procedure RegisterInternalBoolFunction(InternalFunctionClass: TInternalMagicBoolFunctionClass;
      const FuncName: string; const FuncParams: array of string; const isStateLess : Boolean = False);
begin
   RegisterInternalFunction(InternalFunctionClass, FuncName, FuncParams, 'Boolean', isStateLess);
end;

// RegisterInternalFloatFunction
//
procedure RegisterInternalFloatFunction(InternalFunctionClass: TInternalMagicFloatFunctionClass;
      const FuncName: string; const FuncParams: array of string; const isStateLess : Boolean = False);
begin
   RegisterInternalFunction(InternalFunctionClass, FuncName, FuncParams, 'Float', isStateLess);
end;

// RegisterInternalStringFunction
//
procedure RegisterInternalStringFunction(InternalFunctionClass: TInternalMagicStringFunctionClass;
      const FuncName: string; const FuncParams: array of string; const isStateLess : Boolean = False);
begin
   RegisterInternalFunction(InternalFunctionClass, FuncName, FuncParams, 'String', isStateLess);
end;

// RegisterInternalProcedure
//
procedure RegisterInternalProcedure(InternalFunctionClass: TInternalFunctionClass;
      const FuncName: string; const FuncParams: array of string);
begin
   RegisterInternalFunction(InternalFunctionClass, FuncName, FuncParams, '', False);
end;

// ConvertFuncParams
//
procedure ConvertFuncParams(var Params: TParamArray; const FuncParams: array of string);

   procedure ParamSpecifier(c : Char; paramRec : PParamRec);
   begin
      paramRec.IsVarParam:=(c='@');
      paramRec.IsConstParam:=(c='&');
      paramRec.ParamName:=Copy(paramRec.ParamName, 2, MaxInt)
   end;

   procedure ParamDefaultValue(p : Integer; paramRec : PParamRec);
   begin
      SetLength(paramRec.DefaultValue, 1);
      paramRec.DefaultValue[0]:=Trim(Copy(paramRec.ParamName, p+1, MaxInt));
      paramRec.HasDefaultValue:=True;
      paramRec.ParamName:=Trim(Copy(paramRec.ParamName, 1, p-1));
   end;

var
   x, p : Integer;
   c : Char;
   paramRec : PParamRec;
begin
   SetLength(Params, Length(FuncParams) div 2);
   x:=0;
   while x<Length(FuncParams)-1 do begin
      paramRec:=@Params[x div 2];

      paramRec.ParamName:=FuncParams[x];
      c:=#0;
      if paramRec.ParamName<>'' then
         c:=paramRec.ParamName[1];

      case c of
         '@','&':
            ParamSpecifier(c, paramRec);
      else
         paramRec.IsVarParam:=False;
         paramRec.IsConstParam:=False;
      end;

      p:=Pos('=', paramRec.ParamName);
      if p>0 then
         ParamDefaultValue(p, paramRec);

      paramRec.ParamType:=FuncParams[x+1];

      Inc(x, 2);
   end;
end;

{ TEmptyFunc }

procedure TEmptyFunc.Call(Caller: TdwsProgram; Func: TFuncSymbol);
begin
end;

procedure TEmptyFunc.InitSymbol(Symbol: TSymbol);
begin
end;

procedure TEmptyFunc.InitExpression(Expr: TExprBase);
begin
end;

{ TFunctionPrototype }

destructor TFunctionPrototype.Destroy;
begin
  FInfo.Free;
  inherited;
end;

procedure TFunctionPrototype.InitSymbol(Symbol: TSymbol);
begin
end;

procedure TFunctionPrototype.InitExpression(Expr: TExprBase);
begin
end;

{ TInternalFunction }

constructor TInternalFunction.Create(Table: TSymbolTable;
  const FuncName: string; const FuncParams: array of string; const FuncType: string;
  const isStateLess : Boolean = False);
var
  sym: TFuncSymbol;
  Params: TParamArray;
begin
  ConvertFuncParams(Params, FuncParams);

  sym := TFuncSymbol.Generate(Table, FuncName, Params, FuncType);
  sym.Params.AddParent(Table);
  sym.Executable := ICallable(Self);
  sym.IsStateless:=isStateLess;
  Table.AddSymbol(sym);

  FInfo := TProgramInfo.Create;
  FInfo.Table := sym.Params;
  FInfo.FuncSym := sym;
end;

procedure TInternalFunction.Call(Caller: TdwsProgram; Func: TFuncSymbol);
begin
  FInfo.Caller := Caller;
  Execute;
end;

// ------------------
// ------------------ TInternalMagicFunction ------------------
// ------------------

// Create
//
constructor TInternalMagicFunction.Create(Table: TSymbolTable;
  const FuncName: string; const FuncParams: array of string; const FuncType: string;
  const isStateLess : Boolean = False);
var
  sym: TMagicFuncSymbol;
  Params: TParamArray;
begin
  ConvertFuncParams(Params, FuncParams);

  sym := TMagicFuncSymbol.Generate(Table, FuncName, Params, FuncType);
  sym.Params.AddParent(Table);
  sym.InternalFunction:=Self;
  sym.IsStateless:=isStateLess;
  Table.AddSymbol(sym);
end;

// Execute
//
procedure TInternalMagicFunction.Execute;
begin
   Assert(False);
end;

// ------------------
// ------------------ TInternalMagicProcedure ------------------
// ------------------

// DoEval
//
function TInternalMagicProcedure.DoEval(args : TExprBaseList) : Variant;
begin
   DoEvalProc(args);
end;

// ------------------
// ------------------ TInternalMagicIntFunction ------------------
// ------------------

// DoEval
//
function TInternalMagicIntFunction.DoEval(args : TExprBaseList) : Variant;
begin
   Result:=DoEvalAsInteger(args);
end;

// ------------------
// ------------------ TInternalMagicBoolFunction ------------------
// ------------------

// DoEval
//
function TInternalMagicBoolFunction.DoEval(args : TExprBaseList) : Variant;
begin
   Result:=DoEvalAsBoolean(args);
end;

// ------------------
// ------------------ TInternalMagicFloatFunction ------------------
// ------------------

// DoEval
//
function TInternalMagicFloatFunction.DoEval(args : TExprBaseList) : Variant;
var
   buf : Double;
begin
   DoEvalAsFloat(args, buf);
   Result:=buf;
end;

// ------------------
// ------------------ TInternalMagicStringFunction ------------------
// ------------------

// DoEval
//
function TInternalMagicStringFunction.DoEval(args : TExprBaseList) : Variant;
var
   buf : String;
begin
   DoEvalAsString(args, buf);
   Result:=buf;
end;

{ TInternalMethod }

constructor TInternalMethod.Create;
var
  sym: TMethodSymbol;
  Params: TParamArray;
begin
  ConvertFuncParams(Params, MethParams);

  sym := TMethodSymbol.Generate(Table, MethKind, Attributes, methName, Params,
    MethType, Cls);
  sym.Params.AddParent(Table);
  sym.Executable := ICallable(Self);

  // Add method to its class
  Cls.AddMethod(sym);

  FInfo := TProgramInfo.Create;
  FInfo.Table := sym.Params;
  FInfo.FuncSym := sym;
end;

procedure TInternalMethod.Call(Caller: TdwsProgram; Func: TFuncSymbol);
var
  scriptObj: IScriptObj;
  extObj: TObject;
begin
  FInfo.Caller := Caller;
  scriptObj := Info.Vars['Self'].ScriptObj;

  if Assigned(scriptObj) then
  begin
    FInfo.ScriptObj := scriptObj;
    extObj := scriptObj.ExternalObject;
    try
      Execute(extObj);
    finally
      scriptObj.ExternalObject := extObj;
      FInfo.ScriptObj := nil;
    end;
  end
  else
  begin
    // Class methods or method calls on nil-object-references
    extObj := nil;
    Execute(extObj);
  end;
end;

{ TSimpleFunction }

constructor TAnonymousFunction.Create;
begin
  FInfo := TProgramInfo.Create;
  FInfo.Table := FuncSym.Params;
  FInfo.FuncSym := FuncSym;
  FuncSym.Executable := ICallable(Self);
end;

procedure TAnonymousFunction.Call(Caller: TdwsProgram; Func: TFuncSymbol);
begin
  FInfo.Caller := Caller;
  Execute;
end;

{ TAnonymousMethod }

procedure TAnonymousMethod.Call(Caller: TdwsProgram; Func: TFuncSymbol);
var
  scriptObj: IScriptObj;
  extObj: TObject;
begin
  FInfo.Caller := Caller;
  scriptObj := Info.Vars['Self'].ScriptObj;

  if Assigned(scriptObj) then
  begin
    FInfo.ScriptObj := scriptObj;
    extObj := scriptObj.ExternalObject;
    try
      Execute(extObj);
    finally
      scriptObj.ExternalObject := extObj;
    end;
  end
  else
  begin
    // Class methods or method calls on nil-object-references
    extObj := nil;
    Execute(extObj);
  end;
end;

constructor TAnonymousMethod.Create(MethSym: TMethodSymbol);
begin
  FInfo := TProgramInfo.Create;
  FInfo.Table := MethSym.Params;
  FInfo.FuncSym := MethSym;
  MethSym.Executable := ICallable(Self);
end;

{ TInternalUnit }

procedure TInternalUnit.AddInitProc(Proc: TInternalInitProc);
begin
  FInitProcs.Add(@Proc);
end;

procedure TInternalUnit.AddInternalFunction(rif: Pointer);
begin
  FRegisteredInternalFunctions.Add(rif);
end;

function TInternalUnit._AddRef: Integer;
begin
  Result := -1;
end;

constructor TInternalUnit.Create;
begin
  FDependencies := TStringList.Create;
  FRegisteredInternalFunctions := TList.Create;
  FInitProcs := TList.Create;
  FStaticSymbols := False;
  FStaticTable := nil;
end;

destructor TInternalUnit.Destroy;
var
  i: Integer;
  rif: PRegisteredInternalFunction;
begin
  ReleaseStaticSymbols;
  FDependencies.Free;
  for i := 0 to FRegisteredInternalFunctions.Count - 1 do
  begin
    rif := PRegisteredInternalFunction(FRegisteredInternalFunctions[i]);
    Dispose(rif);
  end;
  FRegisteredInternalFunctions.Free;
  FInitProcs.Free;
  inherited;
end;

function TInternalUnit.GetDependencies: TStrings;
begin
  Result := FDependencies;
end;

function TInternalUnit.GetUnitName: string;
begin
  Result := SYS_INTERNAL;
end;

function TInternalUnit.InitStaticSymbols(SystemTable, UnitSyms: TSymbolTable): Boolean;
var
  staticParent: TStaticSymbolTable;
begin
  if not Assigned(FStaticTable) then
  begin
    if SystemTable is TStaticSymbolTable then
      staticParent := TStaticSymbolTable(SystemTable)
    else if SystemTable is TLinkedSymbolTable then
      staticParent := TLinkedSymbolTable(SystemTable).Parent
    else
      staticParent := nil;

    if Assigned(staticParent) then
    begin
      FStaticTable := TStaticSymbolTable.Create(staticParent);
      try
        InitUnitTable(SystemTable, UnitSyms, FStaticTable);
      except
        ReleaseStaticSymbols;
        raise;
      end;
    end;
  end;
  Result := Assigned(FStaticTable);
end;

procedure TInternalUnit.ReleaseStaticSymbols;
var
  s: TStaticSymbolTable;
begin
  if Assigned(FStaticTable) then
  begin
    s := FStaticTable;
    FStaticTable := nil;
    s._Release;
  end;
end;

function TInternalUnit.GetUnitTable(SystemTable, UnitSyms: TSymbolTable): TUnitSymbolTable;
begin
  if StaticSymbols and InitStaticSymbols(SystemTable, UnitSyms) then
    Result := TLinkedSymbolTable.Create(FStaticTable)
  else
  begin
    Result := TUnitSymbolTable.Create(SystemTable);
    try
      InitUnitTable(SystemTable, UnitSyms, Result);
    except
      Result.Free;
      raise;
    end;
  end;
end;

procedure TInternalUnit.InitUnitTable(SystemTable, UnitSyms, UnitTable: TSymbolTable);
var
  i: Integer;
  rif: PRegisteredInternalFunction;
begin
  for i := 0 to FInitProcs.Count - 1 do
    TInternalInitProc(FInitProcs[i])(SystemTable, UnitSyms, UnitTable);

  for i := 0 to FRegisteredInternalFunctions.Count - 1 do
  begin
    rif := PRegisteredInternalFunction(FRegisteredInternalFunctions[i]);
    try
      rif.InternalFunctionClass.Create(UnitTable, rif.FuncName, rif.FuncParams,
                                       rif.FuncType, rif.StateLess);
    except
      on e: Exception do
        raise
          Exception.CreateFmt('AddInternalFunctions failed on %s'#13#10'%s',
                              [rif.FuncName, e.Message]);
    end;
  end;
end;

function TInternalUnit.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := 0;
end;

function TInternalUnit._Release: Integer;
begin
  Result := -1;
end;

procedure TInternalUnit.SetStaticSymbols(const Value: Boolean);
begin
  FStaticSymbols := Value;
  if not FStaticSymbols then
    ReleaseStaticSymbols;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

finalization

   FreeAndNil(vInternalUnit);

end.
