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
  Classes, SysUtils, dwsExprs, dwsSymbols, dwsStack, dwsStrings, dwsTokenizer,
  dwsOperators, dwsUtils;

type

   TIdwsUnitFlag = (ufImplicitUse, ufOwnsSymbolTable);
   TIdwsUnitFlags = set of TIdwsUnitFlag;

   // Interface for units
   IdwsUnit = interface
      ['{8D534D12-4C6B-11D5-8DCB-0000216D9E86}']
      function GetUnitName : String;
      function GetUnitTable(systemTable, unitSyms : TSymbolTable; operators : TOperators) : TUnitSymbolTable;
      function GetDependencies : TStrings;
      function GetUnitFlags : TIdwsUnitFlags;
   end;

   TIdwsUnitList = class(TSimpleList<IdwsUnit>)
      function IndexOf(const unitName : String) : Integer; overload;
      function IndexOf(const aUnit : IdwsUnit) : Integer; overload;
      procedure AddUnits(list : TIdwsUnitList);
   end;

   TEmptyFunc = class(TInterfacedObject, ICallable)
      public
         procedure Call(exec: TdwsProgramExecution; func: TFuncSymbol);
         procedure InitSymbol(Symbol: TSymbol);
         procedure InitExpression(Expr: TExprBase);
   end;

   TFunctionPrototype = class(TInterfacedObject)
      private
         FFuncSymbol : TFuncSymbol;
      public
         procedure InitSymbol(Symbol: TSymbol); virtual;
         procedure InitExpression(Expr: TExprBase); virtual;
         procedure Call(exec: TdwsProgramExecution; func: TFuncSymbol); virtual; abstract;
         property FuncSymbol : TFuncSymbol read FFuncSymbol;
   end;

   TAnonymousFunction = class(TFunctionPrototype, IUnknown, ICallable)
      public
         constructor Create(FuncSym: TFuncSymbol);
         procedure Call(exec: TdwsProgramExecution; func: TFuncSymbol); override;
         procedure Execute(info : TProgramInfo); virtual; abstract;
   end;

   TInternalFunction = class(TFunctionPrototype, IUnknown, ICallable)
      public
         constructor Create(Table: TSymbolTable; const FuncName: string;
                            const FuncParams: array of string; const FuncType: string;
                            const isStateLess : Boolean = False); virtual;
         procedure Call(exec: TdwsProgramExecution; func: TFuncSymbol); override;
         procedure Execute(info : TProgramInfo); virtual; abstract;
   end;
   TInternalFunctionClass = class of TInternalFunction;

   TInternalMagicFunction = class(TInternalFunction)
      public
         constructor Create(Table: TSymbolTable; const FuncName: string;
                            const FuncParams: array of string; const FuncType: string;
                            const isStateLess : Boolean = False); override;
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
      public
         constructor Create(MethSym: TMethodSymbol);
         procedure Call(exec: TdwsProgramExecution; func: TFuncSymbol); override;
         procedure Execute(info : TProgramInfo; var ExternalObject: TObject); virtual; abstract;
   end;

   TInternalMethod = class(TFunctionPrototype, IUnknown, ICallable)
      public
         constructor Create(MethKind: TMethodKind; Attributes: TMethodAttributes;
                            const methName: string; const MethParams: array of string;
                            const MethType: string; Cls: TClassSymbol;
                            aVisibility : TdwsVisibility;
                            Table: TSymbolTable);
         procedure Call(exec: TdwsProgramExecution; func: TFuncSymbol); override;
         procedure Execute(info : TProgramInfo; var ExternalObject: TObject); virtual; abstract;
   end;

   TInternalInitProc = procedure (systemTable, unitSyms, unitTable : TSymbolTable;
                                  operators : TOperators);

   TInternalUnit = class(TObject, IUnknown, IdwsUnit)
      private
         FDependencies : TStrings;
         FPreInitProcs : array of TInternalInitProc;
         FPostInitProcs : array of TInternalInitProc;
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
         procedure InitUnitTable(systemTable, unitSyms, unitTable : TSymbolTable; operators : TOperators);
         function GetUnitTable(systemTable, unitSyms : TSymbolTable; operators : TOperators) : TUnitSymbolTable;
         function GetUnitFlags : TIdwsUnitFlags;

      public
         constructor Create;
         destructor Destroy; override;

         procedure AddInternalFunction(rif : Pointer);
         procedure AddPreInitProc(proc : TInternalInitProc);
         procedure AddPostInitProc(proc : TInternalInitProc);

         function InitStaticSymbols(SystemTable, UnitSyms: TSymbolTable; operators : TOperators): Boolean;
         procedure ReleaseStaticSymbols;

         property StaticTable : TStaticSymbolTable read FStaticTable;
         property StaticSymbols : Boolean read FStaticSymbols write SetStaticSymbols;
   end;

   TSourceUnit = class(TInterfacedObject, IdwsUnit)
      private
         FDependencies : TStrings;
         FSymbol : TUnitSymbol;

      protected

      public
         constructor Create(const unitName : String; rootTable : TSymbolTable);
         destructor Destroy; override;

         function GetUnitName : String;
         function GetUnitTable(systemTable, unitSyms : TSymbolTable; operators : TOperators) : TUnitSymbolTable;
         function GetDependencies : TStrings;
         function GetUnitFlags : TIdwsUnitFlags;

         property Symbol : TUnitSymbol read FSymbol write FSymbol;
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

procedure RegisterInternalPreInitProc(Proc: TInternalInitProc);
procedure RegisterInternalPostInitProc(Proc: TInternalInitProc);

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

procedure RegisterInternalPreInitProc(Proc: TInternalInitProc);
begin
   dwsInternalUnit.AddPreInitProc(Proc);
end;

procedure RegisterInternalPostInitProc(Proc: TInternalInitProc);
begin
   dwsInternalUnit.AddPostInitProc(Proc);
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

procedure TEmptyFunc.Call(exec: TdwsProgramExecution; func: TFuncSymbol);
begin
end;

procedure TEmptyFunc.InitSymbol(Symbol: TSymbol);
begin
end;

procedure TEmptyFunc.InitExpression(Expr: TExprBase);
begin
end;

{ TFunctionPrototype }

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
   params: TParamArray;
begin
   ConvertFuncParams(Params, FuncParams);

   sym := TFuncSymbol.Generate(Table, FuncName, Params, FuncType);
   sym.Params.AddParent(Table);
   sym.Executable := ICallable(Self);
   sym.IsStateless:=isStateLess;
   FFuncSymbol:=sym;
   Table.AddSymbol(sym);
end;

// Call
//
procedure TInternalFunction.Call(exec: TdwsProgramExecution; func: TFuncSymbol);
var
   info : TProgramInfo;
begin
   info:=exec.AcquireProgramInfo(func);
   try
      Execute(info);
   finally
      exec.ReleaseProgramInfo(info);
   end;
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

constructor TInternalMethod.Create(MethKind: TMethodKind; Attributes: TMethodAttributes;
                   const methName: string; const MethParams: array of string;
                   const MethType: string; Cls: TClassSymbol;
                   aVisibility : TdwsVisibility;
                   Table: TSymbolTable);
var
  sym: TMethodSymbol;
  Params: TParamArray;
begin
  ConvertFuncParams(Params, MethParams);

  sym := TMethodSymbol.Generate(Table, MethKind, Attributes, methName, Params,
                                MethType, Cls, aVisibility);
  sym.Params.AddParent(Table);
  sym.Executable := ICallable(Self);

  // Add method to its class
  Cls.AddMethod(sym);
end;

procedure TInternalMethod.Call(exec: TdwsProgramExecution; func: TFuncSymbol);
var
   scriptObj: IScriptObj;
   extObj: TObject;
   info : TProgramInfo;
begin
   info:=exec.AcquireProgramInfo(func);
   try
      scriptObj := Info.Vars[SYS_SELF].ScriptObj;

      if Assigned(scriptObj) then begin
         info.ScriptObj := scriptObj;
         extObj := scriptObj.ExternalObject;
         try
            Execute(info, extObj);
         finally
            scriptObj.ExternalObject := extObj;
            info.ScriptObj := nil;
         end;
      end else begin
         // Class methods or method calls on nil-object-references
         extObj := nil;
         Execute(info, extObj);
      end;
   finally
      exec.ReleaseProgramInfo(info);
   end;
end;

{ TAnonymousFunction }

constructor TAnonymousFunction.Create(FuncSym: TFuncSymbol);
begin
   FuncSym.Executable := ICallable(Self);
end;

// Call
//
procedure TAnonymousFunction.Call(exec: TdwsProgramExecution; func: TFuncSymbol);
var
   info : TProgramInfo;
begin
   info:=exec.AcquireProgramInfo(func);
   try
      Execute(info);
   finally
      exec.ReleaseProgramInfo(info);
   end;
end;

{ TAnonymousMethod }

constructor TAnonymousMethod.Create(MethSym: TMethodSymbol);
begin
   MethSym.Executable := ICallable(Self);
end;

procedure TAnonymousMethod.Call(exec: TdwsProgramExecution; func: TFuncSymbol);
var
   info : TProgramInfo;
   scriptObj : IScriptObj;
   extObj : TObject;
begin
   info:=exec.AcquireProgramInfo(func);
   try
      scriptObj:=info.Vars[SYS_SELF].ScriptObj;

      if Assigned(scriptObj) then begin
         info.ScriptObj := scriptObj;
         extObj := scriptObj.ExternalObject;
         try
            Execute(info, extObj);
         finally
            scriptObj.ExternalObject := extObj;
         end;
      end else begin
         // Class methods or method calls on nil-object-references
         extObj := nil;
         Execute(info, extObj);
      end;
   finally
      exec.ReleaseProgramInfo(info);
   end;
end;

// ------------------
// ------------------ TInternalUnit ------------------
// ------------------

constructor TInternalUnit.Create;
begin
   FDependencies := TStringList.Create;
   FRegisteredInternalFunctions := TList.Create;
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
   for i := 0 to FRegisteredInternalFunctions.Count - 1 do begin
      rif := PRegisteredInternalFunction(FRegisteredInternalFunctions[i]);
      Dispose(rif);
   end;
   FRegisteredInternalFunctions.Free;
   inherited;
end;

// AddPreInitProc
//
procedure TInternalUnit.AddPreInitProc(proc : TInternalInitProc);
var
   n : Integer;
begin
   n:=Length(FPreInitProcs);
   SetLength(FPreInitProcs, n+1);
   FPreInitProcs[n]:=proc;
end;

// AddPostInitProc
//
procedure TInternalUnit.AddPostInitProc(proc : TInternalInitProc);
var
   n : Integer;
begin
   n:=Length(FPostInitProcs);
   SetLength(FPostInitProcs, n+1);
   FPostInitProcs[n]:=proc;
end;

procedure TInternalUnit.AddInternalFunction(rif: Pointer);
begin
  FRegisteredInternalFunctions.Add(rif);
end;

function TInternalUnit._AddRef: Integer;
begin
  Result := -1;
end;

function TInternalUnit.GetDependencies: TStrings;
begin
  Result := FDependencies;
end;

function TInternalUnit.GetUnitName: string;
begin
  Result := SYS_INTERNAL;
end;

function TInternalUnit.InitStaticSymbols(SystemTable, UnitSyms: TSymbolTable; operators : TOperators): Boolean;
var
   staticParent: TStaticSymbolTable;
begin
   if not Assigned(FStaticTable) then begin
      if SystemTable is TStaticSymbolTable then
         staticParent := TStaticSymbolTable(SystemTable)
      else if SystemTable is TLinkedSymbolTable then
         staticParent := TLinkedSymbolTable(SystemTable).Parent
      else staticParent := nil;

      if Assigned(staticParent) then begin
         FStaticTable := TStaticSymbolTable.Create(staticParent);
         try
            InitUnitTable(SystemTable, UnitSyms, FStaticTable, operators);
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

function TInternalUnit.GetUnitTable(systemTable, unitSyms : TSymbolTable; operators : TOperators) : TUnitSymbolTable;
begin
  if StaticSymbols and InitStaticSymbols(SystemTable, UnitSyms, operators) then
    Result := TLinkedSymbolTable.Create(FStaticTable)
  else
  begin
    Result := TUnitSymbolTable.Create(SystemTable);
    try
      InitUnitTable(SystemTable, UnitSyms, Result, operators);
    except
      Result.Free;
      raise;
    end;
  end;
end;

// GetUnitFlags
//
function TInternalUnit.GetUnitFlags : TIdwsUnitFlags;
begin
   Result:=[ufImplicitUse];
end;

procedure TInternalUnit.InitUnitTable(systemTable, unitSyms, unitTable : TSymbolTable; operators : TOperators);
var
   i : Integer;
   rif : PRegisteredInternalFunction;
begin
   for i := 0 to High(FPreInitProcs) do
      FPreInitProcs[i](SystemTable, UnitSyms, UnitTable, operators);

   for i := 0 to FRegisteredInternalFunctions.Count - 1 do begin
      rif := PRegisteredInternalFunction(FRegisteredInternalFunctions[i]);
      try
         rif.InternalFunctionClass.Create(UnitTable, rif.FuncName, rif.FuncParams,
                                       rif.FuncType, rif.StateLess);
      except
         on e: Exception do
            raise Exception.CreateFmt('AddInternalFunctions failed on %s'#13#10'%s',
                                      [rif.FuncName, e.Message]);
      end;
   end;

   for i := 0 to High(FPostInitProcs) do
      FPostInitProcs[i](SystemTable, UnitSyms, UnitTable, operators);
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

// ------------------
// ------------------ TIdwsUnitList ------------------
// ------------------

// IndexOf (name)
//
function TIdwsUnitList.IndexOf(const unitName : String) : Integer;
begin
   for Result:=0 to Count-1 do
      if SameText(Items[Result].GetUnitName, unitName) then
         Exit;
   Result:=-1;
end;

// AddUnits
//
procedure TIdwsUnitList.AddUnits(list : TIdwsUnitList);
var
   i : Integer;
begin
   for i:=0 to list.Count-1 do
      Add(list[i]);
end;

// IndexOf (IdwsUnit)
//
function TIdwsUnitList.IndexOf(const aUnit : IdwsUnit) : Integer;
begin
   for Result:=0 to Count-1 do
      if Items[Result]=aUnit then
         Exit;
   Result:=-1;
end;


// ------------------
// ------------------ TSourceUnit ------------------
// ------------------

// Create
//
constructor TSourceUnit.Create(const unitName : String; rootTable : TSymbolTable);

   procedure AddStandardUnit(const name : String);
   var
      stdUnit : TUnitSymbol;
   begin
      stdUnit:=TUnitSymbol(rootTable.FindSymbol(name, cvMagic, TUnitSymbol));
      FSymbol.InterfaceTable.AddSymbol(TUnitSymbol.Create(name, stdUnit.Table, False));
      FSymbol.InterfaceTable.AddParent(stdUnit.Table);
   end;

begin
   inherited Create;
   FDependencies:=TStringList.Create;
   FSymbol:=TUnitSymbol.Create(unitName, TUnitSymbolTable.Create, True);
   rootTable.AddSymbol(FSymbol);

   FSymbol.CreateInterfaceTable;

   AddStandardUnit(SYS_INTERNAL);
   AddStandardUnit(SYS_DEFAULT);
end;

// Destroy
//
destructor TSourceUnit.Destroy;
begin
   FDependencies.Free;
   inherited;
end;

// GetUnitName
//
function TSourceUnit.GetUnitName : String;
begin
   Result:=Symbol.Name;
end;

// GetUnitTable
//
function TSourceUnit.GetUnitTable(systemTable, unitSyms : TSymbolTable; operators : TOperators) : TUnitSymbolTable;
begin
   Result:=(Symbol.Table as TUnitSymbolTable);
end;

// GetDependencies
//
function TSourceUnit.GetDependencies : TStrings;
begin
   Result:=FDependencies;
end;

// GetUnitFlags
//
function TSourceUnit.GetUnitFlags : TIdwsUnitFlags;
begin
   Result:=[ufOwnsSymbolTable];
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
