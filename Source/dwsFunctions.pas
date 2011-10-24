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
unit dwsFunctions;

{$I dws.inc}

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
      function GetUnitName : UnicodeString;
      function GetUnitTable(systemTable : TSymbolTable; unitSyms : TUnitMainSymbols;
                            operators : TOperators) : TUnitSymbolTable;
      function GetDependencies : TStrings;
      function GetUnitFlags : TIdwsUnitFlags;
   end;

   TIdwsUnitList = class(TSimpleList<IdwsUnit>)
      function IndexOf(const unitName : UnicodeString) : Integer; overload;
      function IndexOf(const aUnit : IdwsUnit) : Integer; overload;
      procedure AddUnits(list : TIdwsUnitList);
   end;

   TEmptyFunc = class(TInterfacedSelfObject, ICallable)
      public
         procedure Call(exec: TdwsProgramExecution; func: TFuncSymbol);
         procedure InitSymbol(Symbol: TSymbol);
         procedure InitExpression(Expr: TExprBase);
   end;

   TFunctionPrototype = class(TInterfacedSelfObject)
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
         constructor Create(table : TSymbolTable; const funcName : UnicodeString;
                            const params : TParamArray; const funcType : UnicodeString;
                            const isStateLess : Boolean = False); overload; virtual;
         constructor Create(table : TSymbolTable; const funcName : UnicodeString;
                            const params : array of UnicodeString; const funcType : UnicodeString;
                            const isStateLess : Boolean = False); overload;
         procedure Call(exec : TdwsProgramExecution; func : TFuncSymbol); override;
         procedure Execute(info : TProgramInfo); virtual; abstract;
   end;
   TInternalFunctionClass = class of TInternalFunction;

   TInternalMagicFunction = class(TInternalFunction)
      public
         constructor Create(table: TSymbolTable; const funcName: UnicodeString;
                            const params : TParamArray; const funcType: UnicodeString;
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
         procedure DoEvalAsString(args : TExprBaseList; var Result : UnicodeString); virtual; abstract;
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
         constructor Create(methKind : TMethodKind; attributes : TMethodAttributes;
                            const methName : UnicodeString; const methParams : array of UnicodeString;
                            const methType : UnicodeString; cls : TClassSymbol;
                            aVisibility : TdwsVisibility;
                            table : TSymbolTable);
         procedure Call(exec : TdwsProgramExecution; func : TFuncSymbol); override;
         procedure Execute(info : TProgramInfo; var externalObject : TObject); virtual; abstract;
   end;

   TInternalInitProc = procedure (systemTable : TSymbolTable; unitSyms : TUnitMainSymbols;
                                  unitTable : TSymbolTable; operators : TOperators);

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
         function GetUnitName: UnicodeString;
         procedure InitUnitTable(systemTable : TSymbolTable; unitSyms : TUnitMainSymbols;
                                 unitTable : TSymbolTable; operators : TOperators);
         function GetUnitTable(systemTable : TSymbolTable; unitSyms : TUnitMainSymbols;
                               operators : TOperators) : TUnitSymbolTable;
         function GetUnitFlags : TIdwsUnitFlags;

      public
         constructor Create;
         destructor Destroy; override;

         procedure AddInternalFunction(rif : Pointer);
         procedure AddPreInitProc(proc : TInternalInitProc);
         procedure AddPostInitProc(proc : TInternalInitProc);

         function InitStaticSymbols(systemTable : TSymbolTable; unitSyms : TUnitMainSymbols;
                                    operators : TOperators) : Boolean;
         procedure ReleaseStaticSymbols;

         property StaticTable : TStaticSymbolTable read FStaticTable;
         property StaticSymbols : Boolean read FStaticSymbols write SetStaticSymbols;
   end;

   TSourceUnit = class(TInterfacedObject, IdwsUnit)
      private
         FDependencies : TStrings;
         FSymbol : TUnitMainSymbol;

      protected

      public
         constructor Create(const unitName : UnicodeString; rootTable : TSymbolTable;
                            unitSyms : TUnitMainSymbols);
         destructor Destroy; override;

         function GetUnitName : UnicodeString;
         function GetUnitTable(systemTable : TSymbolTable; unitSyms : TUnitMainSymbols;
                               operators : TOperators) : TUnitSymbolTable;
         function GetDependencies : TStrings;
         function GetUnitFlags : TIdwsUnitFlags;

         property Symbol : TUnitMainSymbol read FSymbol write FSymbol;
   end;

procedure RegisterInternalFunction(InternalFunctionClass: TInternalFunctionClass;
      const FuncName: UnicodeString; const FuncParams: array of UnicodeString;
      const FuncType: UnicodeString; const isStateLess : Boolean = False);
procedure RegisterInternalIntFunction(InternalFunctionClass: TInternalMagicIntFunctionClass;
      const FuncName: UnicodeString; const FuncParams: array of UnicodeString; const isStateLess : Boolean = False);
procedure RegisterInternalBoolFunction(InternalFunctionClass: TInternalMagicBoolFunctionClass;
      const FuncName: UnicodeString; const FuncParams: array of UnicodeString; const isStateLess : Boolean = False);
procedure RegisterInternalFloatFunction(InternalFunctionClass: TInternalMagicFloatFunctionClass;
      const FuncName: UnicodeString; const FuncParams: array of UnicodeString; const isStateLess : Boolean = False);
procedure RegisterInternalStringFunction(InternalFunctionClass: TInternalMagicStringFunctionClass;
      const FuncName: UnicodeString; const FuncParams: array of UnicodeString; const isStateLess : Boolean = False);
procedure RegisterInternalProcedure(InternalFunctionClass: TInternalFunctionClass;
      const FuncName: UnicodeString; const FuncParams: array of UnicodeString);

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

// ConvertFuncParams
//
function ConvertFuncParams(const funcParams : array of UnicodeString) : TParamArray;

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
   SetLength(Result, Length(funcParams) div 2);
   x:=0;
   while x<Length(funcParams)-1 do begin
      paramRec:=@Result[x div 2];

      paramRec.ParamName:=funcParams[x];
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

      paramRec.ParamType:=funcParams[x+1];

      Inc(x, 2);
   end;
end;

type
   TRegisteredInternalFunction = record
      InternalFunctionClass : TInternalFunctionClass;
      FuncName : UnicodeString;
      FuncParams : TParamArray;
      FuncType : UnicodeString;
      StateLess : Boolean;
   end;
   PRegisteredInternalFunction = ^TRegisteredInternalFunction;

// RegisterInternalFunction
//
procedure RegisterInternalFunction(InternalFunctionClass: TInternalFunctionClass;
                                   const FuncName: UnicodeString;
                                   const FuncParams: array of UnicodeString;
                                   const FuncType: UnicodeString;
                                   const isStateLess : Boolean = False);
var
   rif : PRegisteredInternalFunction;
begin
   New(rif);
   rif.InternalFunctionClass := InternalFunctionClass;
   rif.FuncName := FuncName;
   rif.StateLess:=isStateLess;
   rif.FuncParams:=ConvertFuncParams(FuncParams);
   rif.FuncType := FuncType;

   dwsInternalUnit.AddInternalFunction(rif);
end;

// RegisterInternalIntFunction
//
procedure RegisterInternalIntFunction(InternalFunctionClass: TInternalMagicIntFunctionClass;
      const FuncName: UnicodeString; const FuncParams: array of UnicodeString; const isStateLess : Boolean = False);
begin
   RegisterInternalFunction(InternalFunctionClass, FuncName, FuncParams, 'Integer', isStateLess);
end;

// RegisterInternalBoolFunction
//
procedure RegisterInternalBoolFunction(InternalFunctionClass: TInternalMagicBoolFunctionClass;
      const FuncName: UnicodeString; const FuncParams: array of UnicodeString; const isStateLess : Boolean = False);
begin
   RegisterInternalFunction(InternalFunctionClass, FuncName, FuncParams, 'Boolean', isStateLess);
end;

// RegisterInternalFloatFunction
//
procedure RegisterInternalFloatFunction(InternalFunctionClass: TInternalMagicFloatFunctionClass;
      const FuncName: UnicodeString; const FuncParams: array of UnicodeString; const isStateLess : Boolean = False);
begin
   RegisterInternalFunction(InternalFunctionClass, FuncName, FuncParams, 'Float', isStateLess);
end;

// RegisterInternalStringFunction
//
procedure RegisterInternalStringFunction(InternalFunctionClass: TInternalMagicStringFunctionClass;
      const FuncName: UnicodeString; const FuncParams: array of UnicodeString; const isStateLess : Boolean = False);
begin
   RegisterInternalFunction(InternalFunctionClass, FuncName, FuncParams, 'String', isStateLess);
end;

// RegisterInternalProcedure
//
procedure RegisterInternalProcedure(InternalFunctionClass: TInternalFunctionClass;
      const FuncName: UnicodeString; const FuncParams: array of UnicodeString);
begin
   RegisterInternalFunction(InternalFunctionClass, FuncName, FuncParams, '', False);
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

// ------------------
// ------------------ TInternalFunction ------------------
// ------------------

constructor TInternalFunction.Create(table : TSymbolTable; const funcName : UnicodeString;
                                     const params : TParamArray; const funcType : UnicodeString;
                                     const isStateLess : Boolean = False);
var
   sym: TFuncSymbol;
begin
   sym:=TFuncSymbol.Generate(table, funcName, params, funcType);
   sym.Params.AddParent(table);
   sym.Executable:=ICallable(Self);
   sym.IsStateless:=isStateLess;
   FFuncSymbol:=sym;
   table.AddSymbol(sym);
end;

// Create
//
constructor TInternalFunction.Create(table: TSymbolTable; const funcName : UnicodeString;
                                     const params : array of UnicodeString; const funcType : UnicodeString;
                                     const isStateLess : Boolean = False);
begin
   Create(table, funcName, ConvertFuncParams(params), funcType, isStateLess);
end;

// Call
//
procedure TInternalFunction.Call(exec : TdwsProgramExecution; func : TFuncSymbol);
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
constructor TInternalMagicFunction.Create(table : TSymbolTable;
      const funcName : UnicodeString; const params : TParamArray; const funcType : UnicodeString;
      const isStateLess : Boolean = False);
var
  sym : TMagicFuncSymbol;
begin
  sym:=TMagicFuncSymbol.Generate(table, funcName, params, funcType);
  sym.params.AddParent(table);
  sym.InternalFunction:=Self;
  sym.IsStateless:=isStateLess;
  table.AddSymbol(sym);
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
   buf : UnicodeString;
begin
   DoEvalAsString(args, buf);
   Result:=buf;
end;

// ------------------
// ------------------ TInternalMethod ------------------
// ------------------

// Create
//
constructor TInternalMethod.Create(methKind: TMethodKind; attributes: TMethodAttributes;
                                   const methName: UnicodeString; const methParams: array of UnicodeString;
                                   const methType: UnicodeString; cls: TClassSymbol;
                                   aVisibility : TdwsVisibility;
                                   table: TSymbolTable);
var
   sym : TMethodSymbol;
   params : TParamArray;
begin
   params:=ConvertFuncParams(methParams);

   sym:=TMethodSymbol.Generate(table, methKind, attributes, methName, Params,
                                 methType, cls, aVisibility);
   sym.Params.AddParent(table);
   sym.Executable := ICallable(Self);

   // Add method to its class
   cls.AddMethod(sym);
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

function TInternalUnit.GetUnitName: UnicodeString;
begin
  Result := SYS_INTERNAL;
end;

function TInternalUnit.InitStaticSymbols(systemTable : TSymbolTable; unitSyms : TUnitMainSymbols;
                                         operators : TOperators) : Boolean;
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

function TInternalUnit.GetUnitTable(systemTable : TSymbolTable; unitSyms : TUnitMainSymbols;
                                    operators : TOperators) : TUnitSymbolTable;
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

procedure TInternalUnit.InitUnitTable(systemTable : TSymbolTable; unitSyms : TUnitMainSymbols;
                                      unitTable : TSymbolTable; operators : TOperators);
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
function TIdwsUnitList.IndexOf(const unitName : UnicodeString) : Integer;
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
constructor TSourceUnit.Create(const unitName : UnicodeString; rootTable : TSymbolTable;
                               unitSyms : TUnitMainSymbols);
begin
   inherited Create;
   FDependencies:=TStringList.Create;
   FSymbol:=TUnitMainSymbol.Create(unitName, TUnitSymbolTable.Create(nil, rootTable.AddrGenerator), unitSyms);
   FSymbol.ReferenceInSymbolTable(rootTable);

   FSymbol.CreateInterfaceTable;

   unitSyms.Find(SYS_INTERNAL).ReferenceInSymbolTable(FSymbol.InterfaceTable);
   unitSyms.Find(SYS_DEFAULT).ReferenceInSymbolTable(FSymbol.InterfaceTable);
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
function TSourceUnit.GetUnitName : UnicodeString;
begin
   Result:=Symbol.Name;
end;

// GetUnitTable
//
function TSourceUnit.GetUnitTable(systemTable : TSymbolTable; unitSyms : TUnitMainSymbols;
                                  operators : TOperators) : TUnitSymbolTable;
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
