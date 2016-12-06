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
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsJSONConnector;

{$I dws.inc}

interface

uses
   Classes, SysUtils, Variants,
   dwsLanguageExtension, dwsComp, dwsCompiler, dwsDataContext, dwsExprList,
   dwsExprs, dwsTokenizer, dwsSymbols, dwsErrors, dwsCoreExprs, dwsStack,
   dwsStrings, dwsXPlatform, dwsUtils, dwsOperators, dwsUnitSymbols,
   dwsFunctions, dwsJSON, dwsMagicExprs, dwsConnectorSymbols, dwsScriptSource;

type

   // TdwsJSONLibModule
   //
   TdwsJSONLibModule = class (TdwsCustomLangageExtension)
      protected
         function CreateExtension : TdwsLanguageExtension; override;
   end;

   // TdwsJSONLanguageExtension
   //
   TdwsJSONLanguageExtension = class (TdwsLanguageExtension)
      public
         procedure CreateSystemSymbols(table : TSystemSymbolTable); override;
         function StaticSymbols : Boolean; override;
   end;

   // TdwsJSONConnectorType
   //
   TdwsJSONConnectorType = class (TInterfacedSelfObject, IConnectorType)
      private
         FTable : TSystemSymbolTable;
         FTypJSONVariant : TConnectorSymbol;
         FLowCall, FHighCall, FLengthCall : IConnectorCall;
         FIndexReadCall, FIndexWriteCall : IConnectorCall;
         FTypeNameCall : IConnectorCall;
         FElementNameCall : IConnectorCall;
         FCloneCall : IConnectorCall;
         FExtendCall : IConnectorCall;
         FAddCall : IConnectorCall;
         FToStringCall : IConnectorCall;
         FLengthMember : IConnectorMember;

      protected
         function ConnectorCaption : UnicodeString;
         function AutoVarParams : Boolean;
         function AcceptsParams(const params : TConnectorParamArray) : Boolean;
         function WritableReads(const memberName : UnicodeString) : Boolean;

         function HasMethod(const methodName : UnicodeString; const params : TConnectorParamArray;
                            var typSym : TTypeSymbol) : IConnectorCall;
         function HasMember(const memberName : UnicodeString; var typSym : TTypeSymbol;
                            isWrite : Boolean) : IConnectorMember;
         function HasIndex(const propName : UnicodeString; const params : TConnectorParamArray;
                           var typSym : TTypeSymbol; isWrite : Boolean) : IConnectorCall;
         function HasEnumerator(var typSym: TTypeSymbol) : IConnectorEnumerator;
         function HasCast(typSym: TTypeSymbol) : IConnectorCast;

      public
         constructor Create(table : TSystemSymbolTable);

         property TypJSONVariant : TConnectorSymbol read FTypJSONVariant write FTypJSONVariant;
   end;

   TdwsJSONFastCallBase = class (TInterfacedSelfObject, IConnectorCall)
   end;

   TdwsJSONLowCall = class (TdwsJSONFastCallBase, IConnectorFastCall)
      public
         procedure FastCall(const args : TExprBaseListExec; var result : Variant);
   end;

   TdwsJSONHighCall = class (TdwsJSONFastCallBase, IConnectorFastCall)
      public
         procedure FastCall(const args : TExprBaseListExec; var result : Variant);
   end;

   TdwsJSONLengthCall = class (TdwsJSONFastCallBase, IConnectorFastCall)
      public
         procedure FastCall(const args : TExprBaseListExec; var result : Variant);
   end;

   TdwsJSONTypeNameCall = class (TdwsJSONFastCallBase, IConnectorFastCall)
      public
         procedure FastCall(const args : TExprBaseListExec; var result : Variant);
   end;

   TdwsJSONElementNameCall = class (TdwsJSONFastCallBase, IConnectorFastCall)
      public
         procedure FastCall(const args : TExprBaseListExec; var result : Variant);
   end;

   TdwsJSONCloneCall = class (TdwsJSONFastCallBase, IConnectorFastCall)
      public
         procedure FastCall(const args : TExprBaseListExec; var result : Variant);
   end;

   TdwsJSONExtendCall = class (TdwsJSONFastCallBase, IConnectorFastCall)
      public
         procedure FastCall(const args : TExprBaseListExec; var result : Variant);
   end;

   TdwsJSONAddCall = class (TdwsJSONFastCallBase, IConnectorFastCall)
      public
         procedure FastCall(const args : TExprBaseListExec; var result : Variant);
   end;

   TdwsJSONToStringCall = class (TdwsJSONFastCallBase, IConnectorFastCall)
      public
         procedure FastCall(const args : TExprBaseListExec; var result : Variant);
   end;

   TdwsJSONIndexCall = class(TdwsJSONFastCallBase)
      private
         FPropName : UnicodeString;

      public
         constructor Create(const propName : UnicodeString);

         property CallPropName : UnicodeString read FPropName write FPropName;
   end;

   TdwsJSONIndexReadCall = class(TdwsJSONIndexCall, IConnectorFastCall)
      protected
         procedure FastCall(const args : TExprBaseListExec; var result : Variant);
   end;

   TdwsJSONIndexWriteCall = class(TdwsJSONIndexCall, IConnectorFastCall)
      protected
         procedure FastCall(const args : TExprBaseListExec; var result : Variant);
   end;

   // TdwsJSONConnectorMember
   //
   TdwsJSONConnectorMember = class(TConnectorFastMember, IConnectorMember, IConnectorFastMember)
      private
         FMemberName : UnicodeString;
         FMemberHash : Cardinal;

      protected
         procedure SetMemberName(const n : UnicodeString);

         function Read(const base : Variant) : TData;
         procedure Write(const base : Variant; const data : TData);
         procedure FastRead(const exec : TdwsExecution; const base : TExprBase; var result : Variant); override;
         procedure FastWrite(const exec : TdwsExecution; const base, value : TExprBase); override;

      public
         constructor Create(const memberName : UnicodeString);

         property MemberName : UnicodeString read FMemberName write SetMemberName;
   end;

   TdwsJSONConnectorLengthMember = class(TdwsJSONConnectorMember)
      protected
         procedure FastRead(const exec : TdwsExecution; const base : TExprBase; var result : Variant); override;
   end;

   // TJSONConnectorSymbol
   //
   TJSONConnectorSymbol = class(TConnectorSymbol)
      public
         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         function CreateAssignExpr(prog : TdwsProgram; const aScriptPos: TScriptPos;
                                   exec : TdwsExecution;
                                   left : TDataExpr; right : TTypedExpr) : TProgramExpr; override;
   end;

   // TJSONParseMethod
   //
   TJSONParseMethod = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   // TJSONParseIntegerArrayMethod
   //
   TJSONParseIntegerArrayMethod = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   // TJSONParseFloatArrayMethod
   //
   TJSONParseFloatArrayMethod = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   // TJSONParseStringArrayMethod
   //
   TJSONParseStringArrayMethod = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   // TJSONNewObject
   //
   TJSONNewObject = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   // TJSONNewArray
   //
   TJSONNewArray = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   // TJSONStringifyMethod
   //
   TJSONStringifyMethod = class (TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString); override;

      class procedure Stringify(const args : TExprBaseListExec; var Result : UnicodeString); static;

      class procedure StringifyVariant(exec : TdwsExecution; writer : TdwsJSONWriter; const v : Variant); static;
      class procedure StringifySymbol(exec : TdwsExecution; writer : TdwsJSONWriter; sym : TSymbol; const dataPtr : IDataContext); static;
      class procedure StringifyDynamicArray(exec : TdwsExecution; writer : TdwsJSONWriter; dynArray : TScriptDynamicArray); static;
      class procedure StringifyArray(exec : TdwsExecution; writer : TdwsJSONWriter; elemSym : TTypeSymbol;
                                     const dataPtr : IDataContext; nb : Integer); static;
      class procedure StringifyComposite(exec : TdwsExecution; writer : TdwsJSONWriter;
                                         compSym : TCompositeTypeSymbol;
                                         const dataPtr : IDataContext); static;
      class procedure StringifyClass(exec : TdwsExecution; writer : TdwsJSONWriter;
                                     clsSym : TClassSymbol; const obj : IScriptObj); static;
   end;

   IBoxedJSONValue = interface
      ['{585B989C-220C-4120-B5F4-2819A0708A80}']
      function Value : TdwsJSONValue;
   end;

   TAssignBoxJSONExpr = class(TAssignExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

function BoxedJSONValue(value : TdwsJSONValue): IBoxedJSONValue;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cDefaultSymbolMarker = ttAT;

   SYS_JSON = 'JSON';
   SYS_JSONVARIANT = 'JSONVariant';
   SYS_JSON_STRINGIFY = 'Stringify';
   SYS_JSON_PARSE = 'Parse';
   SYS_JSON_PARSE_INTEGER_ARRAY = 'ParseIntegerArray';
   SYS_JSON_PARSE_FLOAT_ARRAY = 'ParseFloatArray';
   SYS_JSON_PARSE_STRING_ARRAY = 'ParseStringArray';
   SYS_JSON_NEWOBJECT = 'NewObject';
   SYS_JSON_NEWARRAY = 'NewArray';

type
   TBoxedJSONValue = class (TInterfacedObject, IBoxedJSONValue, ICoalesceable, IGetSelf, IUnknown)
      FValue : TdwsJSONValue;

      constructor Create(wrapped : TdwsJSONValue);
      destructor Destroy; override;

      function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; stdcall;

      function GetSelf : TObject;
      function ToString : UnicodeString; override;

      function Value : TdwsJSONValue;

      function IsFalsey : Boolean;

      class procedure Allocate(wrapped : TdwsJSONValue; var v : Variant); static;
      class procedure AllocateOrGetImmediate(wrapped : TdwsJSONValue; var v : Variant); static;

      class function UnBox(const v : Variant) : TdwsJSONValue; static;
   end;

   TBoxedNilJSONValue = class (TInterfacedObject, IBoxedJSONValue, ICoalesceable, IGetSelf, IUnknown)
      function GetSelf : TObject;
      function ToString : UnicodeString; override;
      function Value : TdwsJSONValue;
      function IsFalsey : Boolean;
   end;

var
   vNilJSONValue : IBoxedJSONValue;

// Create
//
constructor TBoxedJSONValue.Create(wrapped : TdwsJSONValue);
begin
   FValue:=wrapped;
end;

// Destroy
//
destructor TBoxedJSONValue.Destroy;
begin
   FValue.Free;
end;

// QueryInterface
//
function TBoxedJSONValue.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult;
begin
   if IsEqualGUID(IID, IBoxedJSONValue) then begin // D2010 workaround, does not support = testing
      PIUnknown(@Obj)^:=IBoxedJSONValue(Self);
      Result:=0;
   end else Result:=inherited QueryInterface(IID, Obj);
end;

// GetSelf
//
function TBoxedJSONValue.GetSelf : TObject;
begin
   Result:=Self;
end;

// Value
//
function TBoxedJSONValue.Value : TdwsJSONValue;
begin
   Result:=FValue;
end;

// ToString
//
function TBoxedJSONValue.ToString : UnicodeString;
begin
   if FValue.ValueType=jvtString then
      Result:=FValue.AsString
   else Result:=FValue.ToString;
end;

// IsFalsey
//
function TBoxedJSONValue.IsFalsey : Boolean;
begin
   Result:=FValue.IsFalsey;
end;

// Allocate
//
class procedure TBoxedJSONValue.Allocate(wrapped : TdwsJSONValue; var v : Variant);
var
   b : TBoxedJSONValue;
begin
   b:=TBoxedJSONValue.Create(wrapped);
   v:=IBoxedJSONValue(b);
end;

// AllocateOrGetImmediate
//
class procedure TBoxedJSONValue.AllocateOrGetImmediate(wrapped : TdwsJSONValue; var v : Variant);
begin
   if wrapped=nil then
      v:=vNilJSONValue
   else if wrapped.IsImmediateValue then
      TdwsJSONImmediate(wrapped).GetAsVariant(v)
   else begin
      wrapped.IncRefCount;
      TBoxedJSONValue.Allocate(wrapped, v);
   end;
end;

// UnBox
//
class function TBoxedJSONValue.UnBox(const v : Variant) : TdwsJSONValue;
var
   boxed : IBoxedJSONValue;
begin
   case PVarData(@v)^.VType of
      varUnknown : begin
         boxed:=(IUnknown(PVarData(@v)^.VUnknown) as IBoxedJSONValue);
         if boxed<>nil then
            Result:=boxed.Value
         else Result:=nil;
      end;
      varEmpty :
         Result:=nil;
      varNull :
         Result:=vNilJSONValue.Value;
   else
      raise EdwsJSONException.Create('Unsupported JSONVariant type');
   end;
end;

function BoxedJsonValue(Value : TdwsJSONValue): IBoxedJSONValue;
begin
   result := TBoxedJSONValue.Create(value);
end;

// Value
//
function TBoxedNilJSONValue.Value : TdwsJSONValue;
begin
   Result:=nil;
end;

// ToString
//
function TBoxedNilJSONValue.ToString;
begin
   Result:='';
end;

// IsFalsey
//
function TBoxedNilJSONValue.IsFalsey : Boolean;
begin
   Result:=True;
end;

// GetSelf
//
function TBoxedNilJSONValue.GetSelf : TObject;
begin
   Result := Self;
end;

// ------------------
// ------------------ TdwsJSONLibModule ------------------
// ------------------

// CreateExtension
//
function TdwsJSONLibModule.CreateExtension : TdwsLanguageExtension;
begin
   Result:=TdwsJSONLanguageExtension.Create;
end;

// ------------------
// ------------------ TdwsJSONLanguageExtension ------------------
// ------------------

// CreateSystemSymbols
//
procedure TdwsJSONLanguageExtension.CreateSystemSymbols(table : TSystemSymbolTable);
var
   connType : TdwsJSONConnectorType;
   connSym : TJSONConnectorSymbol;
   jsonObject : TClassSymbol;
begin
   connType:=TdwsJSONConnectorType.Create(table);
   connSym:=TJSONConnectorSymbol.Create(SYS_JSONVARIANT, connType);
   table.AddSymbol(connSym);
   connType.TypJSONVariant:=connSym;

   jsonObject:=TClassSymbol.Create(SYS_JSON, nil);
   jsonObject.InheritFrom(table.TypObject);
   table.AddSymbol(jsonObject);
   jsonObject.IsStatic:=True;
   jsonObject.IsSealed:=True;
   jsonObject.SetNoVirtualMembers;

   TJSONStringifyMethod.Create(
      table, SYS_JSON_STRINGIFY, ['val', SYS_ANY_TYPE], SYS_STRING,
      [iffStateLess, iffStaticMethod], jsonObject
   );

   TJSONParseMethod.Create(
      table, SYS_JSON_PARSE, ['str', SYS_STRING], SYS_JSONVARIANT,
      [iffStaticMethod], jsonObject, ''
   );

   TJSONParseIntegerArrayMethod.Create(
      table, SYS_JSON_PARSE_INTEGER_ARRAY, ['str', SYS_STRING], 'array of integer',
      [iffStaticMethod], jsonObject, ''
   );
   TJSONParseFloatArrayMethod.Create(
      table, SYS_JSON_PARSE_FLOAT_ARRAY, ['str', SYS_STRING], 'array of float',
      [iffStaticMethod], jsonObject, ''
   );
   TJSONParseStringArrayMethod.Create(
      table, SYS_JSON_PARSE_STRING_ARRAY, ['str', SYS_STRING], SYS_ARRAY_OF_STRING,
      [iffStaticMethod], jsonObject, ''
   );

   TJSONNewObject.Create(
      table, SYS_JSON_NEWOBJECT, [], SYS_JSONVARIANT,
      [iffStaticMethod], jsonObject, ''
   );
   TJSONNewArray.Create(
      table, SYS_JSON_NEWARRAY, [], SYS_JSONVARIANT,
      [iffStaticMethod], jsonObject, ''
   );
end;

// StaticSymbols
//
function TdwsJSONLanguageExtension.StaticSymbols : Boolean;
begin
   Result:=True;
end;

// ------------------
// ------------------ TdwsJSONConnectorType ------------------
// ------------------

// Create
//
constructor TdwsJSONConnectorType.Create(table : TSystemSymbolTable);
begin
   inherited Create;

   FTable:=table;

   FLowCall:=TdwsJSONLowCall.Create;
   FHighCall:=TdwsJSONHighCall.Create;
   FLengthCall:=TdwsJSONLengthCall.Create;
   FIndexReadCall:=TdwsJSONIndexReadCall.Create('');
   FIndexWriteCall:=TdwsJSONIndexWriteCall.Create('');
   FTypeNameCall:=TdwsJSONTypeNameCall.Create;
   FElementNameCall:=TdwsJSONElementNameCall.Create;
   FCloneCall:=TdwsJSONCloneCall.Create;
   FExtendCall:=TdwsJSONExtendCall.Create;
   FAddCall:=TdwsJSONAddCall.Create;
   FToStringCall:=TdwsJSONToStringCall.Create;

   FLengthMember:=TdwsJSONConnectorLengthMember.Create('length');
end;

// ConnectorCaption
//
function TdwsJSONConnectorType.ConnectorCaption : UnicodeString;
begin
   Result:='JSON Connector 2.0';
end;

// AutoVarParams
//
function TdwsJSONConnectorType.AutoVarParams : Boolean;
begin
   Result:=False;
end;

// AcceptsParams
//
function TdwsJSONConnectorType.AcceptsParams(const params : TConnectorParamArray) : Boolean;
begin
   Result:=True;
end;

// WritableReads
//
function TdwsJSONConnectorType.WritableReads(const memberName : UnicodeString) : Boolean;
begin
   Result := False;
end;

// HasMethod
//
function TdwsJSONConnectorType.HasMethod(const methodName : UnicodeString; const params : TConnectorParamArray;
                                         var typSym : TTypeSymbol) : IConnectorCall;
var
   paramTyp : TTypeSymbol;
   i : Integer;
begin
   if UnicodeSameText(methodName, 'typename') then begin

      Result:=FTypeNameCall;
      typSym:=FTable.TypString;

      if Length(params)<>0 then
         raise ECompileException.Create(CPE_NoParamsExpected);

   end else if UnicodeSameText(methodName, 'elementname') then begin

      if Length(params)<>1 then
         raise ECompileException.CreateFmt(CPE_BadNumberOfParameters, [1, Length(params)]);
      paramTyp:=params[0].TypSym;
      if not paramTyp.UnAliasedTypeIs(TBaseIntegerSymbol) then
         raise ECompileException.CreateFmt(CPE_BadParameterType, [0, SYS_INTEGER, paramTyp.Caption]);

      Result:=FElementNameCall;
      typSym:=FTable.TypString;

   end else if UnicodeSameText(methodName, 'extend') then begin

      if Length(params)<>1 then
         raise ECompileException.CreateFmt(CPE_BadNumberOfParameters, [1, Length(params)]);
      paramTyp:=params[0].TypSym;
      if paramTyp.UnAliasedType<>TypJSONVariant then
         raise ECompileException.CreateFmt(CPE_BadParameterType, [0, SYS_JSONVARIANT, paramTyp.Caption]);

      Result:=FExtendCall;
      typSym:=nil;

   end else if UnicodeSameText(methodName, 'add') or UnicodeSameText(methodName, 'push') then begin

      if Length(params)<1 then
         raise ECompileException.CreateFmt(CPE_BadNumberOfParameters, [1, Length(params)]);
      for i:=0 to High(params) do begin
         paramTyp:=params[i].TypSym;
         if (not paramTyp.UnAliasedType.IsBaseType) and (paramTyp.ClassType<>TNilSymbol) then
            raise ECompileException.CreateFmt(CPE_BadParameterType, [i, SYS_JSONVARIANT, paramTyp.Caption]);
      end;

      Result:=FAddCall;
      typSym:=FTable.TypInteger;

   end else begin

      if Length(params)<>0 then
         raise ECompileException.Create(CPE_NoParamsExpected);

      if UnicodeSameText(methodName, 'clone') then begin

         typSym:=TypJSONVariant;
         Result:=FCloneCall;

      end else if UnicodeSameText(methodName, 'tostring') then begin

         typSym:=FTable.TypString;
         Result:=FToStringCall;

      end else begin

         typSym:=FTable.TypInteger;
         if UnicodeSameText(methodName, 'length') then
            Result:=FLengthCall
         else if UnicodeSameText(methodName, 'low') then
            Result:=FLowCall
         else if UnicodeSameText(methodName, 'high') then
            Result:=FHighCall
         else Result:=nil;

      end;

   end;
end;

// HasMember
//
function TdwsJSONConnectorType.HasMember(const memberName : UnicodeString; var typSym : TTypeSymbol;
                                         isWrite : Boolean) : IConnectorMember;
begin
   typSym:=TypJSONVariant;
   if memberName='length' then
      Result:=FLengthMember
   else Result:=TdwsJSONConnectorMember.Create(memberName);
end;

// HasIndex
//
function TdwsJSONConnectorType.HasIndex(const propName : UnicodeString; const params : TConnectorParamArray;
                                        var typSym : TTypeSymbol; isWrite : Boolean) : IConnectorCall;
begin
   typSym:=TypJSONVariant;
   if propName='' then begin
      if isWrite then
         Result:=FIndexWriteCall
      else Result:=FIndexReadCall;
   end else begin
      if isWrite then
         Result:=TdwsJSONIndexWriteCall.Create(propName)
      else Result:=TdwsJSONIndexReadCall.Create(propName);
   end;
end;

// HasEnumerator
//
function TdwsJSONConnectorType.HasEnumerator(var typSym: TTypeSymbol) : IConnectorEnumerator;
begin
   Result:=nil;
end;

// HasCast
//
function TdwsJSONConnectorType.HasCast(typSym: TTypeSymbol) : IConnectorCast;
begin
   Result:=nil;
end;

// ------------------
// ------------------ TdwsJSONLowCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONLowCall.FastCall(const args : TExprBaseListExec; var result : Variant);
begin
   VarCopySafe(result, 0);
end;

// ------------------
// ------------------ TdwsJSONHighCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONHighCall.FastCall(const args : TExprBaseListExec; var result : Variant);
var
   base : Variant;
   v : TdwsJSONValue;
begin
   args.EvalAsVariant(0, base);
   v:=TBoxedJSONValue.UnBox(base);
   VarCopySafe(result, v.ElementCount-1);
end;

// ------------------
// ------------------ TdwsJSONLengthCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONLengthCall.FastCall(const args : TExprBaseListExec; var result : Variant);
var
   base : Variant;
   v : TdwsJSONValue;
begin
   args.EvalAsVariant(0, base);
   v:=TBoxedJSONValue.UnBox(base);
   VarCopySafe(result, v.ElementCount);
end;

// ------------------
// ------------------ TdwsJSONTypeNameCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONTypeNameCall.FastCall(const args : TExprBaseListExec; var result : Variant);
var
   base : Variant;
   vt : TdwsJSONValueType;
begin
   args.EvalAsVariant(0, base);
   case PVarData(@base)^.VType of
      varUnknown :
         vt:=TBoxedJSONValue.UnBox(base).ValueType;
      varUString :
         vt:=jvtString;
      varDouble :
         vt:=jvtNumber;
      varBoolean :
         vt:=jvtBoolean;
      varNull :
         vt:=jvtNull;
   else
      vt:=jvtUndefined;
   end;
   VarCopySafe(result, TdwsJSONValue.ValueTypeStrings[vt]);
end;

// ------------------
// ------------------ TdwsJSONElementNameCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONElementNameCall.FastCall(const args : TExprBaseListExec; var result : Variant);
var
   base : Variant;
   v : TdwsJSONValue;
begin
   args.EvalAsVariant(0, base);
   v:=TBoxedJSONValue.UnBox(base);
   if v<>nil then
      VarCopySafe(result, v.Names[args.AsInteger[1]])
   else VarCopySafe(result, '');
end;

// ------------------
// ------------------ TdwsJSONCloneCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONCloneCall.FastCall(const args : TExprBaseListExec; var result : Variant);
var
   base : Variant;
   v : TdwsJSONValue;
begin
   args.EvalAsVariant(0, base);
   v:=TBoxedJSONValue.UnBox(base);
   if v<>nil then
      VarCopySafe(result, IBoxedJSONValue(TBoxedJSONValue.Create(v.Clone)))
   else VarCopySafe(result, vNilJSONValue);
end;

// ------------------
// ------------------ TdwsJSONExtendCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONExtendCall.FastCall(const args : TExprBaseListExec; var result : Variant);
var
   base, param : Variant;
   v : TdwsJSONValue;
begin
   args.EvalAsVariant(0, base);
   v:=TBoxedJSONValue.UnBox(base);
   if v<>nil then begin
      args.EvalAsVariant(1, param);
      v.Extend(TBoxedJSONValue.UnBox(param));
   end;
end;

// ------------------
// ------------------ TdwsJSONAddCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONAddCall.FastCall(const args : TExprBaseListExec; var result : Variant);
var
   i : Integer;
   base, param : Variant;
   baseValue, paramValue : TdwsJSONValue;
   pParam : PVarData;
   baseArray : TdwsJSONArray;
begin
   args.EvalAsVariant(0, base);
   baseValue:=TBoxedJSONValue.UnBox(base);
   if baseValue<>nil then begin
      if baseValue.ValueType=jvtArray then begin
         baseArray:=TdwsJSONArray(baseValue);
         for i:=1 to args.Count-1 do begin
            args.EvalAsVariant(i, param);
            pParam:=PVarData(@param);
            case pParam^.VType of
               varInt64 : baseArray.Add(pParam^.VInt64);
               varDouble : baseArray.Add(pParam^.VDouble);
               varUString : baseArray.Add(UnicodeString(pParam^.VUString));
               varBoolean : baseArray.Add(pParam^.VBoolean);
               varUnknown : begin
                  if pParam.VUnknown<>nil then begin
                     paramValue:=TBoxedJSONValue.UnBox(param);
                     if paramValue.Owner = nil then
                        paramValue.IncRefCount;
                     baseArray.Add(paramValue)
                  end else begin
                     baseArray.AddNull;
                  end;
               end;
               varNull : baseArray.AddNull;
            else
               raise EdwsJSONException.Create('JSON Array Add() unsupported type');
            end;
         end;
         VarCopySafe(result, baseArray.ElementCount);
         Exit;
      end;
   end;
   raise EdwsJSONException.Create('JSON Array required for Add method');
end;

// ------------------
// ------------------ TdwsJSONToStringCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONToStringCall.FastCall(const args : TExprBaseListExec; var result : Variant);
begin
   VarCopySafe(result, '');
   TJSONStringifyMethod.Stringify(args, UnicodeString(PVarData(@Result)^.VString));
end;

// ------------------
// ------------------ TdwsJSONIndexCall ------------------
// ------------------

// Create
//
constructor TdwsJSONIndexCall.Create(const propName : UnicodeString);
begin
   inherited Create;
   FPropName:=propName;
end;

// ------------------
// ------------------ TdwsJSONIndexReadCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONIndexReadCall.FastCall(const args : TExprBaseListExec; var result : Variant);
var
   v : TdwsJSONValue;
   b, idx : Variant;
begin
   args.ExprBase[0].EvalAsVariant(args.Exec, b);
   if PVarData(@b)^.VType=varUnknown then begin
      v:=TBoxedJSONValue.UnBox(b);
      if v<>nil then begin
         if FPropName<>'' then
            v:=v.Items[FPropName];
         args.ExprBase[1].EvalAsVariant(args.Exec, idx);
         v:=v.Values[idx];
         TBoxedJSONValue.AllocateOrGetImmediate(v, Result);
         Exit;
      end;
   end;
   VarCopySafe(Result, vNilJSONValue);
end;

// ------------------
// ------------------ TdwsJSONIndexWriteCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONIndexWriteCall.FastCall(const args : TExprBaseListExec; var result : Variant);
var
   b, val : Variant;
   pVal : PVarData;
   baseValue, argValue : TdwsJSONValue;
begin
   args.ExprBase[0].EvalAsVariant(args.Exec, b);
   if PVarData(@b)^.VType=varUnknown then begin
      baseValue:=TBoxedJSONValue.UnBox(b);
      if FPropName<>'' then
         baseValue:=baseValue.Items[FPropName];
      args.ExprBase[2].EvalAsVariant(args.Exec, val);
      pVal:=@val;
      case pVal^.VType of
         varUnknown : begin
            argValue:=TBoxedJSONValue.UnBox(val);
            if argValue=nil then
               argValue:=TdwsJSONImmediate.FromVariant(Null)
            else if argValue.Owner=nil then
               argValue.IncRefCount;
         end;
         varInt64 : begin
            argValue:=TdwsJSONImmediate.Create;
            argValue.AsNumber:=pVal^.VInt64;
         end;
         varDouble : begin
            argValue:=TdwsJSONImmediate.Create;
            argValue.AsNumber:=pVal^.VDouble;
         end;
         varUString : begin
            argValue:=TdwsJSONImmediate.Create;
            argValue.AsString:=UnicodeString(pVal^.VUString);
         end;
         varBoolean : begin
            argValue:=TdwsJSONImmediate.Create;
            argValue.AsBoolean:=pVal^.VBoolean;
         end;
         varNull : begin
            argValue:=TdwsJSONImmediate.FromVariant(Null);
         end;
         varEmpty : begin
            argValue:=nil;
         end;
      else
         if VarIsNumeric(val) then begin
            argValue:=TdwsJSONImmediate.Create;
            argValue.AsNumber:=val;
         end else raise Exception.Create('Unsupported assignment');
      end;
      args.ExprBase[1].EvalAsVariant(args.Exec, val);
      baseValue.Values[val]:=argValue;
   end else begin
      raise Exception.CreateFmt('Invalid JSON write to %s', [FPropName]);
   end;
end;

// ------------------
// ------------------ TdwsJSONConnectorMember ------------------
// ------------------

// Create
//
constructor TdwsJSONConnectorMember.Create(const memberName : UnicodeString);
begin
   inherited Create;
   SetMemberName(memberName);
end;

// Read
//
function TdwsJSONConnectorMember.Read(const base : Variant) : TData;
begin
   Assert(False);
end;

// Write
//
procedure TdwsJSONConnectorMember.Write(const base : Variant; const data : TData);
begin
   Assert(False);
end;

// SetMemberName
//
procedure TdwsJSONConnectorMember.SetMemberName(const n : UnicodeString);
begin
   FMemberName := n;
   FMemberHash := SimpleStringHash(n);
end;

// FastRead
//
procedure TdwsJSONConnectorMember.FastRead(
      const exec : TdwsExecution; const base : TExprBase; var result : Variant);
var
   b : Variant;
   v : TdwsJSONValue;
begin
   base.EvalAsVariant(exec, b);
   v:=TBoxedJSONValue.UnBox(b);
   if v<>nil then begin
      v:=v.HashedItems[FMemberHash, FMemberName];
      TBoxedJSONValue.AllocateOrGetImmediate(v, result)
   end else VarCopySafe(result, vNilJSONValue);
end;

// FastWrite
//
procedure TdwsJSONConnectorMember.FastWrite(
      const exec : TdwsExecution; const base, value : TExprBase);
var
   b, v : Variant;
   baseValue, dataValue : TdwsJSONValue;
begin
   base.EvalAsVariant(exec, b);
   baseValue := TBoxedJSONValue.UnBox(b);
   if baseValue<>nil then begin
      value.EvalAsVariant(exec, v);
      case PVarData(@v)^.VType of
         varUnknown : begin
            dataValue := TBoxedJSONValue.UnBox(v);
            if dataValue=nil then
               dataValue := TdwsJSONImmediate.FromVariant(Null)
            else begin
               if dataValue.Owner=nil then
                  dataValue.IncRefCount;
            end;
         end;
         varEmpty :
            dataValue := nil;
      else
         dataValue := TdwsJSONImmediate.FromVariant(v);
      end;
   end else dataValue := nil;

   baseValue.HashedItems[FMemberHash, FMemberName]:=dataValue;
end;

// ------------------
// ------------------ TdwsJSONConnectorLengthMember ------------------
// ------------------

// FastRead
//
procedure TdwsJSONConnectorLengthMember.FastRead(const exec : TdwsExecution; const base : TExprBase; var result : Variant);
var
   b : Variant;
   v : TdwsJSONValue;
begin
   base.EvalAsVariant(exec, b);
   v:=TBoxedJSONValue.UnBox(b);
   if v<>nil then begin
      case v.ValueType of
         jvtArray : VarCopySafe(result, v.ElementCount);
         jvtString : VarCopySafe(result, Length(v.AsString));
      else
         v:=v.Items[FMemberName];
         TBoxedJSONValue.AllocateOrGetImmediate(v, result)
      end;
   end else VarCopySafe(result, vNilJSONValue);
end;

// ------------------
// ------------------ TJSONConnectorSymbol ------------------
// ------------------

// IsCompatible
//
function TJSONConnectorSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   Result:=   inherited IsCompatible(typSym)
           or (typSym.AsFuncSymbol<>nil)
           or (typSym is TRecordSymbol);
end;

// CreateAssignExpr
//
function TJSONConnectorSymbol.CreateAssignExpr(prog : TdwsProgram; const aScriptPos: TScriptPos;
                                               exec : TdwsExecution;
                                               left : TDataExpr; right : TTypedExpr) : TProgramExpr;
var
   rightTyp : TTypeSymbol;
   rightTypClass : TClass;
begin
   Result:=nil;
   rightTyp:=right.Typ.BaseType;
   if rightTyp=nil then Exit;

   rightTypClass:=rightTyp.ClassType;
   if rightTypClass=TJSONConnectorSymbol then
      Result:=TAssignExpr.Create(prog, aScriptPos, exec, left, right)
   else if rightTypClass.InheritsFrom(TBaseSymbol) then
      Result:=TAssignBoxJSONExpr.Create(prog, aScriptPos, exec, left, right);

   if Result=nil then begin
      prog.CompileMsgs.AddCompilerErrorFmt(aScriptPos, CPE_AssignIncompatibleTypes,
                                           [right.Typ.Caption, left.Typ.Caption]);
      left.Free;
      right.Free;
   end;
end;

// ------------------
// ------------------ TJSONParseMethod ------------------
// ------------------

procedure TJSONParseMethod.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   v : TdwsJSONValue;
   box : TBoxedJSONValue;
begin
   v:=TdwsJSONValue.ParseString(args.AsString[0]);
   if v=nil then
      box:=nil
   else box:=TBoxedJSONValue.Create(v);
   VarCopySafe(result, IBoxedJSONValue(box));
end;

// ------------------
// ------------------ TJSONParseIntegerArrayMethod ------------------
// ------------------

// DoEvalAsVariant
//
procedure TJSONParseIntegerArrayMethod.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   tokenizer : TdwsJSONParserState;
   values : TSimpleInt64List;
   i : Integer;
   newArray : TScriptDynamicArray;
   newPData : PData;
   s : UnicodeString;
begin
   s := args.AsString[0];

   tokenizer:=TdwsJSONParserState.Create(s);
   values:=TSimpleInt64List.Create;
   try
      tokenizer.ParseIntegerArray(values);

      newArray:=TScriptDynamicArray.CreateNew((args.Exec as TdwsProgramExecution).Prog.TypInteger);
      VarCopySafe(result, IScriptDynArray(newArray));
      newArray.ArrayLength:=values.Count;
      newPData:=newArray.AsPData;

      for i:=0 to newArray.ArrayLength-1 do
         newPData^[i]:=values[i];
   finally
      values.Free;
      tokenizer.Free;
   end;
end;

// ------------------
// ------------------ TJSONParseFloatArrayMethod ------------------
// ------------------

// DoEvalAsVariant
//
procedure TJSONParseFloatArrayMethod.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   tokenizer : TdwsJSONParserState;
   values : TSimpleDoubleList;
   i : Integer;
   newArray : TScriptDynamicArray;
   newPData : PData;
   s : UnicodeString;
begin
   s := args.AsString[0];

   tokenizer:=TdwsJSONParserState.Create(s);
   values:=TSimpleDoubleList.Create;
   try
      tokenizer.ParseNumberArray(values);

      newArray:=TScriptDynamicArray.CreateNew((args.Exec as TdwsProgramExecution).Prog.TypInteger);
      VarCopySafe(result, IScriptDynArray(newArray));
      newArray.ArrayLength:=values.Count;
      newPData:=newArray.AsPData;

      for i:=0 to newArray.ArrayLength-1 do
         newPData^[i]:=values[i];
   finally
      values.Free;
      tokenizer.Free;
   end;
end;

// ------------------
// ------------------ TJSONParseStringArrayMethod ------------------
// ------------------

// DoEvalAsVariant
//
procedure TJSONParseStringArrayMethod.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   tokenizer : TdwsJSONParserState;
   values : TStringList;
   i : Integer;
   newArray : TScriptDynamicArray;
   newPData : PData;
   s : UnicodeString;
begin
   s := args.AsString[0];

   tokenizer:=TdwsJSONParserState.Create(s);
   values:=TStringList.Create;
   try
      tokenizer.ParseStringArray(values);

      newArray:=TScriptDynamicArray.CreateNew((args.Exec as TdwsProgramExecution).Prog.TypString);
      VarCopySafe(result, IScriptDynArray(newArray));
      newArray.ArrayLength:=values.Count;
      newPData:=newArray.AsPData;

      for i:=0 to newArray.ArrayLength-1 do
         newPData^[i]:=values[i];
   finally
      values.Free;
      tokenizer.Free;
   end;
end;

// ------------------
// ------------------ TJSONNewObject ------------------
// ------------------

// DoEvalAsVariant
//
procedure TJSONNewObject.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   v : TdwsJSONValue;
   box : TBoxedJSONValue;
begin
   v := TdwsJSONObject.Create;
   box := TBoxedJSONValue.Create(v);
   VarCopySafe(result, IBoxedJSONValue(box));
end;

// ------------------
// ------------------ TJSONNewArray ------------------
// ------------------

// DoEvalAsVariant
//
procedure TJSONNewArray.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   v : TdwsJSONValue;
   box : TBoxedJSONValue;
begin
   v := TdwsJSONArray.Create;
   box := TBoxedJSONValue.Create(v);
   VarCopySafe(result, IBoxedJSONValue(box));
end;

// ------------------
// ------------------ TJSONStringifyMethod ------------------
// ------------------

// DoEvalAsString
//
procedure TJSONStringifyMethod.DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString);
begin
   Stringify(args, Result);
end;

// Stringify
//
class procedure TJSONStringifyMethod.Stringify(const args : TExprBaseListExec; var Result : UnicodeString);
var
   writer : TdwsJSONWriter;
   stream : TWriteOnlyBlockStream;
   expr : TTypedExpr;
   dataExpr : TDataExpr;
   v : Variant;
begin
   stream:=TWriteOnlyBlockStream.AllocFromPool;
   writer:=TdwsJSONWriter.Create(stream);
   try
      expr:=(args.ExprBase[0] as TTypedExpr);
      if expr.Typ.Size=1 then begin
         expr.EvalAsVariant(args.Exec, v);
         StringifyVariant(args.Exec, writer, v);
      end else begin
         dataExpr:=(expr as TDataExpr);
         StringifySymbol(args.Exec, writer, expr.Typ, dataExpr.DataPtr[args.Exec]);
      end;
      Result:=stream.ToString;
   finally
      writer.Free;
      stream.ReturnToPool;
   end;
end;

// StringifyVariant
//
class procedure TJSONStringifyMethod.StringifyVariant(exec : TdwsExecution; writer : TdwsJSONWriter; const v : Variant);
var
   unk : IUnknown;
   getSelf : IGetSelf;
   selfObj : TObject;
   boxedJSON : IBoxedJSONValue;
   scriptObj : IScriptObj;
   p : PVarData;
begin
   p:=PVarData(@v);
   case p^.VType of
      varInt64 :
         writer.WriteInteger(p^.VInt64);
      varDouble :
         writer.WriteNumber(p^.VDouble);
      varBoolean :
         writer.WriteBoolean(p^.VBoolean);
      varNull :
         writer.WriteNull;
      varUnknown : begin
         unk:=IUnknown(p^.VUnknown);
         if unk=nil then
            writer.WriteNull
         else if unk.QueryInterface(IBoxedJSONValue, boxedJSON)=0 then begin

            if boxedJSON.Value<>nil then
               boxedJSON.Value.WriteTo(writer)
            else writer.WriteString('Undefined');

         end else begin

            if unk.QueryInterface(IGetSelf, getSelf)=0 then begin

               selfObj:=getSelf.GetSelf;
               if selfObj is TScriptObjInstance then begin

                  scriptObj:=TScriptObjInstance(selfObj);
                  StringifyClass(exec, writer, scriptObj.ClassSym, scriptObj);

               end else if selfObj is TScriptDynamicArray then begin

                  StringifyDynamicArray(exec, writer, TScriptDynamicArray(selfObj))

               end else if selfObj<>nil then begin

                  writer.WriteString(selfObj.ToString)

               end else writer.WriteString('null');

            end else writer.WriteString('IUnknown');

         end;
      end;
   else
      writer.WriteString(v);
   end;
end;

// StringifySymbol
//
class procedure TJSONStringifyMethod.StringifySymbol(exec : TdwsExecution; writer : TdwsJSONWriter; sym : TSymbol; const dataPtr : IDataContext);
var
   ct : TClass;
begin
   sym:=sym.BaseType;
   ct:=sym.ClassType;
   if ct.InheritsFrom(TBaseSymbol) then
      StringifyVariant(exec, writer, dataPtr[0])
   else if ct=TDynamicArraySymbol then
      StringifyDynamicArray(exec, writer, IScriptDynArray(dataPtr.AsInterface[0]).GetSelf as TScriptDynamicArray)
   else if ct.InheritsFrom(TStaticArraySymbol) then
      StringifyArray(exec, writer, TStaticArraySymbol(sym).Typ, dataPtr, TStaticArraySymbol(sym).ElementCount)
   else if ct=TRecordSymbol then
      StringifyComposite(exec, writer, TRecordSymbol(sym), dataPtr)
   else if ct=TClassSymbol then
      StringifyClass(exec, writer, TClassSymbol(sym), IScriptObj(dataPtr.AsInterface[0]))
   else writer.WriteString(sym.ClassName);
end;

// StringifyArray
//
class procedure TJSONStringifyMethod.StringifyArray(exec : TdwsExecution;
   writer : TdwsJSONWriter; elemSym : TTypeSymbol; const dataPtr : IDataContext; nb : Integer);
var
   i, s : Integer;
   locData : IDataContext;
   unaliasedClassType : TClass;
begin
   s:=elemSym.Size;
   writer.BeginArray;
   unaliasedClassType:=elemSym.UnAliasedType.ClassType;
   if unaliasedClassType=TBaseIntegerSymbol then begin
      for i:=0 to nb-1 do
         writer.WriteInteger(dataPtr.AsInteger[i]);
   end else if unaliasedClassType=TBaseFloatSymbol then begin
      for i:=0 to nb-1 do
         writer.WriteNumber(dataPtr.AsFloat[i]);
   end else if unaliasedClassType=TBaseStringSymbol then begin
      for i:=0 to nb-1 do
         writer.WriteString(dataPtr.AsString[i]);
   end else begin
      for i:=0 to nb-1 do begin
         dataPtr.CreateOffset(i*s, locData);
         StringifySymbol(exec, writer, elemSym, locData);
      end;
   end;
   writer.EndArray;
end;

// StringifyDynamicArray
//
class procedure TJSONStringifyMethod.StringifyDynamicArray(exec : TdwsExecution;
   writer : TdwsJSONWriter; dynArray : TScriptDynamicArray);
var
   locData : IDataContext;
begin
   exec.DataContext_Create(dynArray.AsData, 0, locData);
   StringifyArray(exec, writer, dynArray.ElementTyp, locData, dynArray.ArrayLength);
end;

// StringifyComposite
//
class procedure TJSONStringifyMethod.StringifyComposite(exec : TdwsExecution;
   writer : TdwsJSONWriter; compSym : TCompositeTypeSymbol; const dataPtr : IDataContext);
var
   i : Integer;
//   bufData : TData;
   sym : TSymbol;
   fieldSym : TFieldSymbol;
   propSym : TPropertySymbol;
   locData : IDataContext;
begin
   writer.BeginObject;
   for i:=0 to compSym.Members.Count-1 do begin
      sym:=compSym.Members[i];
      if sym.ClassType=TPropertySymbol then begin
         propSym:=TPropertySymbol(sym);
         if (propSym.Visibility>=cvPublished) and (propSym.ReadSym<>nil) then
            sym:=propSym.ReadSym
         else continue;
         writer.WriteName(propSym.Name);
      end else if sym.ClassType=TFieldSymbol then begin
         if TFieldSymbol(sym).Visibility<cvPublished then
            continue;
         writer.WriteName(sym.Name);
      end else continue;

      if sym.ClassType=TFieldSymbol then begin
         fieldSym:=TFieldSymbol(sym);
         dataPtr.CreateOffset(fieldSym.Offset, locData);
         StringifySymbol(exec, writer, fieldSym.Typ, locData);
      end else begin
//         SetLength(bufData, sym.Typ.Size);
         Assert(False, 'published method getters not supported yet');
      end;
   end;
   writer.EndObject;
end;

// StringifyClass
//
class procedure TJSONStringifyMethod.StringifyClass(exec : TdwsExecution;
   writer : TdwsJSONWriter; clsSym : TClassSymbol; const obj : IScriptObj);
begin
   if (obj=nil) or (obj.Destroyed) then
      writer.WriteNull
   else StringifyComposite(exec, writer, clsSym, obj);
end;

// ------------------
// ------------------ TAssignBoxJSONExpr ------------------
// ------------------

// EvalNoResult
//
procedure TAssignBoxJSONExpr.EvalNoResult(exec : TdwsExecution);
var
   rv : Variant;
   v : TdwsJSONValue;
   box : TBoxedJSONValue;
begin
   Right.EvalAsVariant(exec, rv);
   v:=TdwsJSONImmediate.FromVariant(rv);
   box:=TBoxedJSONValue.Create(v);
   Left.AssignValue(exec, IBoxedJSONValue(box));
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   vNilJSONValue:=TBoxedNilJSONValue.Create;

finalization

   vNilJSONValue:=nil;

end.
