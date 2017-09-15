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
   dwsFunctions, dwsJSON, dwsMagicExprs, dwsConnectorSymbols, dwsScriptSource,
   dwsXXHash, dwsCompilerContext, dwsCompilerUtils, dwsUnicode;

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
         FDeleteCall : IConnectorCall;
         FSwapCall : IConnectorCall;
         FToStringCall : IConnectorCall;
         FLengthMember : IConnectorMember;

      protected
         function ConnectorCaption : String;
         function AutoVarParams : Boolean;
         function AcceptsParams(const params : TConnectorParamArray) : Boolean;
         function WritableReads(const memberName : String) : Boolean;

         function HasMethod(const methodName : String; const params : TConnectorParamArray;
                            var typSym : TTypeSymbol) : IConnectorCall;
         function HasMember(const memberName : String; var typSym : TTypeSymbol;
                            isWrite : Boolean) : IConnectorMember;
         function HasIndex(const propName : String; const params : TConnectorParamArray;
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

   TdwsJSONDeleteCall = class (TdwsJSONFastCallBase, IConnectorFastCall)
      public
         procedure FastCall(const args : TExprBaseListExec; var result : Variant);
   end;

   TdwsJSONSwapCall = class (TdwsJSONFastCallBase, IConnectorFastCall)
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
         constructor Create(const propName : String);

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
         constructor Create(const memberName : String);

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
         function CreateAssignExpr(context : TdwsCompilerContext; const aScriptPos: TScriptPos;
                                   left : TDataExpr; right : TTypedExpr) : TProgramExpr; override;
   end;

   // TJSONParseMethod
   //
   TJSONParseMethod = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   // TJSONParseUTF8Method
   //
   TJSONParseUTF8Method = class(TInternalMagicVariantFunction)
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
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); override;
   end;

   // TJSONStringifyUTF8Method
   //
   TJSONStringifyUTF8Method = class (TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); override;
   end;

   // TJSONSerializeMethod
   //
   TJSONSerializeMethod = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
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
// -----------------------------------------------------------------

uses dwsConstExprs, dwsJSONScript;

const
   SYS_JSON = 'JSON';
   SYS_JSONVARIANT = 'JSONVariant';
   SYS_JSON_STRINGIFY_UTF8 = 'StringifyUTF8';
   SYS_JSON_PARSE = 'Parse';
   SYS_JSON_PARSE_UTF8 = 'ParseUTF8';
   SYS_JSON_PARSE_INTEGER_ARRAY = 'ParseIntegerArray';
   SYS_JSON_PARSE_FLOAT_ARRAY = 'ParseFloatArray';
   SYS_JSON_PARSE_STRING_ARRAY = 'ParseStringArray';
   SYS_JSON_NEWOBJECT = 'NewObject';
   SYS_JSON_NEWARRAY = 'NewArray';
   SYS_JSON_SERIALIZE = 'Serialize';

type
   TBoxedJSONValue = class (TInterfacedObject,
                            IBoxedJSONValue, IJSONWriteAble,
                            ICoalesceable, INullable, IGetSelf, IUnknown)
      FValue : TdwsJSONValue;

      constructor Create(wrapped : TdwsJSONValue);
      destructor Destroy; override;

      function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; stdcall;

      function GetSelf : TObject;

      function ToString : String; override; final;
      function ToUnicodeString : UnicodeString; virtual;

      function Value : TdwsJSONValue;

      function IsFalsey : Boolean;
      function IsNull : Boolean;
      function IsDefined : Boolean;

      procedure WriteToJSON(writer : TdwsJSONWriter);

      class procedure Allocate(wrapped : TdwsJSONValue; var v : Variant); static;
      class procedure AllocateOrGetImmediate(wrapped : TdwsJSONValue; var v : Variant); static;

      class function UnBox(const v : Variant) : TdwsJSONValue; static;
   end;

   TBoxedNilJSONValue = class (TInterfacedObject, IBoxedJSONValue, ICoalesceable, IGetSelf, IUnknown)
      function GetSelf : TObject;
      function ToString : String; override; final;
      function ToUnicodeString : String;
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
function TBoxedJSONValue.ToString : String;
begin
   Result := String(ToUnicodeString);
end;

// ToUnicodeString
//
function TBoxedJSONValue.ToUnicodeString : UnicodeString;
begin
   if FValue.ValueType = jvtString then
      Result := FValue.AsString
   else Result := FValue.ToUnicodeString;
end;

// IsFalsey
//
function TBoxedJSONValue.IsFalsey : Boolean;
begin
   Result:=FValue.IsFalsey;
end;

// IsNull
//
function TBoxedJSONValue.IsNull : Boolean;
begin
   Result := FValue.IsNull;
end;

// IsDefined
//
function TBoxedJSONValue.IsDefined : Boolean;
begin
   Result := FValue.IsDefined;
end;

// WriteToJSON
//
procedure TBoxedJSONValue.WriteToJSON(writer : TdwsJSONWriter);
begin
   if Value <> nil then
      Value.WriteTo(writer)
   else writer.WriteString('Undefined');
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
      VarClear(v)
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
   Result := '';
end;

// ToUnicodeString
//
function TBoxedNilJSONValue.ToUnicodeString : String;
begin
   Result := '';
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
   TJSONStringifyUTF8Method.Create(
      table, SYS_JSON_STRINGIFY_UTF8, ['val', SYS_ANY_TYPE], SYS_STRING,
      [iffStateLess, iffStaticMethod], jsonObject
   );

   TJSONParseMethod.Create(
      table, SYS_JSON_PARSE, ['str', SYS_STRING], SYS_JSONVARIANT,
      [iffStaticMethod], jsonObject, ''
   );
   TJSONParseUTF8Method.Create(
      table, SYS_JSON_PARSE_UTF8, ['str', SYS_STRING], SYS_JSONVARIANT,
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

   TJSONSerializeMethod.Create(
      table, SYS_JSON_SERIALIZE, ['val', SYS_ANY_TYPE], SYS_JSONVARIANT,
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
   FDeleteCall:=TdwsJSONDeleteCall.Create;
   FSwapCall:=TdwsJSONSwapCall.Create;
   FToStringCall:=TdwsJSONToStringCall.Create;

   FLengthMember:=TdwsJSONConnectorLengthMember.Create('length');
end;

// ConnectorCaption
//
function TdwsJSONConnectorType.ConnectorCaption : String;
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
function TdwsJSONConnectorType.WritableReads(const memberName : String) : Boolean;
begin
   Result := False;
end;

// HasMethod
//
function TdwsJSONConnectorType.HasMethod(const methodName : String; const params : TConnectorParamArray;
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

   end else if UnicodeSameText(methodName, 'delete') then begin

      if Length(params)<>1 then
         raise ECompileException.CreateFmt(CPE_BadNumberOfParameters, [1, Length(params)]);
      paramTyp := params[0].TypSym;
      if not ((paramTyp.UnAliasedType is TBaseStringSymbol) or (paramTyp.UnAliasedType is TBaseIntegerSymbol)) then
         raise ECompileException.CreateFmt(CPE_BadParameterType, [0, SYS_STRING + ' or ' + SYS_INTEGER, paramTyp.Caption]);

      Result:=FDeleteCall;
      typSym:=nil;

   end else if UnicodeSameText(methodName, 'swap') then begin

      if Length(params)<>2 then
         raise ECompileException.CreateFmt(CPE_BadNumberOfParameters, [2, Length(params)]);
      paramTyp := params[0].TypSym;
      if not (paramTyp.UnAliasedType is TBaseIntegerSymbol) then
         raise ECompileException.CreateFmt(CPE_BadParameterType, [0, SYS_INTEGER, paramTyp.Caption]);

      Result:=FSwapCall;
      typSym:=nil;

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
function TdwsJSONConnectorType.HasMember(const memberName : String; var typSym : TTypeSymbol;
                                         isWrite : Boolean) : IConnectorMember;
begin
   typSym:=TypJSONVariant;
   if memberName='length' then
      Result:=FLengthMember
   else Result := TdwsJSONConnectorMember.Create(memberName);
end;

// HasIndex
//
function TdwsJSONConnectorType.HasIndex(const propName : String; const params : TConnectorParamArray;
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
      {$ifdef FPC}
      varString :
         vt:=jvtString;
      {$else}
      varUString :
         vt:=jvtString;
      {$endif}
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
   else VarClear(result);
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
               {$ifdef FPC}
               varString : baseArray.Add(UnicodeString(String(pParam^.VString)));
               {$else}
               varUString : baseArray.Add(String(pParam^.VString));
               {$endif}
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
// ------------------ TdwsJSONDeleteCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONDeleteCall.FastCall(const args : TExprBaseListExec; var result : Variant);
var
   base : Variant;
   baseValue : TdwsJSONValue;
   name : String;
begin
   args.EvalAsVariant(0, base);
   baseValue := TBoxedJSONValue.UnBox(base);
   case baseValue.ValueType of
      jvtObject : begin
         args.EvalAsString(1, name);
         TdwsJSONObject(baseValue).Delete(name);
      end;
      jvtArray  : begin
         TdwsJSONArray(baseValue).Delete(args.ExprBase[1].EvalAsInteger(args.Exec));
      end;
   else
      raise EdwsJSONException.Create('JSON Object or Array required for Delete method');
   end;
end;

// ------------------
// ------------------ TdwsJSONSwapCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONSwapCall.FastCall(const args : TExprBaseListExec; var result : Variant);
var
   base : Variant;
   baseValue : TdwsJSONValue;
begin
   args.EvalAsVariant(0, base);
   baseValue := TBoxedJSONValue.UnBox(base);
   case baseValue.ValueType of
      jvtArray  : begin
         TdwsJSONArray(baseValue).Swap(args.ExprBase[1].EvalAsInteger(args.Exec),
                                       args.ExprBase[2].EvalAsInteger(args.Exec));
      end;
   else
      raise EdwsJSONException.Create('JSON Array required for Swap method');
   end;
end;

// ------------------
// ------------------ TdwsJSONToStringCall ------------------
// ------------------

// FastCall
//
procedure TdwsJSONToStringCall.FastCall(const args : TExprBaseListExec; var result : Variant);
begin
   VarCopySafe(result, '');
   JSONScript.StringifyArgs(args, String(PVarData(@Result)^.VString));
end;

// ------------------
// ------------------ TdwsJSONIndexCall ------------------
// ------------------

// Create
//
constructor TdwsJSONIndexCall.Create(const propName : String);
begin
   inherited Create;
   FPropName := UnicodeString(propName);
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
   VarClear(Result);
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
      case baseValue.ValueType of
         jvtObject, jvtArray : ;
         jvtUndefined :
            raise EdwsJSONException.Create('Cannot set items of Undefined');
      else
         raise EdwsJSONException.Create('Cannot set items of Immediate');
      end;

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
         {$ifdef FPC}
         varString : begin
            argValue:=TdwsJSONImmediate.Create;
            argValue.AsString:=UnicodeString(String(pVal^.VString));
         end;
         {$else}
         varUString : begin
            argValue:=TdwsJSONImmediate.Create;
            argValue.AsString:=String(pVal^.VUString);
         end;
         {$endif}
         varBoolean : begin
            argValue:=TdwsJSONImmediate.Create;
            argValue.AsBoolean:=pVal^.VBoolean;
         end;
         varNull : begin
            argValue:=TdwsJSONImmediate.FromVariant(Null);
         end;
         varEmpty : begin
            argValue:=TdwsJSONImmediate.Create;
         end;
      else
         if VarIsNumeric(val) then begin
            argValue:=TdwsJSONImmediate.Create;
            argValue.AsNumber:=val;
         end else raise Exception.Create('Unsupported assignment');
      end;
      args.ExprBase[1].EvalAsVariant(args.Exec, val);
      baseValue.Values[val] := argValue;
   end else begin
      raise EdwsJSONException.Create('Cannot set items of Undefined');
   end;
end;

// ------------------
// ------------------ TdwsJSONConnectorMember ------------------
// ------------------

// Create
//
constructor TdwsJSONConnectorMember.Create(const memberName : String);
begin
   inherited Create;
   SetMemberName(UnicodeString(memberName));
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
   end else VarClear(result);
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
   case baseValue.ValueType of
      jvtObject : ;
      jvtArray :
         if StrUToInt64(FMemberName, -1) < 0 then
            raise EdwsJSONException.CreateFmt('Invalid array member "%s"', [FMemberName]);
      jvtUndefined :
         raise EdwsJSONException.CreateFmt('Cannot set member "%s" of Undefined', [FMemberName]);
   else
      raise EdwsJSONException.CreateFmt('Cannot set member "%s" of Immediate', [FMemberName]);
   end;

   value.EvalAsVariant(exec, v);
   case PVarData(@v)^.VType of
      varUnknown : begin
         dataValue := TBoxedJSONValue.UnBox(v);
         if dataValue = nil then
            dataValue := TdwsJSONImmediate.FromVariant(Null)
         else begin
            if dataValue.Owner=nil then
               dataValue.IncRefCount;
         end;
      end;
      varEmpty :
         dataValue := TdwsJSONImmediate.Create;
   else
      dataValue := TdwsJSONImmediate.FromVariant(v);
   end;
   baseValue.HashedItems[FMemberHash, FMemberName] := dataValue;
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
   end else VarClear(result);
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
function TJSONConnectorSymbol.CreateAssignExpr(context : TdwsCompilerContext; const aScriptPos: TScriptPos;
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
      Result:=TAssignExpr.Create(context, aScriptPos, left, right)
   else if rightTypClass.InheritsFrom(TBaseSymbol) then
      Result:=TAssignBoxJSONExpr.Create(context, aScriptPos, left, right);

   if Result=nil then begin
      context.Msgs.AddCompilerErrorFmt(aScriptPos, CPE_AssignIncompatibleTypes,
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
   json : String;
begin
   args.EvalAsString(0, json);
   if json <> '' then begin
      v:=TdwsJSONValue.ParseString(json);
      if v=nil then
         box:=nil
      else box:=TBoxedJSONValue.Create(v);
      VarCopySafe(result, IBoxedJSONValue(box));
   end else VarClearSafe(result);
end;

// ------------------
// ------------------ TJSONParseUTF8Method ------------------
// ------------------

procedure TJSONParseUTF8Method.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   v : TdwsJSONValue;
   box : TBoxedJSONValue;
   json : String;
   jsonUTF8 : RawByteString;
begin
   args.EvalAsString(0, json);
   if json <> '' then begin
      ScriptStringToRawByteString(json, jsonUTF8);
      json := UTF8ToString(jsonUTF8);
      v:=TdwsJSONValue.ParseString(json);
      if v=nil then
         box:=nil
      else box:=TBoxedJSONValue.Create(v);
      VarCopySafe(result, IBoxedJSONValue(box));
   end else VarClearSafe(result);
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
   s := UnicodeString(args.AsString[0]);

   tokenizer:=TdwsJSONParserState.Create(s);
   values:=TSimpleInt64List.Create;
   try
      tokenizer.ParseIntegerArray(values);

      newArray:=TScriptDynamicArray.CreateNew((args.Exec as TdwsProgramExecution).CompilerContext.TypInteger);
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
   s := UnicodeString(args.AsString[0]);

   tokenizer:=TdwsJSONParserState.Create(s);
   values:=TSimpleDoubleList.Create;
   try
      tokenizer.ParseNumberArray(values);

      newArray:=TScriptDynamicArray.CreateNew((args.Exec as TdwsProgramExecution).CompilerContext.TypInteger);
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
   values : TUnicodeStringList;
   i : Integer;
   newArray : TScriptDynamicArray;
   newPData : PData;
   s : UnicodeString;
begin
   s := UnicodeString(args.AsString[0]);

   tokenizer:=TdwsJSONParserState.Create(s);
   values := TUnicodeStringList.Create;
   try
      tokenizer.ParseStringArray(values);

      newArray:=TScriptDynamicArray.CreateNew((args.Exec as TdwsProgramExecution).CompilerContext.TypString);
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
procedure TJSONStringifyMethod.DoEvalAsString(const args : TExprBaseListExec; var Result : String);
begin
   JSONScript.StringifyArgs(args, Result);
end;

// ------------------
// ------------------ TJSONStringifyUTF8Method ------------------
// ------------------

// DoEvalAsString
//
procedure TJSONStringifyUTF8Method.DoEvalAsString(const args : TExprBaseListExec; var Result : String);
var
   bufUTF8 : RawByteString;
begin
   JSONScript.StringifyArgs(args, Result);
   bufUTF8 := UTF8Encode(Result);
   RawByteStringToScriptString(bufUTF8, Result);
end;

// ------------------
// ------------------ TJSONSerializeMethod ------------------
// ------------------

// DoEvalAsVariant
//
procedure TJSONSerializeMethod.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   v : TdwsJSONValue;
   box : TBoxedJSONValue;
   js : String;
begin
   JSONScript.StringifyArgs(args, js);
   v:=TdwsJSONValue.ParseString(js);
   if v=nil then
      box:=nil
   else box:=TBoxedJSONValue.Create(v);
   VarCopySafe(result, IBoxedJSONValue(box));
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
   if VarIsEmpty(rv) then
      Left.AssignValue(exec, rv)
   else begin
      v:=TdwsJSONImmediate.FromVariant(rv);
      box:=TBoxedJSONValue.Create(v);
      Left.AssignValue(exec, IBoxedJSONValue(box));
   end;
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
