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
   dwsFunctions, dwsJSON, dwsMagicExprs;

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

   IJSONTypeName = interface(IConnectorCall) end;
   IJSONElementName = interface(IConnectorCall) end;
   IJSONLow = interface(IConnectorCall) end;
   IJSONHigh = interface(IConnectorCall) end;
   IJSONLength = interface(IConnectorCall) end;
   IJSONClone = interface(IConnectorCall) end;
   IJSONExtend = interface(IConnectorCall) end;
   IJSONAdd = interface(IConnectorCall) end;

   // TdwsJSONConnectorType
   //
   TdwsJSONConnectorType = class (TInterfacedSelfObject, IConnectorType,
                                  IJSONTypeName, IJSONElementName,
                                  IJSONLow, IJSONHigh, IJSONLength,
                                  IJSONClone, IJSONExtend, IJSONAdd)
      private
         FTable : TSymbolTable;
         FLowValue : TData;

      protected
         function ConnectorCaption : UnicodeString;
         function AcceptsParams(const params : TConnectorParamArray) : Boolean;
         function NeedDirectReference : Boolean;

         function HasMethod(const methodName : UnicodeString; const params : TConnectorParamArray;
                            var typSym : TTypeSymbol) : IConnectorCall;
         function HasMember(const memberName : UnicodeString; var typSym : TTypeSymbol;
                            isWrite : Boolean) : IConnectorMember;
         function HasIndex(const propName : UnicodeString; const params : TConnectorParamArray;
                           var typSym : TTypeSymbol; isWrite : Boolean) : IConnectorCall;
         function HasEnumerator(var typSym: TTypeSymbol) : IConnectorEnumerator;

         function TypeNameCall(const base : Variant; const args : TConnectorArgs) : TData;
         function ElementNameCall(const base : Variant; const args : TConnectorArgs) : TData;
         function LowCall(const base : Variant; const args : TConnectorArgs) : TData;
         function HighCall(const base : Variant; const args : TConnectorArgs) : TData;
         function LengthCall(const base : Variant; const args : TConnectorArgs) : TData;
         function CloneCall(const base : Variant; const args : TConnectorArgs) : TData;
         function ExtendCall(const base : Variant; const args : TConnectorArgs) : TData;
         function AddCall(const base : Variant; const args : TConnectorArgs) : TData;

         function IJSONTypeName.Call = TypeNameCall;
         function IJSONElementName.Call = ElementNameCall;
         function IJSONLow.Call = LowCall;
         function IJSONHigh.Call = HighCall;
         function IJSONLength.Call = LengthCall;
         function IJSONClone.Call = CloneCall;
         function IJSONExtend.Call = ExtendCall;
         function IJSONAdd.Call = AddCall;

      public
         constructor Create(table : TSymbolTable);
   end;

   // TdwsJSONIndexCall
   //
   TdwsJSONIndexCall = class(TInterfacedSelfObject, IUnknown, IConnectorCall)
      private
         FMethodName : UnicodeString;

      protected
         function Call(const base : Variant; const args : TConnectorArgs) : TData; virtual; abstract;
         function NeedDirectReference : Boolean;

      public
         constructor Create(const methodName : UnicodeString);

         property CallMethodName : UnicodeString read FMethodName write FMethodName;
   end;

   // TdwsJSONIndexReadCall
   //
   TdwsJSONIndexReadCall = class(TdwsJSONIndexCall)
      protected
         function Call(const base : Variant; const args : TConnectorArgs) : TData; override;
   end;

   // TdwsJSONIndexWriteCall
   //
   TdwsJSONIndexWriteCall = class(TdwsJSONIndexCall)
      protected
         function Call(const base : Variant; const args : TConnectorArgs) : TData; override;
   end;

   // TdwsJSONConnectorMember
   //
   TdwsJSONConnectorMember = class(TInterfacedSelfObject, IUnknown, IConnectorMember)
      private
         FMemberName : UnicodeString;

      protected
         function Read(const base : Variant) : TData;
         procedure Write(const base : Variant; const data : TData);

      public
         constructor Create(const memberName : UnicodeString);

         property MemberName : UnicodeString read FMemberName write FMemberName;
   end;

   // TJSONConnectorSymbol
   //
   TJSONConnectorSymbol = class(TConnectorSymbol)
      public
         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
   end;

   // TJSONParseMethod
   //
   TJSONParseMethod = class(TInternalStaticMethod)
      procedure Execute(info : TProgramInfo); override;
   end;

   // TJSONNewObject
   //
   TJSONNewObject = class(TInternalStaticMethod)
      procedure Execute(info : TProgramInfo); override;
   end;

   // TJSONNewArray
   //
   TJSONNewArray = class(TInternalStaticMethod)
      procedure Execute(info : TProgramInfo); override;
   end;

   // TJSONStringifyMethod
   //
   TJSONStringifyMethod = class (TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString); override;

      class procedure StringifyVariant(exec : TdwsExecution; writer : TdwsJSONWriter; const v : Variant); static;
      class procedure StringifySymbol(exec : TdwsExecution; writer : TdwsJSONWriter; sym : TSymbol; const dataPtr : IDataContext); static;
      class procedure StringifyDynamicArray(exec : TdwsExecution; writer : TdwsJSONWriter; dynArray : TScriptDynamicArray); static;
      class procedure StringifyArray(exec : TdwsExecution; writer : TdwsJSONWriter; elemSym : TSymbol;
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
   SYS_JSON_NEWOBJECT = 'NewObject';
   SYS_JSON_NEWARRAY = 'NewArray';

type
   TBoxedJSONValue = class (TInterfacedSelfObject, IBoxedJSONValue)
      FValue : TdwsJSONValue;

      constructor Create(wrapped : TdwsJSONValue);
      destructor Destroy; override;

      function Value : TdwsJSONValue;
      function ToString : UnicodeString; override;

      class procedure Allocate(wrapped : TdwsJSONValue; var v : Variant); static;
      class procedure AllocateOrGetImmediate(wrapped : TdwsJSONValue; var v : Variant); static;

      class function UnBox(p : PVarData) : TdwsJSONValue; static;
   end;

   TBoxedNilJSONValue = class (TInterfacedSelfObject, IBoxedJSONValue)
      function Value : TdwsJSONValue;
      function ToString : UnicodeString; override;
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
   Result:=FValue.ToString;
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
   if wrapped.IsImmediateValue then
      v:=TdwsJSONImmediate(wrapped).AsVariant
   else if wrapped<>nil then begin
      wrapped.IncRefCount;
      TBoxedJSONValue.Allocate(wrapped, v);
   end else v:=vNilJSONValue;
end;

// UnBox
//
class function TBoxedJSONValue.UnBox(p : PVarData) : TdwsJSONValue;
var
   boxed : IBoxedJSONValue;
begin
   boxed:=(IUnknown(p^.VUnknown) as IBoxedJSONValue);
   if boxed<>nil then
      Result:=boxed.Value
   else Result:=nil;
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
   connSym : TJSONConnectorSymbol;
   jsonObject : TClassSymbol;
   anyType : TAnyTypeSymbol;
begin
   connSym:=TJSONConnectorSymbol.Create(SYS_JSONVARIANT, TdwsJSONConnectorType.Create(table));
   table.AddSymbol(connSym);

   jsonObject:=TClassSymbol.Create(SYS_JSON, nil);
   jsonObject.InheritFrom(table.TypObject);
   table.AddSymbol(jsonObject);
   jsonObject.IsStatic:=True;
   jsonObject.IsSealed:=True;
   jsonObject.SetNoVirtualMembers;

   anyType:=TAnyTypeSymbol.Create('Any Type', nil);
   table.AddSymbol(anyType);

   TJSONStringifyMethod.Create(table, SYS_JSON_STRINGIFY,
                               ['val', 'Any Type'], SYS_STRING,
                               [iffStateLess, iffStaticMethod],
                               jsonObject);

   TJSONParseMethod.Create(mkClassFunction, [maStatic], SYS_JSON_PARSE,
                           ['str', SYS_STRING], SYS_JSONVARIANT,
                           jsonObject, cvPublic, table);
   TJSONNewObject.Create(mkClassFunction, [maStatic], SYS_JSON_NEWOBJECT,
                         [], SYS_JSONVARIANT,
                         jsonObject, cvPublic, table);
   TJSONNewArray.Create(mkClassFunction, [maStatic], SYS_JSON_NEWARRAY,
                        [], SYS_JSONVARIANT,
                        jsonObject, cvPublic, table);
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
constructor TdwsJSONConnectorType.Create(table : TSymbolTable);
begin
   inherited Create;

   FTable:=table;

   SetLength(FLowValue, 1);
   FLowValue[0]:=0;
end;

// ConnectorCaption
//
function TdwsJSONConnectorType.ConnectorCaption : UnicodeString;
begin
   Result:='JSON Connector 1.0';
end;

// AcceptsParams
//
function TdwsJSONConnectorType.AcceptsParams(const params : TConnectorParamArray) : Boolean;
begin
   Result:=True;
end;

// NeedDirectReference
//
function TdwsJSONConnectorType.NeedDirectReference : Boolean;
begin
   Result:=False;
end;

// HasMethod
//
function TdwsJSONConnectorType.HasMethod(const methodName : UnicodeString; const params : TConnectorParamArray;
                                         var typSym : TTypeSymbol) : IConnectorCall;
var
   paramTyp : TTypeSymbol;
begin
   if UnicodeSameText(methodName, 'typename') then begin

      Result:=IJSONTypeName(Self);
      typSym:=FTable.FindTypeSymbol(SYS_STRING, cvMagic);

      if Length(params)<>0 then
         raise ECompileException.Create(CPE_NoParamsExpected);

   end else if UnicodeSameText(methodName, 'elementname') then begin

      if Length(params)<>1 then
         raise ECompileException.CreateFmt(CPE_BadNumberOfParameters, [1, Length(params)]);
      paramTyp:=params[0].TypSym;
      if not paramTyp.UnAliasedTypeIs(TBaseIntegerSymbol) then
         raise ECompileException.CreateFmt(CPE_BadParameterType, [0, SYS_INTEGER, paramTyp.Caption]);

      Result:=IJSONElementName(Self);
      typSym:=FTable.FindTypeSymbol(SYS_STRING, cvMagic);

   end else if UnicodeSameText(methodName, 'extend') then begin

      if Length(params)<>1 then
         raise ECompileException.CreateFmt(CPE_BadNumberOfParameters, [1, Length(params)]);
      paramTyp:=params[0].TypSym;
      if paramTyp.UnAliasedType<>FTable.FindTypeSymbol(SYS_JSONVARIANT, cvMagic) then
         raise ECompileException.CreateFmt(CPE_BadParameterType, [0, SYS_JSONVARIANT, paramTyp.Caption]);

      Result:=IJSONExtend(Self);
      typSym:=nil;

   end else if UnicodeSameText(methodName, 'add') then begin

      if Length(params)<>1 then
         raise ECompileException.CreateFmt(CPE_BadNumberOfParameters, [1, Length(params)]);
      if not params[0].TypSym.UnAliasedType.IsBaseType then
         raise ECompileException.CreateFmt(CPE_InvalidParameterType, [0, SYS_JSONVARIANT, params[0].TypSym.Caption]);

      Result:=IJSONAdd(Self);
      typSym:=FTable.FindTypeSymbol(SYS_INTEGER, cvMagic);

   end else begin

      if Length(params)<>0 then
         raise ECompileException.Create(CPE_NoParamsExpected);

      if UnicodeSameText(methodName, 'clone') then begin

         typSym:=FTable.FindTypeSymbol(SYS_JSONVARIANT, cvMagic);
         Result:=IJSONClone(Self);

      end else begin

         typSym:=FTable.FindTypeSymbol(SYS_INTEGER, cvMagic);
         if UnicodeSameText(methodName, 'length') then
            Result:=IJSONLength(Self)
         else if UnicodeSameText(methodName, 'low') then
            Result:=IJSONLow(Self)
         else if UnicodeSameText(methodName, 'high') then
            Result:=IJSONHigh(Self)
         else Result:=nil;

      end;

   end;
end;

// HasMember
//
function TdwsJSONConnectorType.HasMember(const memberName : UnicodeString; var typSym : TTypeSymbol;
                                         isWrite : Boolean) : IConnectorMember;
begin
   typSym:=FTable.FindTypeSymbol(SYS_JSONVARIANT, cvMagic);
   Result:=TdwsJSONConnectorMember.Create(memberName);
end;

// HasIndex
//
function TdwsJSONConnectorType.HasIndex(const propName : UnicodeString; const params : TConnectorParamArray;
                                      var typSym : TTypeSymbol; isWrite : Boolean) : IConnectorCall;
begin
   typSym:=FTable.FindTypeSymbol(SYS_JSONVARIANT, cvMagic);
   if isWrite then
      Result:=TdwsJSONIndexWriteCall.Create(propName)
   else Result:=TdwsJSONIndexReadCall.Create(propName);
end;

// HasEnumerator
//
function TdwsJSONConnectorType.HasEnumerator(var typSym: TTypeSymbol) : IConnectorEnumerator;
begin
   Result:=nil;
end;

// TypeNameCall
//
function TdwsJSONConnectorType.TypeNameCall(const base : Variant; const args : TConnectorArgs) : TData;
var
   box : IBoxedJSONValue;
begin
   SetLength(Result, 1);
   case PVarData(@base)^.VType of
      varUnknown : begin
         box:=IBoxedJSONValue(IUnknown(base));
         Result[0]:=TdwsJSONValue.ValueTypeStrings[box.Value.ValueType];
      end;
      varUString :
         Result[0]:=TdwsJSONValue.ValueTypeStrings[jvtString];
      varDouble :
         Result[0]:=TdwsJSONValue.ValueTypeStrings[jvtNumber];
      varBoolean :
         Result[0]:=TdwsJSONValue.ValueTypeStrings[jvtBoolean];
      varNull :
         Result[0]:=TdwsJSONValue.ValueTypeStrings[jvtNull];
   else
      Result[0]:=TdwsJSONValue.ValueTypeStrings[jvtUndefined];
   end;
end;

// ElementNameCall
//
function TdwsJSONConnectorType.ElementNameCall(const base : Variant; const args : TConnectorArgs) : TData;
var
   box : IBoxedJSONValue;
begin
   SetLength(Result, 1);
   if PVarData(@base)^.VType=varUnknown then begin
      box:=IBoxedJSONValue(IUnknown(base));
      Result[0]:=box.Value.Names[args[0][0]];
   end else Result[0]:='';
end;

// LowCall
//
function TdwsJSONConnectorType.LowCall(const base : Variant; const args : TConnectorArgs) : TData;
begin
   Result:=FLowValue;
end;

// HighCall
//
function TdwsJSONConnectorType.HighCall(const base : Variant; const args : TConnectorArgs) : TData;
var
   p : PVarData;
   n : Integer;
begin
   p:=PVarData(@base);
   if p^.VType=varUnknown then
      n:=IBoxedJSONValue(IUnknown(p^.VUnknown)).Value.ElementCount
   else n:=0;
   SetLength(Result, 1);
   Result[0]:=n-1;
end;

// LengthCall
//
function TdwsJSONConnectorType.LengthCall(const base : Variant; const args : TConnectorArgs) : TData;
var
   p : PVarData;
   n : Integer;
begin
   p:=PVarData(@base);
   if p^.VType=varUnknown then
      n:=IBoxedJSONValue(IUnknown(p^.VUnknown)).Value.ElementCount
   else n:=0;
   SetLength(Result, 1);
   Result[0]:=n;
end;

// CloneCall
//
function TdwsJSONConnectorType.CloneCall(const base : Variant; const args : TConnectorArgs) : TData;
var
   p : PVarData;
   v : TdwsJSONValue;
begin
   SetLength(Result, 1);
   p:=PVarData(@base);
   if p^.VType=varUnknown then begin
      v:=IBoxedJSONValue(p^.VUnknown).Value.Clone;
      Result[0]:=IBoxedJSONValue(TBoxedJSONValue.Create(v));
   end else Result[0]:=vNilJSONValue;
end;

// ExtendCall
//
function TdwsJSONConnectorType.ExtendCall(const base : Variant; const args : TConnectorArgs) : TData;
var
   pBase, pParam : PVarData;
begin
   Result:=nil;
   pBase:=PVarData(@base);
   if pBase^.VType=varUnknown then begin
      pParam:=PVarData(@args[0][0]);
      if (pParam^.VType=varUnknown) and (pParam.VUnknown<>nil) then
         IBoxedJSONValue(pBase^.VUnknown).Value.Extend(IBoxedJSONValue(pParam^.VUnknown).Value);
   end;
end;

// AddCall
//
function TdwsJSONConnectorType.AddCall(const base : Variant; const args : TConnectorArgs) : TData;
var
   pBase, pParam : PVarData;
   baseValue : TdwsJSONValue;
   baseArray : TdwsJSONArray;
begin
   Result:=nil;
   pBase:=PVarData(@base);
   if pBase^.VType=varUnknown then begin
      baseValue:=IBoxedJSONValue(pBase^.VUnknown).Value;
      if baseValue.ValueType=jvtArray then begin
         baseArray:=TdwsJSONArray(baseValue);
         pParam:=PVarData(@args[0][0]);
         case pParam^.VType of
            varInt64 : baseArray.Add(pParam^.VInt64);
            varDouble : baseArray.Add(pParam^.VDouble);
            varUString : baseArray.Add(String(pParam^.VUString));
            varBoolean : baseArray.Add(pParam^.VBoolean);
         else
            raise EdwsJSONException.Create('JSON Array Add unsupported type');
         end;
         if (pParam^.VType=varUnknown) and (pParam.VUnknown<>nil) then
            baseArray.Add(IBoxedJSONValue(pParam^.VUnknown).Value);
      end else raise EdwsJSONException.Create('JSON Array required for Add method');
   end;
end;

// ------------------
// ------------------ TdwsJSONIndexCall ------------------
// ------------------

// Create
//
constructor TdwsJSONIndexCall.Create(const methodName : UnicodeString);
begin
   inherited Create;
   FMethodName:=methodName;
end;

// NeedDirectReference
//
function TdwsJSONIndexCall.NeedDirectReference : Boolean;
begin
   Result:=False;
end;

// ------------------
// ------------------ TdwsJSONIndexReadCall ------------------
// ------------------

// Call
//
function TdwsJSONIndexReadCall.Call(const base : Variant; const args : TConnectorArgs) : TData;
var
   p : PVarData;
   v : TdwsJSONValue;
begin
   SetLength(Result, 1);
   p:=PVarData(@base);
   if p^.VType=varUnknown then begin
      v:=TBoxedJSONValue.UnBox(p);
      if v<>nil then begin
         if FMethodName<>'' then
            v:=v.Items[FMethodName];
         v:=v.Values[args[0][0]];
         TBoxedJSONValue.AllocateOrGetImmediate(v, Result[0]);
         Exit;
      end;
   end;
   Result[0]:=vNilJSONValue;
end;

// ------------------
// ------------------ TdwsJSONIndexWriteCall ------------------
// ------------------

// Call
//
function TdwsJSONIndexWriteCall.Call(const base : Variant; const args : TConnectorArgs) : TData;
var
   pBase, pVal : PVarData;
   baseValue, argValue : TdwsJSONValue;
begin
   pBase:=PVarData(@base);
   if pBase^.VType=varUnknown then begin
      baseValue:=TBoxedJSONValue.UnBox(pBase);
      if FMethodName<>'' then
         baseValue:=baseValue.Items[FMethodName];
      pVal:=PVarData(@args[1][0]);
      case pVal^.VType of
         varUnknown : begin
            argValue:=TBoxedJSONValue.UnBox(pVal);
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
      else
         raise Exception.Create('Unsupported assignment');
      end;
      baseValue.Values[args[0][0]]:=argValue;
   end else begin
      raise Exception.CreateFmt('Invalid JSON write to %s', [FMethodName]);
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
   FMemberName:=memberName;
end;

// Read
//
function TdwsJSONConnectorMember.Read(const base : Variant) : TData;
var
   p : PVarData;
   v : TdwsJSONValue;
begin
   SetLength(Result, 1);
   p:=PVarData(@base);
   if p^.VType=varUnknown then begin
      v:=TBoxedJSONValue.UnBox(p).Items[FMemberName];
      TBoxedJSONValue.AllocateOrGetImmediate(v, Result[0])
   end else Result[0]:=vNilJSONValue;
end;

// Write
//
procedure TdwsJSONConnectorMember.Write(const base : Variant; const data : TData);
var
   p : PVarData;
   baseValue, dataValue : TdwsJSONValue;
begin
   p:=PVarData(@base);
   if p^.VType=varUnknown then
      baseValue:=IBoxedJSONValue(IUnknown(p^.VUnknown)).Value
   else baseValue:=nil;

   if baseValue<>nil then begin
      p:=PVarData(@data[0]);
      if p^.VType=varUnknown then begin
         dataValue:=TBoxedJSONValue.UnBox(p);
         if dataValue=nil then
            dataValue:=TdwsJSONImmediate.FromVariant(Null)
         else begin
            if dataValue.Owner=nil then
               dataValue.IncRefCount;
         end;
      end else dataValue:=TdwsJSONImmediate.FromVariant(Variant(p^));
   end else dataValue:=nil;

   baseValue.Items[FMemberName]:=dataValue;
end;

// ------------------
// ------------------ TJSONConnectorSymbol ------------------
// ------------------

// IsCompatible
//
function TJSONConnectorSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   Result:=   inherited IsCompatible(typSym)
           or typSym.IsFuncSymbol
           or (typSym is TRecordSymbol);
end;

// ------------------
// ------------------ TJSONParseMethod ------------------
// ------------------

// Execute
//
procedure TJSONParseMethod.Execute(info : TProgramInfo);
var
   v : TdwsJSONValue;
   box : TBoxedJSONValue;
begin
   v:=TdwsJSONValue.ParseString(info.ParamAsString[0]);
   if v=nil then
      box:=TBoxedJSONValue.Create(TdwsJSONObject.Create)
   else box:=TBoxedJSONValue.Create(v);
   Info.ResultAsVariant:=IBoxedJSONValue(box);
end;

// ------------------
// ------------------ TJSONNewObject ------------------
// ------------------

// Execute
//
procedure TJSONNewObject.Execute(info : TProgramInfo);
var
   v : TdwsJSONValue;
   box : TBoxedJSONValue;
begin
   v:=TdwsJSONObject.Create;
   box:=TBoxedJSONValue.Create(v);
   Info.ResultAsVariant:=IBoxedJSONValue(box);
end;

// ------------------
// ------------------ TJSONNewArray ------------------
// ------------------

// Execute
//
procedure TJSONNewArray.Execute(info : TProgramInfo);
var
   v : TdwsJSONValue;
   box : TBoxedJSONValue;
begin
   v:=TdwsJSONArray.Create;
   box:=TBoxedJSONValue.Create(v);
   Info.ResultAsVariant:=IBoxedJSONValue(box);
end;

// ------------------
// ------------------ TJSONStringifyMethod ------------------
// ------------------

// DoEvalAsString
//
procedure TJSONStringifyMethod.DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString);
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
   boxedJSON : IBoxedJSONValue;
   scriptObj : IScriptObj;
   scriptObjSelf : TObject;
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
      varUnknown : begin
         unk:=IUnknown(p^.VUnknown);
         if unk=nil then
            writer.WriteNull
         else if unk.QueryInterface(IBoxedJSONValue, boxedJSON)=0 then begin
            if boxedJSON.Value<>nil then
               boxedJSON.Value.WriteTo(writer)
            else writer.WriteString('Undefined');
         end else begin
            if unk.QueryInterface(IScriptObj, scriptObj)=0 then begin
               scriptObjSelf:=scriptObj.GetSelf;
               if scriptObjSelf is TScriptDynamicArray then
                  StringifyDynamicArray(exec, writer, TScriptDynamicArray(scriptObjSelf))
               else StringifyClass(exec, writer, scriptObj.ClassSym, scriptObj);
            end else begin
               if unk.QueryInterface(IGetSelf, getSelf)=0 then
                  writer.WriteString(getSelf.ToString)
               else writer.WriteString('IUnknown');
            end;
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
      StringifyDynamicArray(exec, writer, IScriptObj(dataPtr.AsInterface[0]).GetSelf as TScriptDynamicArray)
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
   writer : TdwsJSONWriter; elemSym : TSymbol; const dataPtr : IDataContext; nb : Integer);
var
   i, s : Integer;
   locData : IDataContext;
begin
   s:=elemSym.Size;
   writer.BeginArray;
   for i:=0 to nb-1 do begin
      dataPtr.CreateOffset(i*s, locData);
      StringifySymbol(exec, writer, elemSym, locData);
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
   bufData : TData;
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
         SetLength(bufData, sym.Typ.Size);
         Assert(False, 'Unsupported yet');
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
