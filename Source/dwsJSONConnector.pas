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

uses Classes, SysUtils, dwsLanguageExtension, dwsComp, dwsCompiler,
   dwsExprs, dwsTokenizer, dwsSymbols, dwsErrors, dwsCoreExprs, dwsStack,
   dwsStrings, dwsXPlatform, dwsUtils, dwsOperators, dwsUnitSymbols,
   dwsFunctions, dwsJSON;

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

   // TdwsJSONConnectorType
   //
   TdwsJSONConnectorType = class (TInterfacedSelfObject, IConnectorType,
                                  IJSONTypeName, IJSONElementName,
                                  IJSONLow, IJSONHigh, IJSONLength)
      private
         FTable : TSymbolTable;
         FLowValue : TData;

      protected
         function ConnectorCaption : String;
         function AcceptsParams(const params : TConnectorParamArray) : Boolean;
         function NeedDirectReference : Boolean;

         function HasMethod(const methodName : String; const params : TConnectorParamArray;
                            var typSym : TTypeSymbol) : IConnectorCall;
         function HasMember(const memberName : String; var typSym : TTypeSymbol;
                            isWrite : Boolean) : IConnectorMember;
         function HasIndex(const propName : String; const params : TConnectorParamArray;
                           var typSym : TTypeSymbol; isWrite : Boolean) : IConnectorCall;

         function TypeNameCall(const base : Variant; const args : TConnectorArgs) : TData;
         function ElementNameCall(const base : Variant; const args : TConnectorArgs) : TData;
         function LowCall(const base : Variant; const args : TConnectorArgs) : TData;
         function HighCall(const base : Variant; const args : TConnectorArgs) : TData;
         function LengthCall(const base : Variant; const args : TConnectorArgs) : TData;

         function IJSONTypeName.Call = TypeNameCall;
         function IJSONElementName.Call = ElementNameCall;
         function IJSONLow.Call = LowCall;
         function IJSONHigh.Call = HighCall;
         function IJSONLength.Call = LengthCall;

      public
         constructor Create(table : TSymbolTable);
   end;

   // TdwsJSONIndexCall
   //
   TdwsJSONIndexCall = class(TInterfacedSelfObject, IUnknown, IConnectorCall)
      private
         FMethodName : String;

      protected
         function Call(const base : Variant; const args : TConnectorArgs) : TData; virtual; abstract;
         function NeedDirectReference : Boolean;

      public
         constructor Create(const methodName : String);

         property CallMethodName : String read FMethodName write FMethodName;
   end;

   // TdwsJSONIndexReadCall
   //
   TdwsJSONIndexReadCall = class(TdwsJSONIndexCall)
      protected
         function Call(const base : Variant; const args : TConnectorArgs) : TData; override;
   end;

   // TdwsJSONConnectorMember
   //
   TdwsJSONConnectorMember = class(TInterfacedSelfObject, IUnknown, IConnectorMember)
      private
         FMemberName : String;

      protected
         function Read(const base : Variant) : TData;
         procedure Write(const base : Variant; const data : TData);

      public
         constructor Create(const memberName : String);

         property MemberName : String read FMemberName write FMemberName;
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

   // TJSONStringifyMethod
   //
   TJSONStringifyMethod = class(TInternalStaticMethod)
      procedure Execute(info : TProgramInfo); override;
   end;

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
   SYS_JSON_VARIANT = 'JSONVariant';
   SYS_JSON_STRINGIFY = 'Stringify';
   SYS_JSON_PARSE = 'Parse';

type
   IBoxedJSONValue = interface
      function Root : TdwsJSONValue;
      function Value : TdwsJSONValue;
   end;

   TBoxedJSONValue = class (TInterfacedSelfObject, IBoxedJSONValue)
      FRoot : TdwsJSONValue;
      FValue : TdwsJSONValue;

      constructor Create(root, wrapped : TdwsJSONValue);
      destructor Destroy; override;

      function Root : TdwsJSONValue;
      function Value : TdwsJSONValue;

      class procedure Allocate(root, wrapped : TdwsJSONValue; var v : Variant); static;
   end;

   TBoxedNilJSONValue = class (TInterfacedSelfObject, IBoxedJSONValue)
      function Root : TdwsJSONValue;
      function Value : TdwsJSONValue;
   end;

var
   vNilJSONValue : IBoxedJSONValue;

// Create
//
constructor TBoxedJSONValue.Create(root, wrapped : TdwsJSONValue);
begin
   root.IncRefCount;
   FRoot:=root;
   FValue:=wrapped;
end;

// Destroy
//
destructor TBoxedJSONValue.Destroy;
begin
   FRoot.DecRefCount;
end;

// Root
//
function TBoxedJSONValue.Root : TdwsJSONValue;
begin
   Result:=FRoot;
end;

// Value
//
function TBoxedJSONValue.Value : TdwsJSONValue;
begin
   Result:=FValue;
end;

// Allocate
//
class procedure TBoxedJSONValue.Allocate(root, wrapped : TdwsJSONValue; var v : Variant);
var
   b : TBoxedJSONValue;
begin
   b:=TBoxedJSONValue.Create(root, wrapped);
   v:=IUnknown(IBoxedJSONValue(b));
end;

// Root
//
function TBoxedNilJSONValue.Root : TdwsJSONValue;
begin
   Result:=nil;
end;

// Value
//
function TBoxedNilJSONValue.Value : TdwsJSONValue;
begin
   Result:=nil;
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
begin
   connSym:=TJSONConnectorSymbol.Create(SYS_JSON_VARIANT, TdwsJSONConnectorType.Create(table));
   table.AddSymbol(connSym);

   jsonObject:=TClassSymbol.Create(SYS_JSON, nil);
   jsonObject.InheritFrom(table.TypObject);
   table.AddSymbol(jsonObject);
   jsonObject.IsStatic:=True;
   jsonObject.IsSealed:=True;
   jsonObject.SetNoVirtualMembers;

   TJSONStringifyMethod.Create(mkClassFunction, [maStatic], SYS_JSON_STRINGIFY,
                               ['obj', SYS_JSON_VARIANT], SYS_STRING,
                               jsonObject, cvPublic, table);
   TJSONParseMethod.Create(mkClassFunction, [maStatic], SYS_JSON_PARSE,
                           ['str', SYS_STRING], SYS_JSON_VARIANT,
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
function TdwsJSONConnectorType.ConnectorCaption : String;
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
function TdwsJSONConnectorType.HasMethod(const methodName : String; const params : TConnectorParamArray;
                                       var typSym : TTypeSymbol) : IConnectorCall;
begin
   if UnicodeSameText(methodName, 'typename') then begin

      Result:=IJSONTypeName(Self);
      typSym:=FTable.FindTypeSymbol(SYS_STRING, cvMagic);

      if Length(params)<>0 then
         raise ECompileException.Create(CPE_NoParamsExpected);

   end else if UnicodeSameText(methodName, 'elementname') then begin

      if Length(params)<>1 then
         raise ECompileException.CreateFmt(CPE_BadNumberOfParameters, [1, Length(params)]);
      if not (params[0].TypSym.UnAliasedType is TBaseIntegerSymbol) then
         raise ECompileException.CreateFmt(CPE_BadParameterType, [0, SYS_INTEGER, params[0].TypSym.Caption]);

      Result:=IJSONElementName(Self);
      typSym:=FTable.FindTypeSymbol(SYS_STRING, cvMagic);

   end else begin

      if Length(params)<>0 then
         raise ECompileException.Create(CPE_NoParamsExpected);

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

// HasMember
//
function TdwsJSONConnectorType.HasMember(const memberName : String; var typSym : TTypeSymbol;
                                       isWrite : Boolean) : IConnectorMember;
begin
   if isWrite then Exit(nil); // unsupported yet

   typSym:=FTable.FindTypeSymbol(SYS_JSON_VARIANT, cvMagic);
   Result:=TdwsJSONConnectorMember.Create(memberName);
end;

// HasIndex
//
function TdwsJSONConnectorType.HasIndex(const propName : String; const params : TConnectorParamArray;
                                      var typSym : TTypeSymbol; isWrite : Boolean) : IConnectorCall;
begin
   if isWrite then Exit(nil); // unsupported yet

   typSym:=FTable.FindTypeSymbol(SYS_JSON_VARIANT, cvMagic);
   Result:=TdwsJSONIndexReadCall.Create(propName);
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

// ------------------
// ------------------ TdwsJSONIndexCall ------------------
// ------------------

// Create
//
constructor TdwsJSONIndexCall.Create(const methodName : String);
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
   v, r : TdwsJSONValue;
begin
   SetLength(Result, 1);
   p:=PVarData(@base);
   if p^.VType=varUnknown then begin
      v:=IBoxedJSONValue(IUnknown(p^.VUnknown)).Value;
      if FMethodName<>'' then
         v:=v.Items[FMethodName];
      v:=v.Values[args[0][0]];
      if v.IsImmediateValue then
         Result[0]:=TdwsJSONImmediate(v).RawValue
      else if v<>nil then begin
         r:=IBoxedJSONValue(IUnknown(p^.VUnknown)).Root;
         TBoxedJSONValue.Allocate(r, v, Result[0])
      end else Result[0]:=vNilJSONValue;
   end else begin
      Result[0]:=vNilJSONValue;
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
   FMemberName:=memberName;
end;

// Read
//
function TdwsJSONConnectorMember.Read(const base : Variant) : TData;
var
   p : PVarData;
   r, v : TdwsJSONValue;
begin
   SetLength(Result, 1);
   p:=PVarData(@base);
   if p^.VType=varUnknown then begin
      v:=IBoxedJSONValue(IUnknown(p^.VUnknown)).Value.Items[FMemberName];
      if v.IsImmediateValue then
         Result[0]:=v.Value.RawValue
      else if v<>nil then begin
         r:=IBoxedJSONValue(IUnknown(p^.VUnknown)).Root;
         TBoxedJSONValue.Allocate(r, v, Result[0]);
      end else Result[0]:=vNilJSONValue;
   end else Result[0]:=vNilJSONValue;
end;

// Write
//
procedure TdwsJSONConnectorMember.Write(const base : Variant; const data : TData);
begin
   Assert('Not supported yet');
end;

// ------------------
// ------------------ TJSONConnectorSymbol ------------------
// ------------------

// IsCompatible
//
function TJSONConnectorSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   Result:=   inherited IsCompatible(typSym)
           or (typSym is TFuncSymbol)
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
   box:=TBoxedJSONValue.Create(v, v);
   v.DecRefCount;
   Info.ResultAsVariant:=IUnknown(IBoxedJSONValue(box));
end;

// ------------------
// ------------------ TJSONStringifyMethod ------------------
// ------------------

// Execute
//
procedure TJSONStringifyMethod.Execute(info : TProgramInfo);
begin
   Info.ResultAsString:='test';
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
