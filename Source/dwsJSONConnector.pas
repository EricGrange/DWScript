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

   // TdwsJSONConnectorType
   //
   TdwsJSONConnectorType = class (TInterfacedSelfObject, IConnectorType,
                                  IJSONTypeName, IJSONElementName,
                                  IJSONLow, IJSONHigh, IJSONLength,
                                  IJSONClone, IJSONExtend)
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
         function CloneCall(const base : Variant; const args : TConnectorArgs) : TData;
         function ExtendCall(const base : Variant; const args : TConnectorArgs) : TData;

         function IJSONTypeName.Call = TypeNameCall;
         function IJSONElementName.Call = ElementNameCall;
         function IJSONLow.Call = LowCall;
         function IJSONHigh.Call = HighCall;
         function IJSONLength.Call = LengthCall;
         function IJSONClone.Call = CloneCall;
         function IJSONExtend.Call = ExtendCall;

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
   TJSONStringifyMethod = class (TInternalMagicStringFunction)
      procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;

      class procedure StringifyVariant(writer : TdwsJSONWriter; const v : Variant); static;
      class procedure StringifySymbol(writer : TdwsJSONWriter; sym : TSymbol; const data : TData; offset : Integer); static;
      class procedure StringifyDynamicArray(writer : TdwsJSONWriter; dynArray : TScriptDynamicArray); static;
      class procedure StringifyArray(writer : TdwsJSONWriter; elemSym : TSymbol;
                                     const data : TData; offset, nb : Integer); static;
      class procedure StringifyComposite(writer : TdwsJSONWriter; compSym : TCompositeTypeSymbol; const data : TData; offset : Integer); static;
      class procedure StringifyClass(writer : TdwsJSONWriter; clsSym : TClassSymbol; const data : TData; offset : Integer); static;
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
      ['{585B989C-220C-4120-B5F4-2819A0708A80}']
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
      function ToString : String; override;

      class procedure Allocate(root, wrapped : TdwsJSONValue; var v : Variant); static;
      class procedure AllocateOrGetImmediate(root, wrapped : TdwsJSONValue; var v : Variant); static;
   end;

   TBoxedNilJSONValue = class (TInterfacedSelfObject, IBoxedJSONValue)
      function Root : TdwsJSONValue;
      function Value : TdwsJSONValue;
      function ToString : String; override;
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

// ToString
//
function TBoxedJSONValue.ToString : String;
begin
   Result:=FValue.ToString;
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

// AllocateOrGetImmediate
//
class procedure TBoxedJSONValue.AllocateOrGetImmediate(root, wrapped : TdwsJSONValue; var v : Variant);
begin
   if wrapped.IsImmediateValue then
      v:=TdwsJSONImmediate(wrapped).RawValue
   else if wrapped<>nil then
      TBoxedJSONValue.Allocate(root, wrapped, v)
   else v:=vNilJSONValue;
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
   connSym:=TJSONConnectorSymbol.Create(SYS_JSON_VARIANT, TdwsJSONConnectorType.Create(table));
   table.AddSymbol(connSym);

   jsonObject:=TClassSymbol.Create(SYS_JSON, nil);
   jsonObject.InheritFrom(table.TypObject);
   table.AddSymbol(jsonObject);
   jsonObject.IsStatic:=True;
   jsonObject.IsSealed:=True;
   jsonObject.SetNoVirtualMembers;

   anyType:=TAnyTypeSymbol.Create('Any Type', nil);
   table.AddSymbol(anyType);

//   TJSONStringifyMethod.Create(mkClassFunction, [maStatic], SYS_JSON_STRINGIFY,
//                               ['obj', 'Any Type'], SYS_STRING,
//                               jsonObject, cvPublic, table);
   TJSONStringifyMethod.Create(table, SYS_JSON_STRINGIFY,
                               ['val', 'Any Type'], SYS_STRING,
                               [iffStateLess, iffStaticMethod],
                               jsonObject);

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

   end else if UnicodeSameText(methodName, 'extend') then begin

      if Length(params)<>1 then
         raise ECompileException.CreateFmt(CPE_BadNumberOfParameters, [1, Length(params)]);
      if params[0].TypSym.UnAliasedType<>FTable.FindTypeSymbol(SYS_JSON_VARIANT, cvMagic) then
         raise ECompileException.CreateFmt(CPE_BadParameterType, [0, SYS_JSON_VARIANT, params[0].TypSym.Caption]);

      Result:=IJSONExtend(Self);
      typSym:=nil;

   end else begin

      if Length(params)<>0 then
         raise ECompileException.Create(CPE_NoParamsExpected);

      if UnicodeSameText(methodName, 'clone') then begin

         typSym:=FTable.FindTypeSymbol(SYS_JSON_VARIANT, cvMagic);
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
function TdwsJSONConnectorType.HasMember(const memberName : String; var typSym : TTypeSymbol;
                                         isWrite : Boolean) : IConnectorMember;
begin
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
      v:=IBoxedJSONValue(IUnknown(p^.VUnknown)).Value.Clone;
      Result[0]:=IUnknown(IBoxedJSONValue(TBoxedJSONValue.Create(v, v)));
      v.DecRefCount;
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
      pParam:=PVarData(@args[0]);
      if (pParam^.VType=varUnknown) and (pParam.VUnknown<>nil) then
         IBoxedJSONValue(IUnknown(pBase^.VUnknown)).Value.Extend(IBoxedJSONValue(IUnknown(pParam^.VUnknown)).Value);
   end;
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
   v : TdwsJSONValue;
begin
   SetLength(Result, 1);
   p:=PVarData(@base);
   if p^.VType=varUnknown then begin
      v:=IBoxedJSONValue(IUnknown(p^.VUnknown)).Value;
      if FMethodName<>'' then
         v:=v.Items[FMethodName];
      v:=v.Values[args[0][0]];
      TBoxedJSONValue.AllocateOrGetImmediate(IBoxedJSONValue(IUnknown(p^.VUnknown)).Root, v, Result[0])
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
   v : TdwsJSONValue;
begin
   SetLength(Result, 1);
   p:=PVarData(@base);
   if p^.VType=varUnknown then begin
      v:=IBoxedJSONValue(IUnknown(p^.VUnknown)).Value.Items[FMemberName];
      TBoxedJSONValue.AllocateOrGetImmediate(IBoxedJSONValue(IUnknown(p^.VUnknown)).Root, v, Result[0])
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
      if p^.VType=varUnknown then
         dataValue:=IBoxedJSONValue(IUnknown(p^.VUnknown)).Value
      else dataValue:=TdwsJSONImmediate.FromVariant(Variant(p^));
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
   if v=nil then
      box:=TBoxedJSONValue.Create(TdwsJSONObject.Create, nil)
   else begin
      box:=TBoxedJSONValue.Create(v, v);
      v.DecRefCount;
   end;
   Info.ResultAsVariant:=IUnknown(IBoxedJSONValue(box));
end;

// ------------------
// ------------------ TJSONStringifyMethod ------------------
// ------------------

// DoEvalAsString
//
procedure TJSONStringifyMethod.DoEvalAsString(args : TExprBaseList; var Result : String);
var
   writer : TdwsJSONWriter;
   stream : TWriteOnlyBlockStream;
   expr : TTypedExpr;
   dataExpr : TDataExpr;
   v : Variant;
begin
   stream:=TWriteOnlyBlockStream.Create;
   writer:=TdwsJSONWriter.Create(stream);
   try
      expr:=(args.ExprBase[0] as TTypedExpr);
      if expr.Typ.Size=1 then begin
         expr.EvalAsVariant(args.Exec, v);
         StringifyVariant(writer, v);
      end else begin
         dataExpr:=(expr as TDataExpr);
         StringifySymbol(writer, expr.Typ, dataExpr.Data[args.Exec], dataExpr.Addr[args.Exec]);
      end;
      Result:=stream.ToString;
   finally
      writer.Free;
      stream.Free;
   end;
end;

// StringifyVariant
//
class procedure TJSONStringifyMethod.StringifyVariant(writer : TdwsJSONWriter; const v : Variant);
var
   unk : IUnknown;
   getSelf : IGetSelf;
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
               if scriptObj.InternalObject.ClassType=TScriptDynamicArray then
                  StringifyDynamicArray(writer, TScriptDynamicArray(scriptObj.InternalObject))
               else writer.WriteString(scriptObj.ClassSym.Name);
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
class procedure TJSONStringifyMethod.StringifySymbol(writer : TdwsJSONWriter; sym : TSymbol; const data : TData; offset : Integer);
var
   ct : TClass;
begin
   sym:=sym.BaseType;
   ct:=sym.ClassType;
   if ct.InheritsFrom(TBaseSymbol) then
      StringifyVariant(writer, data[offset])
   else if ct=TDynamicArraySymbol then
      StringifyDynamicArray(writer, IScriptObj(IUnknown(data[offset])).InternalObject as TScriptDynamicArray)
   else if ct.InheritsFrom(TStaticArraySymbol) then
      StringifyArray(writer, TStaticArraySymbol(sym).Typ, data, offset, TStaticArraySymbol(sym).ElementCount)
   else if ct=TRecordSymbol then
      StringifyComposite(writer, TRecordSymbol(sym), data, offset)
   else if ct=TClassSymbol then
      StringifyClass(writer, TClassSymbol(sym), data, offset)
   else writer.WriteString(sym.ClassName);
end;

// StringifyArray
//
class procedure TJSONStringifyMethod.StringifyArray(
   writer : TdwsJSONWriter; elemSym : TSymbol; const data : TData; offset, nb : Integer);
var
   i, s : Integer;
begin
   s:=elemSym.Size;
   writer.BeginArray;
   for i:=0 to nb-1 do
      StringifySymbol(writer, elemSym, data, offset+i*s);
   writer.EndArray;
end;

// StringifyDynamicArray
//
class procedure TJSONStringifyMethod.StringifyDynamicArray(writer : TdwsJSONWriter; dynArray : TScriptDynamicArray);
begin
   StringifyArray(writer, dynArray.ElementTyp, dynArray.Data, 0, dynArray.Length);
end;

// StringifyComposite
//
class procedure TJSONStringifyMethod.StringifyComposite(writer : TdwsJSONWriter; compSym : TCompositeTypeSymbol; const data : TData; offset : Integer);
var
   i : Integer;
   bufData : TData;
   sym : TSymbol;
   fieldSym : TFieldSymbol;
   propSym : TPropertySymbol;
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
         StringifySymbol(writer, fieldSym.Typ, data, offset+fieldSym.Offset);
      end else begin
         SetLength(bufData, sym.Typ.Size);
         Assert(False, 'Unsupported yet');
      end;
   end;
   writer.EndObject;
end;

// StringifyClass
//
class procedure TJSONStringifyMethod.StringifyClass(writer : TdwsJSONWriter; clsSym : TClassSymbol; const data : TData; offset : Integer);
var
   obj : IScriptObj;
begin
   obj:=IScriptObj(IUnknown(data[offset]));
   if (obj=nil) or (obj.Destroyed) then
      writer.WriteNull
   else StringifyComposite(writer, clsSym, obj.Data, 0);
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
