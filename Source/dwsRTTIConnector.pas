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
unit dwsRTTIConnector;

{$I dws.inc}

interface

uses
   Windows, Forms, Variants, Classes, SysUtils, SysConst, TypInfo, RTTI,
   dwsComp, dwsSymbols, dwsDataContext, dwsErrors, dwsUnitSymbols,
   dwsExprs, dwsStrings, dwsFunctions, dwsStack, dwsOperators, dwsLegacy,
   dwsUtils, dwsLanguageExtension, dwsCompiler, dwsConnectorSymbols,
   dwsCompilerContext;

const
   RTTI_ConnectorCaption = 'RTTI Connector 1.0';
   RTTI_UnitName = 'RttiConnector';
   SYS_RTTIVARIANT = 'RttiVariant';

type
   TdwsRTTIConnector = class(TdwsAbstractStaticUnit, IUnknown, IConnector)
      private
         function ConnectorCaption : String;
         function ConnectorName : String;
         function GetUnit(const UnitName : String) : IConnectorType;

      protected
         procedure AddUnitSymbols(systemTable : TSystemSymbolTable; table : TSymbolTable; operators : TOperators); override;

      public
         constructor Create(AOwner: TComponent); override;

      published
         property StaticSymbols;
   end;

   TdwsRTTIVariant = class(TInterfacedSelfObject)
      private
         FValue : TValue;
         FType : TRttiType;

      public
         class function FromObject(obj : TObject) : IUnknown;
         class function FromValue(const v : TValue) : IUnknown;

         property RTTIType : TRttiType read FType;
         function TypeKind : TTypeKind; inline;

         function AsObject : TObject; inline;
         property AsValue : TValue read FValue;

         function InstancePointer : Pointer;
   end;

   TRTTIConnectorSymbol = class (TConnectorSymbol)
      public
         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         function SpecializeConnector(table : TSymbolTable; const qualifier : String) : TConnectorSymbol; override;
   end;

   TRTTIEnvironmentOption = (eoAllowFieldWrite);
   TRTTIEnvironmentOptions = set of TRTTIEnvironmentOption;

   // can be attached to an execution by way of UserObject
   TRTTIEnvironment = class(TdwsLanguageExtension)
      private
         FDefaultEnvironment : TValue;
         FRttiType : TRttiType;
         FOptions : TRTTIEnvironmentOptions;

      protected
         procedure SetDefaultEnvironment(const val : TValue);
         procedure SetRttiType(const val : TRttiType);

      public
         function FindUnknownName(compiler : TdwsCompiler; const name : String) : TSymbol; override;
         procedure GetDefaultEnvironment(var enviro : IdwsEnvironment); override;

         procedure SetForClass(cls : TClass);

         property DefaultEnvironment : TValue read FDefaultEnvironment write SetDefaultEnvironment;
         property RttiType : TRttiType read FRttiType write SetRttiType;

         property Options : TRTTIEnvironmentOptions read FOptions write FOptions;
   end;

   TRTTIRuntimeEnvironment = class (TInterfacedSelfObject, IdwsEnvironment)
      private
         FValue : TValue;

      public
         constructor Create(const value : TValue);

         class function Instance(exec : TdwsProgramExecution) : Pointer; static;

         property Value : TValue read FValue write FValue;
   end;

   TRTTIEnvironmentCallable = class(TInterfacedSelfObject, IExecutable, ICallable)
      private
         FEnvironment : TRTTIEnvironment;

      public
         constructor Create(environment : TRTTIEnvironment);

         procedure Call(exec : TdwsProgramExecution; func : TFuncSymbol); virtual; abstract;
         procedure CompileTimeCheck(context : TdwsCompilerContext; expr : TFuncExprBase);
         procedure InitSymbol(symbol : TSymbol; const msgs : TdwsCompileMessageList);
         procedure InitExpression(expr : TExprBase);
         function SubExpr(i : Integer) : TExprBase;
         function SubExprCount : Integer;
   end;

   TRTTIEnvironmentField = class(TRTTIEnvironmentCallable)
      private
         FField : TRttiField;

      public
         constructor Create(environment : TRTTIEnvironment; rttiField : TRttiField);
   end;

   TRTTIEnvironmentFieldRead = class(TRTTIEnvironmentField)
      public
         procedure Call(exec : TdwsProgramExecution; func : TFuncSymbol); override;
   end;

   TRTTIEnvironmentFieldWrite = class(TRTTIEnvironmentField)
      public
         procedure Call(exec : TdwsProgramExecution; func : TFuncSymbol); override;
   end;

   TRTTIEnvironmentProp = class(TRTTIEnvironmentCallable)
      private
         FProp : TRttiProperty;

      public
         constructor Create(environment : TRTTIEnvironment; rttiProp : TRttiProperty);
   end;

   TRTTIEnvironmentPropRead = class(TRTTIEnvironmentProp)
      public
         procedure Call(exec : TdwsProgramExecution; func : TFuncSymbol); override;
   end;

   TRTTIEnvironmentPropWrite = class(TRTTIEnvironmentProp)
      public
         procedure Call(exec : TdwsProgramExecution; func : TFuncSymbol); override;
   end;

   EdwsRTTIException = class(Exception) end;

   TRTTIIndexedPropertyGetter = function(instance: Pointer; const args: array of TValue): TValue;
   TRTTIIndexedPropertySetter = procedure(instance: Pointer; const args: array of TValue);
   TRTTIIndexedProperty = record
      ClassType : TClass;
      PropName : String;
      IsDefault : Boolean;
      PropType : TVarType;
      GetValue : TRTTIIndexedPropertyGetter;
      SetValue : TRTTIIndexedPropertySetter;
   end;
   PRTTIIndexedProperty = ^TRTTIIndexedProperty;

procedure RegisterRTTIIndexedProperty(aClassType : TClass; const aPropName : String; isDefault : Boolean;
                                      const aPropType : TVarType;
                                      const getValue : TRTTIIndexedPropertyGetter; const setValue : TRTTIIndexedPropertySetter);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
   vRTTIContext : TRttiContext;
   vEmptyTValue : TValue;
   vIndexedProperties : array of TRTTIIndexedProperty;

type

   TConnectFormFunc = class(TInternalFunctionWithExecute)
      procedure Execute(info : TProgramInfo); override;
   end;

   TCreateComponentFunc = class(TInternalFunctionWithExecute)
      procedure Execute(info : TProgramInfo); override;
   end;

   TdwsRTTIConnectorType = class(TInterfacedObject, IUnknown, IConnectorType)
      private
         FTable : TSymbolTable;
         FRttiType : TRttiType;

      protected
         function ConnectorCaption : String;
         function AcceptsParams(const params : TConnectorParamArray) : Boolean;
         function AutoVarParams : Boolean;
         function WritableReads(const memberName : UnicodeString) : Boolean;

         function HasMethod(const methodName : String; const params : TConnectorParamArray;
                            var typSym : TTypeSymbol) : IConnectorCall;
         function HasMember(const memberName : String; var typSym : TTypeSymbol;
                            isWrite : Boolean) : IConnectorMember;
         function HasIndex(const propName : String; const params : TConnectorParamArray;
                           var typSym : TTypeSymbol; isWrite : Boolean) : IConnectorCall;
         function HasEnumerator(var typSym: TTypeSymbol) : IConnectorEnumerator;
         function HasCast(typSym: TTypeSymbol) : IConnectorCast;
      public
         constructor Create(table : TSymbolTable; rttiType : TRttiType);
   end;

   TdwsRTTIMethodType = (mtMethod, mtPropertyGet, mtPropertySet);

   TdwsRTTIConnectorCall = class(TInterfacedSelfObject, IUnknown, IConnectorCall, IConnectorArgsCall)
      private
         FMethodName : String;
         FMethodType : TdwsRTTIMethodType;

      protected
         function Call(const base : Variant; const args : TConnectorArgs) : TData;
         function NeedDirectReference : Boolean;

      public
         constructor Create(const methodName : String; const params : TConnectorParamArray;
                            methodType : TdwsRTTIMethodType);
   end;

   TdwsRTTIConnectorMember = class(TInterfacedSelfObject, IUnknown, IConnectorMember, IConnectorDataMember)
      protected
         FMemberName : String;

         function Read(const base : Variant) : TData;
         procedure Write(const base : Variant; const data : TData);

      public
         constructor Create(const memberName : String);
   end;

   TdwsRTTIConnectorIndexedProperty = class(TInterfacedSelfObject, IUnknown, IConnectorCall, IConnectorArgsCall)
      private
         FPropertyName : String;
         FMethodType : TdwsRTTIMethodType;

      protected
         function Call(const base : Variant; const args : TConnectorArgs) : TData;
         function NeedDirectReference : Boolean;

      public
         constructor Create(const propertyName : String; const params : TConnectorParamArray;
                            methodType : TdwsRTTIMethodType);
   end;

// ValueToVariant
//
procedure ValueToVariant(const v : TValue; var result : Variant);
begin
   case v.Kind of
      tkInteger, tkInt64 :
         result:=v.AsInt64;
      tkEnumeration :
         if v.TypeInfo=TypeInfo(Boolean) then
            result:=v.AsBoolean
         else result:=v.AsInt64;
      tkChar, tkString, tkUString, tkWChar, tkWString, tkLString :
         result:=v.AsString;
      tkFloat :
         result:=v.AsType<Double>;
      tkVariant :
         result:=v.AsVariant;
      tkClass, tkRecord :
         result:=TdwsRTTIVariant.FromValue(v);
   else
      result:=Null;
   end;
end;

// VariantToValue
//
procedure VariantToValue(const v : Variant; var result : TValue);
begin
   if (VarType(v)=varUnknown) and (IUnknown(v) is TdwsRTTIVariant) then
      Result:=(IUnknown(v) as TdwsRTTIVariant).AsValue
   else Result:=TValue.FromVariant(v)
end;

// RTTITypeToTypeSymbol
//
function RTTITypeToTypeSymbol(rttiType : TRttiType; table : TSymbolTable; default : TRTTIConnectorSymbol) : TTypeSymbol;
begin
   case rttiType.TypeKind of
      tkInteger, tkInt64 :
         Result:=table.FindTypeSymbol(SYS_INTEGER, cvMagic);
      tkEnumeration :
         if rttiType.Handle=TypeInfo(Boolean) then
            Result:=table.FindTypeSymbol(SYS_BOOLEAN, cvMagic)
         else Result:=table.FindTypeSymbol(SYS_INTEGER, cvMagic);
      tkChar, tkString, tkUString, tkWChar, tkWString, tkLString :
         Result:=table.FindTypeSymbol(SYS_STRING, cvMagic);
      tkFloat :
         Result:=table.FindTypeSymbol(SYS_FLOAT, cvMagic);
      tkRecord, tkClass, tkInterface :
         Result:=default.SpecializeConnector(table, rttiType.QualifiedName);
   else
      Result:=default;
   end;
end;

procedure RegisterRTTIIndexedProperty(
   aClassType : TClass; const aPropName : String; isDefault : Boolean;
   const aPropType : TVarType;
   const getValue : TRTTIIndexedPropertyGetter; const setValue : TRTTIIndexedPropertySetter);
var
   n : Integer;
   entry : PRTTIIndexedProperty;
begin
   n:=Length(vIndexedProperties);
   SetLength(vIndexedProperties, n+1);
   entry:=@vIndexedProperties[n];
   entry.ClassType:=aClassType;
   entry.PropName:=aPropName;
   entry.IsDefault:=isDefault;
   entry.PropType:=aPropType;
   entry.GetValue:=getValue;
   entry.SetValue:=setValue;
end;

function TStrings_GetStrings(instance: Pointer; const args: array of TValue): TValue;
begin
   Result:=TStrings(instance).Strings[args[0].AsInteger];
end;

procedure TStrings_SetStrings(instance: Pointer; const args: array of TValue);
begin
   TStrings(instance).Strings[args[0].AsInteger]:=args[1].AsString;
end;

function TComponent_GetComponents(instance: Pointer; const args: array of TValue): TValue;
begin
   Result:=TComponent(instance).Components[args[0].AsInteger];
end;

// ------------------
// ------------------ TdwsRTTIConnector ------------------
// ------------------

// Create
//
constructor TdwsRTTIConnector.Create(AOwner: TComponent);
begin
   inherited;
   UnitName := RTTI_UnitName;
end;

// ConnectorCaption
//
function TdwsRTTIConnector.ConnectorCaption : String;
begin
   Result:=RTTI_ConnectorCaption;
end;

// ConnectorName
//
function TdwsRTTIConnector.ConnectorName : String;
begin
   Result:=RTTI_UnitName;
end;

// GetUnit
//
function TdwsRTTIConnector.GetUnit(const unitName : String) : IConnectorType;
begin
   raise Exception.Create('Not supported');
end;

// AddUnitSymbols
//
procedure TdwsRTTIConnector.AddUnitSymbols(systemTable : TSystemSymbolTable; table : TSymbolTable; operators : TOperators);
var
   rttiVariantSym: TTypeSymbol;
begin
   rttiVariantSym:=TRTTIConnectorSymbol.Create(SYS_RTTIVARIANT, TdwsRTTIConnectorType.Create(table, nil));
   table.AddSymbol(rttiVariantSym);

   TConnectFormFunc.Create(table, 'ConnectForm', ['name', SYS_STRING], SYS_RTTIVARIANT);
   TCreateComponentFunc.Create(table, 'CreateComponent', ['parent', SYS_RTTIVARIANT, 'className', SYS_STRING], SYS_RTTIVARIANT);
end;

// ------------------
// ------------------ TConnectFormFunc ------------------
// ------------------

procedure TConnectFormFunc.Execute(info : TProgramInfo);
var
   c : TComponent;
begin
   c:=Application.FindComponent(Info.ParamAsString[0]);
   if not (c is TForm) then
      Info.ResultAsVariant:=Null
   else begin
      Info.ResultAsVariant:=TdwsRTTIVariant.FromObject(c);
   end;
end;

// ------------------
// ------------------ TCreateComponentFunc ------------------
// ------------------

procedure TCreateComponentFunc.Execute(info : TProgramInfo);
var
   parent : TdwsRTTIVariant;
   obj : TObject;
   clsName : String;
   comp : TComponent;
   compClass : TClass;
   compType : TRttiType;
begin
   parent:=IUnknown(Info.ParamAsVariant[0]) as TdwsRTTIVariant;
   if parent.TypeKind<>tkClass then
      raise EdwsRTTIException.Create('Class expected as parent');
   obj:=parent.AsObject;
   if not (obj is TComponent) then
      raise EdwsRTTIException.Create('TComponent instance expected as parent');

   clsName:=Info.ParamAsString[1];
   compClass:=nil;
   if StrContains(clsName, '.') then begin
      compType:=vRTTIContext.FindType(clsName);
      if compType is TRttiInstanceType then
         compClass:=TRttiInstanceType(compType).MetaclassType;
   end;
   if compClass=nil then
      compClass:=FindClass(clsName);
   if not compClass.InheritsFrom(TComponent) then
      raise EdwsRTTIException.CreateFmt('"%s" does not inherit from TComponent', [clsName]);

   comp:=TComponentClass(compClass).Create(TComponent(obj));
   Info.ResultAsVariant:=TdwsRTTIVariant.FromObject(comp);
end;

// ------------------
// ------------------ TdwsRTTIConnectorType ------------------
// ------------------

// Create
//
constructor TdwsRTTIConnectorType.Create(table : TSymbolTable; rttiType : TRttiType);
begin
   FTable:=Table;
   FRttiType:=rttiType;
end;

// ConnectorCaption
//
function TdwsRTTIConnectorType.ConnectorCaption: string;
begin
   Result:=SYS_RTTIVARIANT;
   if FRttiType<>nil then
      Result:=Result+' <'+FRttiType.QualifiedName+'>';
end;

// HasIndex
//
function TdwsRTTIConnectorType.HasIndex(const propName : String; const params : TConnectorParamArray;
                                        var typSym : TTypeSymbol; isWrite : Boolean) : IConnectorCall;
begin
   Result:=nil; // unsupported by Delphi XE RTTI
   typSym:=FTable.FindTypeSymbol(SYS_RTTIVARIANT, cvMagic);
   if isWrite then
      Result:=TdwsRTTIConnectorIndexedProperty.Create(propName, params, mtPropertySet)
   else Result:=TdwsRTTIConnectorIndexedProperty.Create(propName, params, mtPropertyGet)
end;

// HasEnumerator
//
function TdwsRTTIConnectorType.HasEnumerator(var typSym: TTypeSymbol) : IConnectorEnumerator;
begin
   Result:=nil;
end;

// HasCast
//
function TdwsRTTIConnectorType.HasCast(typSym: TTypeSymbol) : IConnectorCast;
begin
   Result:=nil;
end;

// HasMember
//
function TdwsRTTIConnectorType.HasMember(const memberName : String; var typSym : TTypeSymbol;
                                         isWrite : Boolean) : IConnectorMember;
var
   connector : TdwsRTTIConnectorMember;
   connSym : TRTTIConnectorSymbol;
   rttiProp : TRttiProperty;
   rttiField : TRttiField;
   rttiMeth : TRttiMethod;
   rttiType : TRttiType;
begin
   connSym:=TRTTIConnectorSymbol(FTable.FindSymbol(SYS_RTTIVARIANT, cvMagic, TRTTIConnectorSymbol));

   if FRttiType<>nil then begin
      rttiProp:=FRttiType.GetProperty(memberName);
      if rttiProp<>nil then
         rttiType:=rttiProp.PropertyType
      else begin
         rttiField:=FRttiType.GetField(memberName);
         if rttiField<>nil then
            rttiType:=rttiField.FieldType
         else begin
            rttiMeth:=FRttiType.GetMethod(memberName);
            if (rttiMeth<>nil) and (Length(rttiMeth.GetParameters)=0) then begin
               rttiType:=rttiMeth.ReturnType;
               if rttiType=nil then Exit;
            end else Exit;
         end
      end;
      typSym:=RTTITypeToTypeSymbol(rttiType, FTable, connSym)
   end else typSym:=connSym;

   connector:=TdwsRTTIConnectorMember.Create(memberName);
   Result:=connector;
end;

// HasMethod
//
function TdwsRTTIConnectorType.HasMethod(const methodName : String; const params : TConnectorParamArray;
                                         var typSym : TTypeSymbol) : IConnectorCall;
var
   connSym : TRTTIConnectorSymbol;
   rttiMethod : TRttiMethod;
   connector : TdwsRTTIConnectorCall;
begin
   connSym:=TRTTIConnectorSymbol(FTable.FindSymbol(SYS_RTTIVARIANT, cvMagic, TRTTIConnectorSymbol));
   typSym:=connSym;

   if FRttiType<>nil then begin
      rttiMethod:=FRttiType.GetMethod(methodName);
      if rttiMethod<>nil then begin
         if rttiMethod.ReturnType<>nil then
            typSym:=RTTITypeToTypeSymbol(rttiMethod.ReturnType, FTable, connSym)
         else typSym:=nil;
      end else Exit(nil);
   end;

   connector:=TdwsRTTIConnectorCall.Create(methodName, params, mtMethod);
   Result:=connector;
end;

// AcceptsParams
//
function TdwsRTTIConnectorType.AcceptsParams(const params : TConnectorParamArray) : Boolean;
var
   x : Integer;
   typ : TTypeSymbol;
begin
   for x:=0 to High(params) do begin
      typ:=params[x].TypSym;
      if not (typ is TBaseSymbol) then
         Exit(False);
   end;
   Result:=True;
end;

// AutoVarParams
//
function TdwsRTTIConnectorType.AutoVarParams : Boolean;
begin
   Result:=True;
end;

// WritableReads
//
function TdwsRTTIConnectorType.WritableReads(const memberName : UnicodeString) : Boolean;
begin
   Result := False;
end;

// ------------------
// ------------------ TdwsRTTIConnectorCall ------------------
// ------------------

// Create
//
constructor TdwsRTTIConnectorCall.Create(const methodName : String; const params : TConnectorParamArray;
                                         methodType : TdwsRTTIMethodType);
begin
   FMethodName:=methodName;
   FMethodType:=methodType;
end;

// Call
//
function TdwsRTTIConnectorCall.Call(const base : Variant; const args : TConnectorArgs) : TData;
var
   i : Integer;
   paramData : array of TValue;
   instance : TdwsRTTIVariant;
   meth, overloadedMeth : TRttiMethod;
   methParams : TArray<TRttiParameter>;
   methList : TArray<TRttiMethod>;
   resultValue : TValue;
begin
   instance:=IUnknown(base) as TdwsRTTIVariant;
   meth:=instance.RTTIType.GetMethod(FMethodName);
   if meth=nil then
      raise EdwsRTTIException.CreateFmt('"%s" does not have a method "%s" exposed via RTTI',
                                        [instance.RTTIType.Name, FMethodName]);

   methParams:=meth.GetParameters;
   if Length(methParams)<>Length(args) then begin
      // manually check for an overload
      // only match on parameter count for now
      methList:=instance.RTTIType.GetMethods;
      for overloadedMeth in methList do begin
         if UnicodeSameText(overloadedMeth.Name, FMethodName) then begin
            methParams:=overloadedMeth.GetParameters;
            if Length(methParams)=Length(args) then begin
               meth:=overloadedMeth;
               Break;
            end;
         end;
      end;
   end;

   if Length(methParams)<>Length(args) then
      raise EdwsRTTIException.CreateFmt('Method "%s.%s" expects %d params, got %d',
                                        [instance.RTTIType.Name, FMethodName,
                                         Length(methParams), Length(args)]);

   SetLength(paramData, Length(args));
   for i:=0 to High(args) do begin
      if Length(args[i])=1 then
         paramData[i]:=TValue.FromVariant(args[i][0])
      else paramData[i]:=TValue.FromVariant(args[i]);
   end;

   if meth.IsClassMethod then
      resultValue:=meth.Invoke(instance.AsObject.ClassType, paramData)
   else resultValue:=meth.Invoke(instance.AsObject, paramData);

   SetLength(Result, 1);

   ValueToVariant(resultValue, Result[0]);
end;

// NeedDirectReference
//
function TdwsRTTIConnectorCall.NeedDirectReference : Boolean;
begin
   Result:=False;
end;

// ------------------
// ------------------ TdwsRTTIConnectorMember ------------------
// ------------------

// Create
//
constructor TdwsRTTIConnectorMember.Create(const memberName : String);
begin
   FMemberName:=memberName;
end;

// Read
//
function TdwsRTTIConnectorMember.Read(const base : Variant) : TData;
var
   instance : TdwsRTTIVariant;
   field : TRttiField;
   prop : TRttiProperty;
   meth : TRttiMethod;
begin
   instance:=IUnknown(base) as TdwsRTTIVariant;
   SetLength(Result, 1);

   field:=instance.RTTIType.GetField(FMemberName);
   if field<>nil then
      ValueToVariant(field.GetValue(instance.InstancePointer), Result[0])
   else begin
      prop:=instance.RTTIType.GetProperty(FMemberName);
      if prop<>nil then begin
         if not prop.IsReadable then
            raise EdwsRTTIException.CreateFmt('"%s.%s" is not readable',
                                              [instance.RTTIType.Name, FMemberName]);
         ValueToVariant(prop.GetValue(instance.InstancePointer), Result[0])
      end else begin
         meth:=instance.RTTIType.GetMethod(FMemberName);
         if (meth<>nil) and (meth.ReturnType<>nil) and (Length(meth.GetParameters)=0) then begin
            if meth.IsClassMethod then
               ValueToVariant(meth.Invoke(TObject(instance.InstancePointer).ClassType, []), Result[0])
            else ValueToVariant(meth.Invoke(TObject(instance.InstancePointer), []), Result[0])
         end else raise EdwsRTTIException.CreateFmt('"%s" does not have member "%s" exposed via RTTI',
                                                    [instance.RTTIType.Name, FMemberName]);
      end;
   end;
end;

// Write
//
procedure TdwsRTTIConnectorMember.Write(const base : Variant; const data : TData);
var
   instance : TdwsRTTIVariant;
   field : TRttiField;
   prop : TRttiProperty;
   v : TValue;
   intf : IUnknown;
begin
   instance:=IUnknown(base) as TdwsRTTIVariant;

   if Length(data)<>1 then
      raise EdwsRTTIException.Create('Unsupported Rtti write of this data type');
   case VarType(data[0]) of
      varUnknown : begin
         intf:=IUnknown(data[0]);
         if intf is TdwsRTTIVariant then
            v:=TValue.From<TObject>((intf as TdwsRTTIVariant).AsObject)
         else raise EdwsRTTIException.Create('Unsupported Rtti write of this datatype');
      end;
   else
      v:=TValue.FromVariant(data[0]);
   end;

   field:=instance.RTTIType.GetField(FMemberName);
   if field<>nil then
      field.SetValue(instance.InstancePointer, v)
   else begin
      prop:=instance.RTTIType.GetProperty(FMemberName);
      if prop<>nil then begin
         if not prop.IsWritable then
            raise EdwsRTTIException.CreateFmt('"%s.%s" is not writable',
                                              [instance.RTTIType.Name, FMemberName]);
         prop.SetValue(instance.InstancePointer, v);
      end else begin
         raise EdwsRTTIException.CreateFmt('"%s" does not have member "%s" exposed via RTTI',
                                           [instance.RTTIType.Name, FMemberName]);
      end;
   end;
end;

// ------------------
// ------------------ TdwsRTTIVariant ------------------
// ------------------

// FromObject
//
class function TdwsRTTIVariant.FromObject(obj : TObject) : IUnknown;
begin
   Result:=TdwsRTTIVariant.FromValue(obj);
end;

// FromValue
//
class function TdwsRTTIVariant.FromValue(const v : TValue) : IUnknown;
var
   o : TdwsRTTIVariant;
begin
   o:=TdwsRTTIVariant.Create;
   o.FValue:=v;
   o.FType:=vRTTIContext.GetType(v.TypeInfo);
   Result:=o;
end;

// TypeKind
//
function TdwsRTTIVariant.TypeKind : TTypeKind;
begin
   Result:=FValue.Kind;
end;

// AsObject
//
function TdwsRTTIVariant.AsObject : TObject;
begin
   Result:=FValue.AsObject;
end;

// InstancePointer
//
function TdwsRTTIVariant.InstancePointer : Pointer;
begin
   case TypeKind of
      tkRecord :
         Result:=FValue.GetReferenceToRawData;
   else
      Result:=FValue.AsObject;
   end;
end;

// ------------------
// ------------------ TRTTIConnectorSymbol ------------------
// ------------------

// SpecializeConnector
//
function TRTTIConnectorSymbol.SpecializeConnector(table : TSymbolTable; const qualifier : String) : TConnectorSymbol;
var
   t : TRttiType;
   qualifiedName : String;
begin
   if qualifier<>'' then begin
      t:=vRTTIContext.FindType(qualifier);
      if t=nil then
         Exit(nil);
   end else t:=nil;

   qualifiedName:=name+'<'+qualifier+'>';
   Result:=TRTTIConnectorSymbol(table.FindSymbol(qualifiedName, cvMagic, TRTTIConnectorSymbol));
   if Result=nil then begin
      Result:=TRTTIConnectorSymbol.Create(qualifiedName, TdwsRTTIConnectorType.Create(table, t));
      table.AddSymbol(Result);
   end;
end;

// IsCompatible
//
function TRTTIConnectorSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
var
   st, tt : TRttiType;
begin
   if Self=typSym then Exit(True);
   Result:=inherited IsCompatible(typSym);
   if not Result then Exit;

   if typSym is TRTTIConnectorSymbol then begin
      st:=(ConnectorType as TdwsRTTIConnectorType).FRttiType;
      tt:=(TRTTIConnectorSymbol(typSym).ConnectorType as TdwsRTTIConnectorType).FRttiType;
      if (st is TRttiInstanceType) and (tt is TRttiInstanceType) then
         Result:=TRttiInstanceType(tt).MetaclassType.InheritsFrom(TRttiInstanceType(st).MetaclassType);
   end;
end;

// ------------------
// ------------------ TRTTIEnvironment ------------------
// ------------------

// FindUnknownName
//
function TRTTIEnvironment.FindUnknownName(compiler : TdwsCompiler; const name : String) : TSymbol;
var
   rttiField : TRttiField;
   rttiProp : TRttiProperty;
   rttiSymbol : TRTTIConnectorSymbol;
   externalSymbol : TExternalVarSymbol;
   funcSymbol : TFuncSymbol;
   table : TSymbolTable;
begin
   Result:=nil;
   if FRttiType=nil then exit;

   rttiSymbol:=TRTTIConnectorSymbol(compiler.CurrentProg.Table.FindSymbol(SYS_RTTIVARIANT, cvMagic, TRTTIConnectorSymbol));
   if rttiSymbol=nil then Exit;

   table:=compiler.CurrentProg.Root.Table;

   rttiField:=FRttiType.GetField(name);
   if rttiField<>nil then begin
      funcSymbol:=TFuncSymbol.Create(name, fkFunction, 0);
      funcSymbol.Typ:=RTTITypeToTypeSymbol(rttiField.FieldType, table, rttiSymbol);
      funcSymbol.Executable:=TRTTIEnvironmentFieldRead.Create(Self, rttiField);
      if eoAllowFieldWrite in Options then begin
         externalSymbol:=TExternalVarSymbol.Create(name, funcSymbol.Typ);
         externalSymbol.ReadFunc:=funcSymbol;
         funcSymbol:=TFuncSymbol.Create(name, fkProcedure, 0);
         funcSymbol.AddParam(TParamSymbol.Create('v', externalSymbol.Typ));
         funcSymbol.Executable:=TRTTIEnvironmentFieldWrite.Create(Self, rttiField);
         externalSymbol.WriteFunc:=funcSymbol;
         table.AddSymbol(externalSymbol);
         Exit(externalSymbol);
      end else begin
         table.AddSymbol(funcSymbol);
         Exit(funcSymbol);
      end;
   end;

   rttiProp:=FRttiType.GetProperty(name);
   if rttiProp<>nil then begin
      externalSymbol:=TExternalVarSymbol.Create(name, RTTITypeToTypeSymbol(rttiProp.PropertyType, table, rttiSymbol));
      if rttiProp.IsReadable then begin
         funcSymbol:=TFuncSymbol.Create(name, fkFunction, 0);
         funcSymbol.Typ:=externalSymbol.Typ;
         funcSymbol.Executable:=TRTTIEnvironmentPropRead.Create(Self, rttiProp);
         externalSymbol.ReadFunc:=funcSymbol;
      end;
      if rttiProp.IsWritable then begin
         funcSymbol:=TFuncSymbol.Create(name, fkProcedure, 0);
         funcSymbol.AddParam(TParamSymbol.Create('v', externalSymbol.Typ));
         funcSymbol.Executable:=TRTTIEnvironmentPropWrite.Create(Self, rttiProp);
         externalSymbol.WriteFunc:=funcSymbol;
      end;
      table.AddSymbol(externalSymbol);
      Exit(externalSymbol);
   end;
end;

// GetDefaultEnvironment
//
procedure TRTTIEnvironment.GetDefaultEnvironment(var enviro : IdwsEnvironment);
begin
   Assert(enviro=nil);
   enviro:=TRTTIRuntimeEnvironment.Create(FDefaultEnvironment);
end;

// SetForClass
//
procedure TRTTIEnvironment.SetForClass(cls : TClass);
begin
   RttiType:=vRTTIContext.GetType(cls.ClassInfo);
end;

// SetDefaultEnvironment
//
procedure TRTTIEnvironment.SetDefaultEnvironment(const val : TValue);
begin
   FDefaultEnvironment:=val;
   if val.IsEmpty then
      FRttiType:=nil
   else FRttiType:=vRTTIContext.GetType(val.TypeInfo);
end;

// SetRttiType
//
procedure TRTTIEnvironment.SetRttiType(const val : TRttiType);
begin
   if FRttiType=val then Exit;
   FRttiType:=val;
   FDefaultEnvironment:=vEmptyTValue;
end;

// ------------------
// ------------------ TRTTIEnvironmentCallable ------------------
// ------------------

// Create
//
constructor TRTTIEnvironmentCallable.Create(environment : TRTTIEnvironment);
begin
   FEnvironment:=environment;
end;

// InitSymbol
//
procedure TRTTIEnvironmentCallable.InitSymbol(symbol : TSymbol; const msgs : TdwsCompileMessageList);
begin
   // nothing
end;

// InitExpression
//
procedure TRTTIEnvironmentCallable.InitExpression(expr : TExprBase);
begin
   // nothing
end;

// SubExpr
//
function TRTTIEnvironmentCallable.SubExpr(i : Integer) : TExprBase;
begin
   Result:=nil;
end;

// SubExprCount
//
function TRTTIEnvironmentCallable.SubExprCount : Integer;
begin
   Result:=0;

end;

// CompileTimeCheck
//
procedure TRTTIEnvironmentCallable.CompileTimeCheck(context : TdwsCompilerContext; expr : TFuncExprBase);
begin
   // nothing yet
end;

// ------------------
// ------------------ TRTTIEnvironmentFieldRead ------------------
// ------------------

// Create
//
constructor TRTTIEnvironmentField.Create(environment : TRTTIEnvironment; rttiField : TRttiField);
begin
   inherited Create(environment);
   FField:=rttiField;
end;

// ------------------
// ------------------ TRTTIEnvironmentFieldRead ------------------
// ------------------

// Call
//
procedure TRTTIEnvironmentFieldRead.Call(exec : TdwsProgramExecution; func : TFuncSymbol);
var
   info : TProgramInfo;
   result : Variant;
   p : Pointer;
   v : TValue;
begin
   info:=exec.AcquireProgramInfo(func);
   try
      p:=TRTTIRuntimeEnvironment.Instance(exec);
      v:=FField.GetValue(p);
      ValueToVariant(v, result);
      info.ResultAsVariant:=result;
   finally
      exec.ReleaseProgramInfo(info);
   end;
end;

// ------------------
// ------------------ TRTTIEnvironmentFieldWrite ------------------
// ------------------

// Call
//
procedure TRTTIEnvironmentFieldWrite.Call(exec : TdwsProgramExecution; func : TFuncSymbol);
var
   info : TProgramInfo;
   v : Variant;
   value : TValue;
begin
   info:=exec.AcquireProgramInfo(func);
   try
      v:=info.ParamAsVariant[0];
      VariantToValue(v, value);
      FField.SetValue(TRTTIRuntimeEnvironment.Instance(exec), value);
   finally
      exec.ReleaseProgramInfo(info);
   end;
end;

// ------------------
// ------------------ TRTTIEnvironmentProp ------------------
// ------------------

// Create
//
constructor TRTTIEnvironmentProp.Create(environment : TRTTIEnvironment; rttiProp : TRttiProperty);
begin
   inherited Create(environment);
   FProp:=rttiProp;
end;

// ------------------
// ------------------ TRTTIEnvironmentPropRead ------------------
// ------------------

// Call
//
procedure TRTTIEnvironmentPropRead.Call(exec : TdwsProgramExecution; func : TFuncSymbol);
var
   info : TProgramInfo;
   result : Variant;
begin
   info:=exec.AcquireProgramInfo(func);
   try
      ValueToVariant(FProp.GetValue(TRTTIRuntimeEnvironment.Instance(exec)), result);
      info.ResultAsVariant:=result;
   finally
      exec.ReleaseProgramInfo(info);
   end;
end;

// ------------------
// ------------------ TRTTIEnvironmentPropWrite ------------------
// ------------------

// Call
//
procedure TRTTIEnvironmentPropWrite.Call(exec : TdwsProgramExecution; func : TFuncSymbol);
var
   info : TProgramInfo;
begin
   info:=exec.AcquireProgramInfo(func);
   try
      FProp.SetValue(TRTTIRuntimeEnvironment.Instance(exec),
                     TValue.FromVariant(info.ParamAsVariant[0]));
   finally
      exec.ReleaseProgramInfo(info);
   end;
end;

// ------------------
// ------------------ TRTTIRuntimeEnvironment ------------------
// ------------------

// Create
//
constructor TRTTIRuntimeEnvironment.Create(const value : TValue);
begin
   inherited Create;
   FValue:=value;
end;

// Instance
//
class function TRTTIRuntimeEnvironment.Instance(exec : TdwsProgramExecution) : Pointer;
var
   v : TValue;
begin
   v:=(exec.Environment.GetSelf as TRTTIRuntimeEnvironment).Value;
   if v.IsObject then
      Result:=v.AsObject
   else Result:=v.GetReferenceToRawData;
end;

// ------------------
// ------------------ TdwsRTTIConnectorIndexedProperty ------------------
// ------------------

// Create
//
constructor TdwsRTTIConnectorIndexedProperty.Create(const propertyName: String;
   const params: TConnectorParamArray; methodType: TdwsRTTIMethodType);
begin
   FPropertyName:=propertyName;
   FMethodType:=methodType;
end;

// Call
//
function TdwsRTTIConnectorIndexedProperty.Call(const base : Variant;
                                               const args : TConnectorArgs) : TData;
var
   i, k : Integer;
   instance : TdwsRTTIVariant;
   instanceClass : TClass;
   resultValue : TValue;
   prop : TRttiProperty;
   value : Variant;
   values : TArray<TValue>;
   cip : TdwsRTTIConnectorIndexedProperty;
   argVarType : TVarType;
begin
   // current implementation limitation
   if Length(args)<1 then
      raise EdwsRTTIException.CreateFmt(CPE_BadNumberOfParameters, [1, Length(args)]);

   SetLength(Result, 1);
   instance:=IUnknown(base) as TdwsRTTIVariant;
   instanceClass:=instance.RTTIType.AsInstance.MetaclassType;

   argVarType:=VarType(args[0][0]);

   for i:=0 to High(vIndexedProperties) do begin

      if not instanceClass.InheritsFrom(vIndexedProperties[i].ClassType) then
         continue;
      if argVarType<>vIndexedProperties[i].PropType then
         continue;

      if    UnicodeSameText(FPropertyName, vIndexedProperties[i].PropName)
         or (    (FPropertyName = '')
             and vIndexedProperties[i].IsDefault) then begin

         case FMethodType of
            mtPropertyGet : begin
               SetLength(values, Length(args));
               for k:=Low(values) to High(values) do
                  VariantToValue(args[k][0], values[k]);
               ValueToVariant(vIndexedProperties[i].GetValue(instance.InstancePointer, values), value);
            end;
            mtPropertySet : begin
               if not Assigned(vIndexedProperties[i].SetValue) then
                  raise EdwsRTTIException.CreateFmt('"%s.%s" is not writable',
                                                    [instance.RTTIType.Name, FPropertyName]);
               SetLength(values, Length(args));
               for k:=Low(values) to High(values) do
                  VariantToValue(args[k][0], values[k]);
               vIndexedProperties[i].SetValue(instance.InstancePointer, values);
            end;
         else
            Assert(False);
         end;
         Result[0]:=value;
         Exit;

      end else begin

         prop:=instance.RTTIType.GetProperty(FPropertyName);
         if     (prop<>nil)
            and vIndexedProperties[i].IsDefault then begin

            resultValue:=prop.GetValue(instance.InstancePointer);
            ValueToVariant(resultValue, value);

            cip:=TdwsRTTIConnectorIndexedProperty.Create(vIndexedProperties[i].PropName, nil, FMethodType);
            try
               Result[0]:=cip.Call(value, args)[0];
            finally
               cip.Free;
            end;
            Exit;

         end;

      end;
   end;

   raise EdwsRTTIException.CreateFmt('"%s" does not have a method "%s" exposed via RTTI accepting a %s',
                                     [instance.RTTIType.Name, FPropertyName, VarTypeAsText(argVarType)]);
end;

// NeedDirectReference
//
function TdwsRTTIConnectorIndexedProperty.NeedDirectReference: Boolean;
begin
   Result := False;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  RegisterRTTIIndexedProperty(TStrings, 'Strings', True, varInt64, TStrings_GetStrings, TStrings_SetStrings);
  RegisterRTTIIndexedProperty(TComponent, 'Components', False, varInt64, TComponent_GetComponents, nil);

end.
