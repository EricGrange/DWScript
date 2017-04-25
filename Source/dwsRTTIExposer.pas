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
unit dwsRTTIExposer;

{$I dws.inc}

interface

uses
   Classes, SysUtils, RTTI, TypInfo,
   dwsXPlatform, dwsStrings,
   dwsComp, dwsSymbols, dwsExprs, dwsStack, dwsInfo;

type

   TdwsRTTIExposerOption = (
      eoExposeVirtual, eoNoFreeOnCleanup, eoExposePublic
      );

   TdwsRTTIExposerOptions = set of TdwsRTTIExposerOption;

   {$RTTI EXPLICIT METHODS([vcPublic, vcPublished]) PROPERTIES([vcPublic, vcPublished])}

   // TdwsPublished
   //
   dwsPublished = class (TCustomAttribute)
      private
         FNameOverride : String;
      public
         constructor Create(const nameOverride : String); overload;
         property NameOverride : String read FNameOverride;

         class function NameOf(item : TRttiNamedObject) : String; static;
   end;

   // TdwsNotPublished
   //
   dwsNotPublished = class (TCustomAttribute)
   end;

   {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

   // TdwsRTTIExposer
   //
   TdwsRTTIExposer = class helper for TdwsUnit
      protected
         class var vRTTIContext : TRttiContext;

         function ExposeRTTIClass(cls : TRttiInstanceType;
                                  const options : TdwsRTTIExposerOptions) : TdwsClass;

         function ExposeRTTIConstructor(meth : TRttiMethod; scriptClass : TdwsClass;
                                        const options : TdwsRTTIExposerOptions) : TdwsConstructor;
         function ExposeRTTIMethod(meth : TRttiMethod; scriptClass : TdwsClass;
                                   const options : TdwsRTTIExposerOptions) : TdwsMethod;
         function ExposeRTTIProperty(prop : TRttiProperty; scriptClass : TdwsClass;
                                     const options : TdwsRTTIExposerOptions) : TdwsProperty;
         function ExposeRTTIParameter(param : TRttiParameter; scriptParameters : TdwsParameters;
                                      const options : TdwsRTTIExposerOptions) : TdwsParameter;

         function ExposeRTTIEnumeration(enum : TRttiEnumerationType;
                                        const options : TdwsRTTIExposerOptions) : TdwsEnumeration;

         function ExposeRTTIRecord(rec : TRttiRecordType;
                                   const options : TdwsRTTIExposerOptions) : TdwsRecord;

         function ExposeRTTIInterface(intf : TRttiInterfaceType;
                                      const options : TdwsRTTIExposerOptions) : TdwsInterface;

         function ExposeRTTIDynamicArray(intf : TRttiDynamicArrayType;
                                      const options : TdwsRTTIExposerOptions) : TdwsArray;

         procedure DoStandardCleanUp(externalObject: TObject);

      public
         function ExposeRTTI(ATypeInfo : Pointer; const options : TdwsRTTIExposerOptions = []) : TdwsSymbol;

         class function RTTITypeToScriptType(const aType : TRTTIType) : String; static;
         class function RTTIVisibilityToVisibility(const aVisibility : TMemberVisibility) : TdwsVisibility; static;
   end;

   TdwsRTTIInvoker = class;
   TdwsRTTIHelper = class;

   // TdwsRTTIHelper
   //
   {: Helper to wraps a Delphi side object to DWS based on RTTI data. }
   TdwsRTTIHelper = class
      private
         FInstanceType : TRttiInstanceType;
         FInstanceConstructor : TRttiMethod;
         FInvokers : array of TdwsRTTIInvoker;

      public
         constructor Create(anInstanceType : TRttiInstanceType);
         destructor Destroy; override;

         procedure AddInvoker(invoker : TdwsRTTIInvoker);
         procedure DoStandardCreate(info: TProgramInfo; var extObject: TObject);
   end;

   // TdwsRTTIInvoker
   //
   {: Invokes a Delphi method from a DWS context. }
   TdwsRTTIInvoker = class
      private
         FHelper : TdwsRTTIHelper;

      public
         procedure Invoke(info : TProgramInfo; externalObject : TObject); virtual; abstract;

         property Helper : TdwsRTTIHelper read FHelper;

         class procedure AssignIInfoFromValue(const info : IInfo; const value : TValue;
                                              asType : TRttiType); static;
         class procedure AssignRecordFromValue(const recInfo : IInfo; const value : TValue;
                                               asType : TRttiType); static;

         class function ValueFromParam(progInfo : TProgramInfo; const paramName : String;
                                       asType : TRttiType) : TValue; static;
         class function ValueFromIInfo(asType : TRttiType; const info : IInfo) : TValue; static;
         class function ValueFromRecord(asType : TRttiType; const recInfo : IInfo) : TValue; static;
   end;

   // TdwsRTTIMethodInvoker
   //
   {: Invokes a Delphi method from a DWS context. }
   TdwsRTTIMethodInvoker = class (TdwsRTTIInvoker)
      private
         FMethod : TRttiMethod;  // referred, not owned
         FTypParams : array of TRttiType;
         FNameParams : array of UnicodeString;
         FVarParams : array of Integer;
         FTypResult : TRttiType;

      protected
         procedure Initialize(aMethod : TRttiMethod);

      public
         constructor Create(aMethod : TRttiMethod);

         procedure PrepareParams(info : TProgramInfo; var params : TArray<TValue>);
         procedure Invoke(info : TProgramInfo; externalObject : TObject); override;
   end;

   // TdwsRTTIConstructorInvoker
   //
   TdwsRTTIConstructorInvoker = class (TdwsRTTIMethodInvoker)
      public
         procedure InvokeConstructor(info: TProgramInfo; var extObject: TObject);
   end;

   // TdwsRTTIPropertyInvoker
   //
   TdwsRTTIPropertyInvoker = class (TdwsRTTIInvoker)
      protected
         FProperty : TRttiProperty; // referred, not owned
         FTyp : TRttiType;

         procedure Initialize(aProperty : TRttiProperty);

      public
         constructor Create(aProperty : TRttiProperty);
   end;

   // TdwsRTTISetterInvoker
   //
   TdwsRTTISetterInvoker = class (TdwsRTTIPropertyInvoker)
      public
         procedure Invoke(info : TProgramInfo; externalObject : TObject); override;
   end;

   // TdwsRTTIGetterInvoker
   //
   TdwsRTTIGetterInvoker = class (TdwsRTTIPropertyInvoker)
      public
         procedure Invoke(info : TProgramInfo; externalObject : TObject); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
const
  cTYPEKIND_NAMES: array[TTypeKind] of string = (
    'Unknown', 'Integer', 'Char', 'Enumeration', 'Float',
    'String', 'Set', 'Class', 'Method', 'WChar', 'LString', 'WString',
    'Variant', 'Array', 'Record', 'Interface', 'Int64', 'DynArray', 'UString',
    'ClassRef', 'Pointer', 'Procedure');

// ------------------
// ------------------ dwsPublished ------------------
// ------------------

// Create
//
constructor dwsPublished.Create(const nameOverride : String);
begin
   FNameOverride:=nameOverride;
end;

// NameOf
//
class function dwsPublished.NameOf(item : TRttiNamedObject) : String;
var
   attrib : TCustomAttribute;
begin
   Result:='';
   for attrib in item.GetAttributes do begin
      if attrib.ClassType=dwsPublished then
         Result:=dwsPublished(attrib).NameOverride;
   end;
   if Result='' then
      Result:=item.Name;
end;

// ------------------
// ------------------ TdwsRTTIHelper ------------------
// ------------------

// Create
//
constructor TdwsRTTIHelper.Create(anInstanceType : TRttiInstanceType);
var
   meth : TRttiMethod;
begin
   inherited Create;
   FInstanceType:=anInstanceType;

   for meth in anInstanceType.GetMethods do begin
      if     (meth.MethodKind=TypInfo.mkConstructor)
         and ASCIISameText(meth.Name, 'Create')
         and (Length(meth.GetParameters)=0) then begin
         FInstanceConstructor:=meth;
         Break;
      end;
   end;
end;

// Destroy
//
destructor TdwsRTTIHelper.Destroy;
var
   invoker : TdwsRTTIInvoker;
begin
   for invoker in FInvokers do
      invoker.Free;
   inherited;
end;

// AddInvoker
//
procedure TdwsRTTIHelper.AddInvoker(invoker : TdwsRTTIInvoker);
var
   n : Integer;
begin
   n:=Length(FInvokers);
   SetLength(FInvokers, n+1);
   FInvokers[n]:=invoker;
   invoker.FHelper:=Self;
end;

// DoStandardCreate
//
procedure TdwsRTTIHelper.DoStandardCreate(info: TProgramInfo; var extObject: TObject);
begin
   extObject:=FInstanceConstructor.Invoke(FInstanceType.MetaclassType, []).AsObject;
end;

// ------------------
// ------------------ TdwsRTTIExposer ------------------
// ------------------

// ExposeRTTI
//
function TdwsRTTIExposer.ExposeRTTI(ATypeInfo : Pointer; const options : TdwsRTTIExposerOptions = []) : TdwsSymbol;
var
   typ : TRttiType;
begin
   typ:=vRTTIContext.GetType(ATypeInfo);
   if typ is TRttiInstanceType then
      Result:=ExposeRTTIClass(TRttiInstanceType(typ), options)
   else if typ is TRttiEnumerationType then
      Result:=ExposeRTTIEnumeration(TRttiEnumerationType(typ), options)
   else if typ is TRttiRecordType then
      Result:=ExposeRTTIRecord(TRttiRecordType(typ), options)
   else if typ is TRttiInterfaceType then
      Result:=ExposeRTTIInterface(TRttiInterfaceType(typ), options)
   else if typ is TRttiDynamicArrayType then
      Result := ExposeRTTIDynamicArray(TRttiDynamicArrayType(typ), options)
   else raise Exception.CreateFmt('Expose unsupported for %s', [typ.ClassName]);
end;

// RTTITypeToScriptType
//
class function TdwsRTTIExposer.RTTITypeToScriptType(const aType : TRTTIType) : String;
var
  LDynType: string;
  LTypeKind: TTypeKind;
begin
   if aType<>nil then begin
      case aType.TypeKind of
         tkInteger, tkInt64 :
            Result := SYS_INTEGER;
         tkChar, tkString, tkUString, tkWChar, tkLString, tkWString :
            Result := SYS_STRING;
         tkFloat :
            Result := SYS_FLOAT;
         tkVariant :
            Result := SYS_VARIANT;
         tkRecord :
            Result:=dwsPublished.NameOf(aType);
         tkSet, tkProcedure, tkPointer, {tkDynArray, }tkInterface, tkArray,
            tkEnumeration, tkClassRef, tkClass, tkMethod : begin
             Result := SYS_VARIANT; // todo, someday maybe...
         end;
         tkUnknown : begin
             Result := SYS_VARIANT; // unsupported
             Assert(False);
         end;
         tkDynArray: begin
          LTypeKind := TRttiDynamicArrayType( aType ).ElementType.TypeKind;
            //  we want to raise exception if the tpe is not expected
            case LTypeKind of
              tkInteger, tkInt64:
                LDynType := SYS_INTEGER;
              tkChar, tkString, tkUString, tkWChar, tkLString, tkWString:
                LDynType := SYS_STRING;
              tkFloat:
                LDynType := SYS_FLOAT;
              tkVariant:
                LDynType := SYS_VARIANT;
              else
                raise Exception.Create(
                  'Cannot handle this dynamic array RTTI type, maybe you haven''t exposed type"' + atype.name + '" yet?');
            end;
            // set the type, we expect that the type is already exposed
            Result := aType.Name;
         end
      else
         Result := SYS_VARIANT;
         Assert(False);
      end;
   end else Result:='';
end;

// RTTIVisibilityToVisibility
//
class function TdwsRTTIExposer.RTTIVisibilityToVisibility(const aVisibility : TMemberVisibility) : TdwsVisibility;
const
   cVisibilityMap : array [TMemberVisibility] of TdwsVisibility = (
      cvPrivate, cvProtected, cvPublic, cvPublished
      );
begin
   Result:=cVisibilityMap[aVisibility];
end;

// ExposeRTTIClass
//
function TdwsRTTIExposer.ExposeRTTIClass(cls : TRttiInstanceType; const options : TdwsRTTIExposerOptions) : TdwsClass;
var
   exposableVisibilities : set of TMemberVisibility;

   function ShouldExpose(item : TRttiMember) : Boolean;
   var
      attrib : TCustomAttribute;
   begin
      Result:=(item.Visibility in exposableVisibilities);
      for attrib in item.GetAttributes do begin
         if attrib.ClassType=dwsPublished then
            Result:=True
         else if attrib.ClassType=dwsNotPublished then
            Result:=False;
      end;
   end;

var
   meth : TRttiMethod;
   prop : TRttiProperty;
   helper : TdwsRTTIHelper;
   scriptConstructor : TdwsConstructor;
begin
   if eoExposePublic in options then
      exposableVisibilities:=[mvPublic, mvPublished]
   else exposableVisibilities:=[mvPublished];

   Result:=Classes.Add;
   Result.Name:=dwsPublished.NameOf(cls);
   if not (eoNoFreeOnCleanup in options) then
      Result.OnCleanUp:=DoStandardCleanUp;

   helper:=TdwsRTTIHelper.Create(cls);
   Result.HelperObject:=helper;

   for meth in cls.GetMethods do begin
      if meth.Parent.Name='TObject' then continue;
      if not meth.HasExtendedInfo then continue;
      if ShouldExpose(meth) then begin
         case meth.MethodKind of
            TypInfo.mkProcedure, TypInfo.mkFunction, TypInfo.mkClassProcedure, TypInfo.mkClassFunction :
               ExposeRTTIMethod(meth, Result, options);
            TypInfo.mkConstructor :
               ExposeRTTIConstructor(meth, Result, options);
         end;
      end;
   end;
   if Result.Constructors.IndexOf('Create')<0 then begin
      scriptConstructor:=(Result.Constructors.Add as TdwsConstructor);
      scriptConstructor.OnEval:=helper.DoStandardCreate;
      scriptConstructor.Name:='Create';
   end;

   for prop in cls.GetProperties do begin
      if ShouldExpose(prop) then begin
         ExposeRTTIProperty(prop, Result, options);
      end;
   end;
end;

// ExposeRTTIConstructor
//
function TdwsRTTIExposer.ExposeRTTIConstructor(meth : TRttiMethod; scriptClass : TdwsClass;
                                        const options : TdwsRTTIExposerOptions) : TdwsConstructor;
var
   param : TRttiParameter;
   helper : TdwsRTTIHelper;
   invoker : TdwsRTTIConstructorInvoker;
begin
   Result:=scriptClass.Constructors.Add;
   Result.Name:=dwsPublished.NameOf(meth);

   if eoExposeVirtual in options then begin
      if meth.DispatchKind in [dkVtable, dkDynamic] then
         Result.Attributes:=Result.Attributes+[maVirtual];
   end;

   for param in meth.GetParameters do
      ExposeRTTIParameter(param, Result.Parameters, options);

   helper:=scriptClass.HelperObject as TdwsRTTIHelper;
   invoker:=TdwsRTTIConstructorInvoker.Create(meth);
   helper.AddInvoker(invoker);
   Result.OnEval:=invoker.InvokeConstructor;
end;

function TdwsRTTIExposer.ExposeRTTIDynamicArray(intf: TRttiDynamicArrayType;
  const options: TdwsRTTIExposerOptions): TdwsArray;
var
  LType: string;
  LTypeKind: TTypeKind;
begin
  LTypeKind := intf.ElementType.TypeKind;
  case LTypeKind of
    tkInteger,
    tkInt64: LType := SYS_INTEGER;
    tkFloat: LType := SYS_FLOAT;
    tkChar, tkString, tkUString, tkWChar, tkLString, tkWString:
      LType := SYS_STRING;
    tkVariant: LType := SYS_VARIANT;
    else raise Exception.CreateFmt('Cannot expose dynamic array of type: %s', [
      cTYPEKIND_NAMES[LTypeKind]]);
  end; // case LTypeKind of
  Result := Self.Arrays.Add;
  Result.DataType := LType;
  Result.IsDynamic := True;
  Result.Name := intf.Name;
end;

// ExposeRTTIMethod
//
function TdwsRTTIExposer.ExposeRTTIMethod(meth : TRttiMethod; scriptClass : TdwsClass;
                                          const options : TdwsRTTIExposerOptions) : TdwsMethod;
var
   param : TRttiParameter;
   helper : TdwsRTTIHelper;
   invoker : TdwsRTTIMethodInvoker;
begin
   Result:=scriptClass.Methods.Add;
   Result.Name:=dwsPublished.NameOf(meth);
   Result.ResultType:=RTTITypeToScriptType(meth.ReturnType);

   case meth.MethodKind of
      TypInfo.mkClassProcedure :
         Result.Kind:=TMethodKind.mkClassProcedure;
      TypInfo.mkClassFunction :
         Result.Kind:=TMethodKind.mkClassFunction;
   end;

   if eoExposeVirtual in options then begin
      if meth.DispatchKind in [dkVtable, dkDynamic] then
         Result.Attributes:=Result.Attributes+[maVirtual];
   end;

   for param in meth.GetParameters do
      ExposeRTTIParameter(param, Result.Parameters, options);

   helper:=scriptClass.HelperObject as TdwsRTTIHelper;
   invoker:=TdwsRTTIMethodInvoker.Create(meth);
   helper.AddInvoker(invoker);
   Result.OnEval:=invoker.Invoke;
end;

// ExposeRTTIProperty
//
function TdwsRTTIExposer.ExposeRTTIProperty(prop : TRttiProperty; scriptClass : TdwsClass;
                                            const options : TdwsRTTIExposerOptions) : TdwsProperty;
var
   helper : TdwsRTTIHelper;
   setterInvoker : TdwsRTTISetterInvoker;
   getterInvoker : TdwsRTTIGetterInvoker;
   setterMethod, getterMethod : TdwsMethod;
   setterParam : TdwsParameter;
begin
   Result:=scriptClass.Properties.Add;
   Result.Name:=dwsPublished.NameOf(prop);
   Result.DataType:=RTTITypeToScriptType(prop.PropertyType);

   helper:=scriptClass.HelperObject as TdwsRTTIHelper;

   if prop.IsReadable then begin
      getterInvoker:=TdwsRTTIGetterInvoker.Create(prop);
      helper.AddInvoker(getterInvoker);
      getterMethod:=(scriptClass.Methods.Add as TdwsMethod);
      getterMethod.Name:='Get'+prop.Name;
      Result.ReadAccess:=getterMethod.Name;
      getterMethod.ResultType:=RTTITypeToScriptType(prop.PropertyType);
      getterMethod.OnEval:=getterInvoker.Invoke;
   end;

   if prop.IsWritable then begin
      setterInvoker:=TdwsRTTISetterInvoker.Create(prop);
      helper.AddInvoker(setterInvoker);
      setterMethod:=(scriptClass.Methods.Add as TdwsMethod);
      setterMethod.Name:='Set'+prop.Name;
      Result.WriteAccess:=setterMethod.Name;
      setterParam:=setterMethod.Parameters.Add;
      setterParam.Name:='v';
      setterParam.DataType:=RTTITypeToScriptType(prop.PropertyType);
      setterMethod.OnEval:=setterInvoker.Invoke;
   end;
end;

// ExposeRTTIParameter
//
function TdwsRTTIExposer.ExposeRTTIParameter(param : TRttiParameter; scriptParameters : TdwsParameters;
                                             const options : TdwsRTTIExposerOptions) : TdwsParameter;
begin
   Result:=scriptParameters.Add;
   Result.Name:=dwsPublished.NameOf(param);
   Result.DataType:=RTTITypeToScriptType(param.ParamType);
   if pfVar in param.Flags then
      Result.IsVarParam:=True;
   if pfConst in param.Flags then
      Result.IsWritable:=True;
end;

// ExposeRTTIEnumeration
//
function TdwsRTTIExposer.ExposeRTTIEnumeration(enum : TRttiEnumerationType;
                                               const options : TdwsRTTIExposerOptions) : TdwsEnumeration;
var
   i : Integer;
   enumName : String;
   element : TdwsElement;
begin
   Result:=Enumerations.Add;
   Result.Name:=dwsPublished.NameOf(enum);

   for i:=enum.MinValue to enum.MaxValue do begin
      enumName:=GetEnumName(enum.Handle, i);
      element:=Result.Elements.Add;
      element.Name:=enumName;
      element.UserDefValue:=i;
   end;
end;

// ExposeRTTIRecord
//
function TdwsRTTIExposer.ExposeRTTIRecord(rec : TRttiRecordType;
                                          const options : TdwsRTTIExposerOptions) : TdwsRecord;
var
   field : TRttiField;
   member : TdwsMember;
begin
   Result:=Records.Add;
   Result.Name:=dwsPublished.NameOf(rec);

   for field in rec.GetFields do begin
      member:=Result.Members.Add;
      member.Name:=field.Name;
      member.DataType:=RTTITypeToScriptType(field.FieldType);
      member.Visibility:=RTTIVisibilityToVisibility(field.Visibility);
   end;
end;

// ExposeRTTIInterface
//
function TdwsRTTIExposer.ExposeRTTIInterface(intf : TRttiInterfaceType;
                                      const options : TdwsRTTIExposerOptions) : TdwsInterface;
var
   rttiMeth : TRttiMethod;
   rttiParam : TRttiParameter;
   meth : TdwsMethod;
begin
   Result:=Interfaces.Add;
   Result.Name:=dwsPublished.NameOf(intf);

   for rttiMeth in intf.GetMethods do begin
      meth:=Result.Methods.Add;
      meth.Name:=rttiMeth.Name;
      meth.ResultType:=RTTITypeToScriptType(rttiMeth.ReturnType);

      for rttiParam in rttiMeth.GetParameters do
         ExposeRTTIParameter(rttiParam, meth.Parameters, options);
   end;
end;

// DoStandardCleanUp
//
procedure TdwsRTTIExposer.DoStandardCleanUp(externalObject: TObject);
begin
   externalObject.Free;
end;

// ------------------
// ------------------ TdwsRTTIInvoker ------------------
// ------------------

// AssignIInfoFromValue
//
class procedure TdwsRTTIInvoker.AssignIInfoFromValue(const info : IInfo; const value : TValue;
                                                     asType : TRttiType);
begin
   case asType.TypeKind of
      tkInteger, tkInt64 :
         info.Value:=value.AsInt64;
      tkFloat :
         info.Value:=value.AsType<Double>;
      tkChar, tkString, tkUString, tkWChar, tkLString, tkWString :
         info.Value:=value.AsString;
      tkRecord :
         AssignRecordFromValue(info, value, asType);
      tkEnumeration :
         if asType.Handle=TypeInfo(Boolean) then
            info.Value:=value.AsBoolean
         else info.Value:=value.AsInt64;
   else
      info.Value:=value.AsVariant;
   end;
end;

// AssignRecordFromValue
//
class procedure TdwsRTTIInvoker.AssignRecordFromValue(const recInfo : IInfo; const value : TValue;
                                                      asType : TRttiType);
var
   field : TRttiField;
   rawData : Pointer;
begin
   rawData:=value.GetReferenceToRawData;
   for field in asType.AsRecord.GetFields do
      AssignIInfoFromValue(recInfo.Member[field.Name],
                           field.GetValue(rawData),
                           field.FieldType);
end;

// ValueFromParam
//
class function TdwsRTTIInvoker.ValueFromParam(progInfo : TProgramInfo; const paramName : String;
                                              asType : TRttiType) : TValue;
begin
   Result:=ValueFromIInfo(asType, progInfo.Vars[paramName]);
end;

// ValueFromIInfo
//
class function TdwsRTTIInvoker.ValueFromIInfo(asType : TRttiType; const info : IInfo) : TValue;
type
  TValueArray = TArray<TValue>;
var
  LLen: Integer;
  Index: Integer;
begin
   case asType.TypeKind of
      tkInteger, tkInt64 :
         Result:=TValue.From<Int64>(info.ValueAsInteger);
      tkFloat :
         Result:=TValue.From<Double>(info.ValueAsFloat);
      tkChar, tkString, tkUString, tkWChar, tkLString, tkWString :
         Result:=TValue.From<String>(info.ValueAsString);
      tkVariant :
         Result:=TValue.From<Variant>(info.Value);
      tkRecord :
         Result:=ValueFromRecord(asType, info);
      tkEnumeration :
         if asType.Handle=TypeInfo(Boolean) then
            Result:=info.ValueAsBoolean
         else Result:=info.ValueAsInteger;
      tkDynArray: begin
        LLen := Length(info.Data);
        TValue.MakeWithoutCopy(NIL, asType.Handle, Result);
        DynArraySetLength(PPointer(Result.GetReferenceToRawData)^, Result.TypeInfo, 1, @LLen);
        for Index := Low(info.Data) to High(info.Data) do
          Result.SetArrayElement(Index, TValue.FromVariant(info.Data[Index]));
      end
   else
      // Unsupported
      Result:=TValue.Empty;
      Assert(False);
   end;
end;

// ValueFromRecord
//
class function TdwsRTTIInvoker.ValueFromRecord(asType : TRttiType; const recInfo : IInfo) : TValue;
var
   field : TRttiField;
   rawData : Pointer;
begin
   TValue.Make(nil, asType.Handle, Result);
   rawData:=Result.GetReferenceToRawData;
   for field in asType.AsRecord.GetFields do
      field.SetValue(rawData, ValueFromIInfo(field.FieldType, recInfo.Member[field.Name]));
end;

// ------------------
// ------------------ TdwsRTTIMethodInvoker ------------------
// ------------------

// Create
//
constructor TdwsRTTIMethodInvoker.Create(aMethod : TRttiMethod);
begin
   inherited Create;
   Initialize(aMethod);
end;

// Initialize
//
procedure TdwsRTTIMethodInvoker.Initialize(aMethod : TRttiMethod);
var
   methParams : TArray<TRttiParameter>;
   param : TRttiParameter;
   i, n, k : Integer;
begin
   FMethod:=aMethod;
   methParams:=aMethod.GetParameters;
   n:=Length(methParams);
   SetLength(FTypParams, n);
   SetLength(FNameParams, n);
   k:=0;
   for i:=0 to n-1 do begin
      param:=methParams[i];
      FTypParams[i]:=param.ParamType;
      FNameParams[i]:=param.Name;
      if (pfVar in param.Flags) then begin
         SetLength(FVarParams, k+1);
         FVarParams[k]:=i;
         Inc(k);
      end;
   end;
   FTypResult:=aMethod.ReturnType;
end;

// PrepareParams
//
procedure TdwsRTTIMethodInvoker.PrepareParams(info : TProgramInfo; var params : TArray<TValue>);
var
   i : Integer;
begin
   SetLength(params, Length(FTypParams));
   for i:=0 to High(FTypParams) do begin
      params[i]:=ValueFromParam(info, FNameParams[i], FTypParams[i]);
   end;
end;

// Invoke
//
procedure TdwsRTTIMethodInvoker.Invoke(info : TProgramInfo; externalObject : TObject);
var
   params : TArray<TValue>;
   resultValue : TValue;
   i : Integer;
begin
   PrepareParams(info, params);
   resultValue:=FMethod.Invoke(externalObject, params);
   if FTypResult<>nil then
      AssignIInfoFromValue(info.ResultVars, resultValue, FTypResult);
   for i in FVarParams do
      AssignIInfoFromValue(info.Vars[FNameParams[i]], params[i], FTypParams[i]);
end;

// ------------------
// ------------------ TdwsRTTIConstructorInvoker ------------------
// ------------------

// InvokeConstructor
//
procedure TdwsRTTIConstructorInvoker.InvokeConstructor(info: TProgramInfo; var extObject: TObject);
var
   params : TArray<TValue>;
begin
   PrepareParams(info, params);
   extObject:=FMethod.Invoke(FHelper.FInstanceType.MetaclassType, params).AsObject;
end;

// ------------------
// ------------------ TdwsRTTIPropertyInvoker ------------------
// ------------------

// Create
//
constructor TdwsRTTIPropertyInvoker.Create(aProperty : TRttiProperty);
begin
   inherited Create;
   Initialize(aProperty);
end;

// Initialize
//
procedure TdwsRTTIPropertyInvoker.Initialize(aProperty : TRttiProperty);
begin
   FProperty:=aProperty;
   FTyp:=aProperty.PropertyType;
end;

// ------------------
// ------------------ TdwsRTTISetterInvoker ------------------
// ------------------

// Invoke
//
procedure TdwsRTTISetterInvoker.Invoke(info : TProgramInfo; externalObject : TObject);
var
   value : TValue;
begin
   value:=ValueFromParam(info, 'v', FTyp);
   FProperty.SetValue(externalObject, value);
end;

// ------------------
// ------------------ TdwsRTTIGetterInvoker ------------------
// ------------------

// Invoke
//
procedure TdwsRTTIGetterInvoker.Invoke(info : TProgramInfo; externalObject : TObject);
var
   resultValue : TValue;
begin
   resultValue:=FProperty.GetValue(externalObject);
   AssignIInfoFromValue(info.ResultVars, resultValue, FTyp);
end;

end.
