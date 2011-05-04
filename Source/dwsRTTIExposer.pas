unit dwsRTTIExposer;

interface

uses SysUtils, RTTI, TypInfo, dwsComp, dwsSymbols, dwsExprs;

type

   TdwsRTTIExposerOption = (
      eoExposeVirtual
      );

   TdwsRTTIExposerOptions = set of TdwsRTTIExposerOption;

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

         procedure DoStandardCleanUp(externalObject: TObject);

      public
         procedure ExposeRTTI(ATypeInfo : Pointer; const options : TdwsRTTIExposerOptions = []);

         class function TypeKindToScriptBaseType(const aType : TTypeKind) : TBaseTypeId; static;
         class function TypeKindToScriptType(const aType : TTypeKind) : String; static;
         class function RTTITypeToScriptBaseType(const aType : TRTTIType) : TBaseTypeId; static;
         class function RTTITypeToScriptType(const aType : TRTTIType) : String; static;
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

         class procedure AssignResultFromValue(info : TProgramInfo; const value : TValue;
                                               asType : TBaseTypeId); static;
         class function ValueFromParam(info : TProgramInfo; const paramName : String;
                                       asType : TBaseTypeId) : TValue; static;
   end;

   // TdwsRTTIMethodInvoker
   //
   {: Invokes a Delphi method from a DWS context. }
   TdwsRTTIMethodInvoker = class (TdwsRTTIInvoker)
      private
         FMethod : TRttiMethod;  // referred, not owned
         FTypParams : array of TBaseTypeId;
         FNameParams : array of String;
         FTypResult : TBaseTypeId;

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
         FTyp : TBaseTypeId;

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
         and SameText(meth.Name, 'Create')
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
procedure TdwsRTTIExposer.ExposeRTTI(ATypeInfo : Pointer; const options : TdwsRTTIExposerOptions = []);
var
   typ : TRttiType;
begin
   typ:=vRTTIContext.GetType(ATypeInfo);
   if typ is TRttiInstanceType then
      ExposeRTTIClass(TRttiInstanceType(typ), options)
   else if typ is TRttiEnumerationType then
      ExposeRTTIEnumeration(TRttiEnumerationType(typ), options)
   else raise Exception.CreateFmt('Expose unsupported for %s', [typ.ClassName]);
end;

// TypeKindToScriptBaseType
//
class function TdwsRTTIExposer.TypeKindToScriptBaseType(const aType : TTypeKind) : TBaseTypeId;
begin
   case aType of
      tkInteger, tkInt64 :
         Result:=typIntegerID;
      tkChar, tkString, tkUString, tkWChar, tkLString, tkWString :
         Result:=typStringID;
      tkFloat :
         Result:=typFloatID;
      tkVariant :
         Result:=typVariantID;
      tkSet, tkProcedure, tkPointer, tkDynArray, tkInterface, tkRecord, tkArray,
         tkEnumeration, tkClassRef, tkClass, tkMethod : begin
          Result:=typVariantID; // todo, someday maybe...
      end;
      tkUnknown : begin
          Result:=typVariantID; // unsupported
          Assert(False);
      end;
   else
      Result:=typVariantID;
      Assert(False);
   end;
end;

// TypeKindToScriptType
//
class function TdwsRTTIExposer.TypeKindToScriptType(const aType : TTypeKind) : String;
begin
   case aType of
      tkInteger, tkInt64 :
         Result:='Integer';
      tkChar, tkString, tkUString, tkWChar, tkLString, tkWString :
         Result:='String';
      tkFloat :
         Result:='Float';
      tkVariant :
         Result:='Variant';
      tkSet, tkProcedure, tkPointer, tkDynArray, tkInterface, tkRecord, tkArray,
         tkEnumeration, tkClassRef, tkClass, tkMethod : begin
          Result:='Variant'; // todo, someday maybe...
      end;
      tkUnknown : begin
          Result:='Variant'; // unsupported
          Assert(False);
      end;
   else
      Result:='Variant';
      Assert(False);
   end;
end;

// RTTITypeToScriptBaseType
//
class function TdwsRTTIExposer.RTTITypeToScriptBaseType(const aType : TRTTIType) : TBaseTypeId;
begin
   if aType<>nil then
      Result:=TypeKindToScriptBaseType(aType.TypeKind)
   else Result:=typNoneID;
end;

// RTTITypeToScriptType
//
class function TdwsRTTIExposer.RTTITypeToScriptType(const aType : TRTTIType) : String;
begin
   if aType<>nil then
      Result:=TypeKindToScriptType(aType.TypeKind)
   else Result:='';
end;

// ExposeRTTIClass
//
function TdwsRTTIExposer.ExposeRTTIClass(cls : TRttiInstanceType; const options : TdwsRTTIExposerOptions) : TdwsClass;

   function ShouldExpose(item : TRttiMember) : Boolean;
   var
      attrib : TCustomAttribute;
   begin
      Result:=(item.Visibility=mvPublished);
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
   Result:=Classes.Add;
   Result.Name:=dwsPublished.NameOf(cls);
   Result.OnCleanUp:=DoStandardCleanUp;

   helper:=TdwsRTTIHelper.Create(cls);
   Result.HelperObject:=helper;

   scriptConstructor:=(Result.Constructors.Add as TdwsConstructor);
   scriptConstructor.OnEval:=helper.DoStandardCreate;

   for meth in cls.GetMethods do begin
      if ShouldExpose(meth) then begin
         case meth.MethodKind of
            TypInfo.mkProcedure, TypInfo.mkFunction :
               ExposeRTTIMethod(meth, Result, options);
            TypInfo.mkConstructor :
               ExposeRTTIConstructor(meth, Result, options);
         end;
      end;
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
   Result:=(scriptClass.Constructors.Add as TdwsConstructor);
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

// ExposeRTTIMethod
//
function TdwsRTTIExposer.ExposeRTTIMethod(meth : TRttiMethod; scriptClass : TdwsClass;
                                          const options : TdwsRTTIExposerOptions) : TdwsMethod;
var
   param : TRttiParameter;
   helper : TdwsRTTIHelper;
   invoker : TdwsRTTIMethodInvoker;
begin
   Result:=(scriptClass.Methods.Add as TdwsMethod);
   Result.Name:=dwsPublished.NameOf(meth);
   Result.ResultType:=RTTITypeToScriptType(meth.ReturnType);

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
   Result:=(scriptClass.Properties.Add as TdwsProperty);
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
   Result:=(Enumerations.Add as TdwsEnumeration);
   Result.Name:=dwsPublished.NameOf(enum);

   for i:=enum.MinValue to enum.MaxValue do begin
      enumName:=GetEnumName(enum.Handle, i);
      element:=(Result.Elements.Add as TdwsElement);
      element.Name:=enumName;
      element.UserDefValue:=i;
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

// AssignResultFromValue
//
class procedure TdwsRTTIInvoker.AssignResultFromValue(info : TProgramInfo; const value : TValue;
                                                      asType : TBaseTypeId);
begin
   if asType<>typNoneID then begin
      Info.ResultAsVariant:=value.AsVariant;
   end;
end;

// ValueFromParam
//
class function TdwsRTTIInvoker.ValueFromParam(info : TProgramInfo; const paramName : String;
                                              asType : TBaseTypeId) : TValue;
begin
   case asType of
      typIntegerID : Result:=TValue.From<Int64>(info.ValueAsInteger[paramName]);
      typFloatID : Result:=TValue.From<Double>(info.ValueAsFloat[paramName]);
      typStringID : Result:=TValue.From<String>(info.ValueAsString[paramName]);
      typBooleanID : Result:=TValue.From<Boolean>(info.ValueAsBoolean[paramName]);
      typVariantID : Result:=TValue.From<Variant>(info.ValueAsVariant[paramName]);
   else
      Result:=TValue.Empty;
      Assert(False);
   end;
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
   i, n : Integer;
begin
   FMethod:=aMethod;
   methParams:=aMethod.GetParameters;
   n:=Length(methParams);
   SetLength(FTypParams, n);
   SetLength(FNameParams, n);
   for i:=0 to n-1 do begin
      FTypParams[i]:=TdwsUnit.RTTITypeToScriptBaseType(methParams[i].ParamType);
      FNameParams[i]:=methParams[i].Name;
   end;
   FTypResult:=TdwsUnit.RTTITypeToScriptBaseType(aMethod.ReturnType);
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
begin
   PrepareParams(info, params);
   resultValue:=FMethod.Invoke(externalObject, params);
   AssignResultFromValue(info, resultValue, FTypResult);
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
   FTyp:=TdwsUnit.RTTITypeToScriptBaseType(aProperty.PropertyType);
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
   AssignResultFromValue(info, resultValue, FTyp);
end;

end.
