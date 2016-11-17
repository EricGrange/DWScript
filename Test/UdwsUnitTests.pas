unit UdwsUnitTests;

interface

uses
   Classes, SysUtils,
   dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsDataContext, dwsInfo,
   dwsExprList, dwsTokenizer, dwsSymbols, dwsUtils, dwsStack;

type

   TdwsUnitTestsContext = class
      protected
         FCompiler : TDelphiWebScript;
         FUnit : TdwsUnit;
         FMagicVar : String;

         procedure SetupUnit;

      public
         destructor Destroy; override;

         procedure DeclareTestEnumerate;
         procedure DeclareTestSet;
         procedure DeclareTestFuncs;
         procedure DeclareTestClasses;
         procedure DeclareTestVars;
         procedure DeclareTestArrays;
         procedure DeclareTestRecords;
         procedure DeclareTestOperators;

         procedure Func1Eval(Info: TProgramInfo);
         procedure FuncOneEval(Info: TProgramInfo);
         procedure FuncOneDotFiveEval(Info: TProgramInfo);
         procedure FuncTrueEval(Info: TProgramInfo);
         procedure FuncIncEval(Info: TProgramInfo);
         procedure FuncIncNEval(Info: TProgramInfo);
         procedure FuncEnumEval(Info: TProgramInfo);
         procedure FuncSetEval(Info: TProgramInfo);
         procedure FuncVariantEval(Info: TProgramInfo);
         procedure FuncVariantDateEval(Info: TProgramInfo);
         procedure FuncVarEval(Info: TProgramInfo);
         procedure FuncFloatEval(Info: TProgramInfo);
         procedure FuncPointEval(Info: TProgramInfo);
         procedure FuncPointVarParamEval(Info: TProgramInfo);
         procedure FuncPointArrayEval(Info: TProgramInfo);
         procedure FuncClassNameEval(Info: TProgramInfo);
         procedure FuncMetaClassNameEval(Info: TProgramInfo);
         procedure FuncOpenArrayEval(Info: TProgramInfo);
         procedure FuncOverloadIntEval(Info: TProgramInfo);
         procedure FuncOverloadStrEval(Info: TProgramInfo);
         function  FuncFastEval(const args : TExprBaseListExec) : Variant;
         function  FuncFastPointEval(const args : TExprBaseListExec) : Variant;
         procedure ProcCallLevelsEval(Info: TProgramInfo);
         procedure FuncReturnStrings(Info: TProgramInfo);
         procedure FuncReturnStrings2(Info: TProgramInfo);
         procedure FuncReturnVirtCreate(Info: TProgramInfo);
         procedure FuncNil(Info: TProgramInfo);

         procedure ClassConstructor(Info: TProgramInfo; var ExtObject: TObject);
         procedure ClassVirtConstructor(Info: TProgramInfo; var ExtObject: TObject);
         procedure ClassConstructorInit(Info: TProgramInfo; var ExtObject: TObject);
         procedure ClassCleanup(ExternalObject: TObject);
         procedure ClassDestructor(Info: TProgramInfo; ExtObject: TObject);
         procedure MethodPrintEval(Info: TProgramInfo; ExtObject: TObject);
         procedure MethodPrintExternalEval(Info: TProgramInfo; ExtObject: TObject);
         procedure MethodGetIntEval(Info: TProgramInfo; ExtObject: TObject);
         procedure MethodSetIntEval(Info: TProgramInfo; ExtObject: TObject);
         procedure MethodGetArrayIntEval(Info: TProgramInfo; ExtObject: TObject);
         procedure MethodOverloadIntEval(Info: TProgramInfo; ExtObject: TObject);
         procedure MethodOverloadStrEval(Info: TProgramInfo; ExtObject: TObject);

         procedure FuncExceptionEval(Info: TProgramInfo);

         procedure DoReadVar(info: TProgramInfo; var value : Variant);
         procedure DoWriteVar(info: TProgramInfo; const value : Variant);
         procedure DoReadVar42(info: TProgramInfo; var value : Variant);
         procedure DoReadVarIncMagic(info: TProgramInfo; var value : Variant);
         procedure DoReadVarDateTime(info: TProgramInfo; var value : Variant);
   end;


   TdwsUnitTests = class (TTestCase)
      protected
         FContext : TdwsUnitTestsContext;
         FCompiler : TDelphiWebScript;
         FUnit : TdwsUnit;

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure CompilationExecution(execute : Boolean);

      published

         procedure DesignTimeDisplayValues;
         procedure CompiledDescriptions;

         procedure CompilationNormal;
         procedure CompilationWithMapAndSymbols;
         procedure ExecutionNonOptimized;
         procedure ExecutionOptimized;

         procedure DelphiException;
         procedure DelphiExceptionReRaise;
         procedure ListOrdAutoEnum;
         procedure CallFunc;
         procedure CallFuncVarParam;
         procedure CallFuncPointVarParam;
         procedure CallFuncPointArray;
         procedure PredefinedVar;
         procedure VarDateTime;
         procedure AssignTest;
         procedure PredefinedArray;
         procedure PredefinedRecord;
         procedure DynamicArray;
         procedure DynamicArrayResult;
         procedure DynamicArrayResult2;
         procedure ClassPropertyInfo;
         procedure ClassInit;
         procedure DestructorAndExternalObject;
         procedure ExternalObject;
         procedure CustomDestructor;
         procedure Delegates;
         procedure Operators;
         procedure OpenArray;
         procedure CallPrint;
         procedure CreateExternally;
         procedure DeprecatedProp;
         procedure ReservedNameMethod;
         procedure CallInNested;
         procedure OverloadedFunc;
         procedure FastEvalTest;
         procedure ArrayOfObjects;
         procedure FuncVariantTest;
         procedure FuncVariantDateTest;
         procedure FuncNilTest;
         procedure SetTest;
         procedure ClassNameTest;
         procedure VirtCreateFunc;

         procedure ParseNameTests;

         procedure ExplicitUses;

         procedure UnknownUnit;
         procedure CircularUnit;
         procedure DuplicateUnit;

         procedure CallLevels;
   end;

   EDelphiException = class (Exception)
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cFuncsTestsSource =
       'if Func1<>1 then PrintLn(''Func1 failed'');'#13#10
      +'if FuncOne<>''One'' then PrintLn(''FuncOne failed'');'#13#10
      +'if FuncOneDotFive<>1.5 then PrintLn(''FuncOneDotFive failed'');'#13#10
      +'if FuncTrue<>True then PrintLn(''FuncTrue failed'');'#13#10
      +'if FuncEnum<>1 then PrintLn(''FuncEnum default failed'');'#13#10
      +'if FuncEnum(meTen)<>10 then PrintLn(''FuncEnum meTen failed'');'#13#10
      +'if FuncSet([meOne])<>2 then PrintLn(''FuncSet default failed'');'#13#10
      +'if FuncSet([meOne, meTen])<>1026 then PrintLn(''FuncSet default failed'');'#13#10
      +'var i=1; FuncVar(i); if i<>2 then PrintLn(''FuncVar def failed'');'#13#10
      +'FuncVar(i, 10); if i<>12 then PrintLn(''FuncVar 10 failed'');'#13#10
      +'FuncVar(i, i); if i<>24 then PrintLn(''FuncVar i failed'');'#13#10
      +'if FuncFloat(10)<>10.5 then PrintLn(''FuncFloat def failed'');'#13#10
      +'if FuncFloat(5.1, 4.2)<>9.3 then PrintLn(''FuncFloat failed'');'#13#10
      +'if TTestClass.cTest<>''My class const'' then PrintLn(''class const failed'');'#13#10
      ;

type
   TdwsFunctionCracker = class (TdwsFunction)
   end;

   TdwsClassCracker = class (TdwsClass)
   end;

   TdwsPropertyCracker = class (TdwsProperty)
   end;

   TdwsMethodCracker = class (TdwsMethod)
   end;

   TdwsClassConstantCracker = class (TdwsClassConstant)
   end;

   TdwsEnumerationCracker = class (TdwsEnumeration)
   end;

   TdwsOperatorCracker = class (TdwsOperator)
   end;

// ------------------
// ------------------ TdwsUnitTestsContext ------------------
// ------------------

// SetupUnit
//
procedure TdwsUnitTestsContext.SetupUnit;
begin
   FCompiler:=TDelphiWebScript.Create(nil);

   FUnit:=TdwsUnit.Create(nil);
   FUnit.UnitName:='Test';
   FUnit.Script:=FCompiler;

   DeclareTestEnumerate;
   DeclareTestSet;
   DeclareTestRecords;
   DeclareTestClasses;
   DeclareTestVars;
   DeclareTestArrays;
   DeclareTestFuncs;
   DeclareTestOperators;
end;

// Destroy
//
destructor TdwsUnitTestsContext.Destroy;
begin
   inherited;
   FUnit.Free;
   FCompiler.Free;
end;

// DeclareTestEnumerate
//
procedure TdwsUnitTestsContext.DeclareTestEnumerate;
var
   i : Integer;
   enum : TdwsEnumeration;
   elem : TdwsElement;
begin
   enum:=FUnit.Enumerations.Add;
   enum.Name:='TMyEnum';
   elem:=enum.Elements.Add;
   elem.Name:='meOne';
   elem.UserDefValue:=1;
   elem:=enum.Elements.Add;
   elem.Name:='meTen';
   elem.UserDefValue:=10;

   enum:=FUnit.Enumerations.Add;
   enum.Name:='TAutoEnum';
   for i:=1 to 9 do begin
      elem:=enum.Elements.Add;
      elem.Name:='aeVal'+IntToStr(10-i);
   end;
end;

// DeclareTestSet
//
procedure TdwsUnitTestsContext.DeclareTestSet;
var
   s : TdwsSet;
begin
   s:=FUnit.Sets.Add;
   s.Name:='TMyEnums';
   s.BaseType:='TMyEnum';
end;

// DeclareTestFuncs
//
procedure TdwsUnitTestsContext.DeclareTestFuncs;
var
   func : TdwsFunction;
   param : TdwsParameter;
begin
   func:=FUnit.Functions.Add;
   func.Name:='Func1';
   func.ResultType:='Integer';
   func.OnEval:=Func1Eval;

   func:=FUnit.Functions.Add;
   func.Name:='FuncOne';
   func.ResultType:='String';
   func.OnEval:=FuncOneEval;

   func:=FUnit.Functions.Add;
   func.Name:='FuncOneDotFive';
   func.ResultType:='Float';
   func.OnEval:=FuncOneDotFiveEval;

   func:=FUnit.Functions.Add;
   func.Name:='FuncTrue';
   func.ResultType:='Boolean';
   func.OnEval:=FuncTrueEval;

   func:=FUnit.Functions.Add;
   func.Name:='FuncException';
   func.ResultType:='';
   func.OnEval:=FuncExceptionEval;

   func:=FUnit.Functions.Add;
   func.Name:='FuncInc';
   func.ResultType:='Integer';
   func.OnEval:=FuncIncEval;
   param:=func.Parameters.Add;
   param.Name:='v';
   param.DataType:='Integer';

   func:=FUnit.Functions.Add;
   func.Name:='FuncIncN';
   func.ResultType:='Integer';
   func.OnEval:=FuncIncNEval;
   param:=func.Parameters.Add;
   param.Name:='v';
   param.DataType:='Integer';
   param:=func.Parameters.Add;
   param.Name:='n';
   param.DataType:='Integer';
   param.DefaultValue:='1';

   func:=FUnit.Functions.Add;
   func.Name:='FuncEnum';
   func.ResultType:='Integer';
   func.OnEval:=FuncEnumEval;
   param:=func.Parameters.Add;
   param.Name:='e';
   param.DataType:='TMyEnum';
   param.DefaultValue:='meOne';

   func:=FUnit.Functions.Add;
   func.Name:='FuncSet';
   func.ResultType:='Integer';
   func.OnEval:=FuncSetEval;
   param:=func.Parameters.Add;
   param.Name:='s';
   param.DataType:='TMyEnums';

   func:=FUnit.Functions.Add;
   func.Name:='FuncVar';
   func.OnEval:=FuncVarEval;
   param:=func.Parameters.Add;
   param.Name:='i';
   param.DataType:='Integer';
   param.IsVarParam:=True;
   param.IsWritable:=True;
   param:=func.Parameters.Add;
   param.Name:='n';
   param.DataType:='Integer';
   param.DefaultValue:='1';

   func:=FUnit.Functions.Add;
   func.Name:='FuncFloat';
   func.OnEval:=FuncFloatEval;
   func.ResultType:='Float';
   param:=func.Parameters.Add;
   param.Name:='a';
   param.DataType:='Float';
   param:=func.Parameters.Add;
   param.Name:='b';
   param.DataType:='Float';
   param.DefaultValue:='0.5';

   func:=FUnit.Functions.Add;
   func.Name:='FuncVariant';
   func.ResultType:='Variant';
   func.OnEval:=FuncVariantEval;
   param:=func.Parameters.Add;
   param.Name:='v';
   param.DataType:='Variant';

   func:=FUnit.Functions.Add;
   func.Name:='FuncVariantDate';
   func.ResultType:='Variant';
   func.OnEval:=FuncVariantDateEval;

   func:=FUnit.Functions.Add;
   func.Name:='FuncPoint';
   func.ResultType:='TPoint';
   func.OnEval:=FuncPointEval;

   func:=FUnit.Functions.Add;
   func.Name:='FuncPointVarParam';
   param:=func.Parameters.Add;
   param.Name:='pIn';
   param.DataType:='TPoint';
   param:=func.Parameters.Add;
   param.Name:='pOut';
   param.DataType:='TPoint';
   param.IsVarParam:=True;
   func.OnEval:=FuncPointVarParamEval;

   func:=FUnit.Functions.Add;
   func.Name:='FuncPointArray';
   param:=func.Parameters.Add;
   param.Name:='a';
   param.DataType:='TPoints';
   func.OnEval:=FuncPointArrayEval;

   func:=FUnit.Functions.Add;
   func.Name:='FuncClassName';
   func.ResultType:='String';
   param:=func.Parameters.Add;
   param.Name:='obj';
   param.DataType:='TObject';
   param.DefaultValue:=IUnknown(nil);
   func.OnEval:=FuncClassNameEval;

   func:=FUnit.Functions.Add;
   func.Name:='FuncMetaClassName';
   func.ResultType:='String';
   param:=func.Parameters.Add;
   param.Name:='cls';
   param.DataType:='TClass';
   func.OnEval:=FuncMetaClassNameEval;

   func:=FUnit.Functions.Add;
   func.Name:='FuncOpenArray';
   func.ResultType:='String';
   param:=func.Parameters.Add;
   param.Name:='p';
   param.DataType:='array of const';
   func.OnEval:=FuncOpenArrayEval;

   func:=FUnit.Functions.Add;
   func.Name:='FuncOverload';
   func.ResultType:='String';
   func.Overloaded:=True;
   func.Parameters.Add('v', 'Integer');
   func.OnEval:=FuncOverloadIntEval;

   func:=FUnit.Functions.Add;
   func.Name:='FuncOverload';
   func.ResultType:='String';
   func.Overloaded:=True;
   func.Parameters.Add('v', 'String');
   func.OnEval:=FuncOverloadStrEval;

   func:=FUnit.Functions.Add('FuncFast', 'Integer');
   func.Parameters.Add('v', 'String');
   func.OnFastEval:=FuncFastEval;

   func:=FUnit.Functions.Add('FuncFastPoint', 'TPoint');
   func.Parameters.Add('i', 'Integer');
   func.OnFastEval:=FuncFastPointEval;

   func:=FUnit.Functions.Add('FuncStrings', 'TStringArray');
   func.Parameters.Add('i', 'Integer');
   func.OnEval:=FuncReturnStrings;

   func:=FUnit.Functions.Add('FuncStrings2', 'TStringArray');
   func.Parameters.Add('i', 'Integer');
   func.OnEval:=FuncReturnStrings2;

   func:=FUnit.Functions.Add('FuncReturnVirtCreate', 'TTestClass');
   func.OnEval:=FuncReturnVirtCreate;

   func:=FUnit.Functions.Add('FuncNil', 'TObject');
   func.OnEval:=FuncNil;
end;

// DeclareTestClasses
//
procedure TdwsUnitTestsContext.DeclareTestClasses;
var
   cls : TdwsClass;
   cst : TdwsConstructor;
   meth : TdwsMethod;
   fld : TdwsField;
   prop : TdwsProperty;
   param : TdwsParameter;
   constant : TdwsConstant;
   child : TdwsClass;
begin
   cls:=FUnit.Classes.Add;
   cls.Name:='TTestClass';
   cls.OnCleanUp:=ClassCleanup;

   cst:=cls.Constructors.Add;
   cst.Name:='MyCreate';
   param:=cst.Parameters.Add;
   param.DataType:='String';
   param.Name:='v';
   cst.OnEval:=ClassConstructor;

   cst:=cls.Constructors.Add;
   cst.Name:='MyCreateInit';
   cst.OnEval:=ClassConstructorInit;

   cst:=cls.Constructors.Add;
   cst.Name:='VirtCreate';
   cst.Attributes:=[maVirtual];
   cst.OnEval:=ClassVirtConstructor;

   meth:=cls.Methods.Add;
   meth.Name:='MyDestroy';
   meth.Kind:=mkDestructor;
   meth.OnEval:=ClassDestructor;

   meth:=cls.Methods.Add;
   meth.Name:='Print';
   meth.OnEval:=MethodPrintEval;

   meth:=cls.Methods.Add;
   meth.Name:='PrintExternal';
   meth.OnEval:=MethodPrintExternalEval;

   fld:=cls.Fields.Add;
   fld.Name:='FField';
   fld.DataType:='Integer';

   meth:=cls.Methods.Add;
   meth.Name:='GetMyProp';
   meth.ResultType:='Integer';
   meth.OnEval:=MethodGetIntEval;

   meth:=cls.Methods.Add;
   meth.Name:='SetMyProp';
   param:=meth.Parameters.Add;
   param.DataType:='Integer';
   param.Name:='v';
   meth.OnEval:=MethodSetIntEval;

   meth:=cls.Methods.Add;
   meth.Name:='Function';
   meth.ResultType:='Integer';
   meth.OnEval:=MethodGetIntEval;

   meth:=cls.Methods.Add;
   meth.Name:='GetArrayProp';
   meth.ResultType:='Integer';
   param:=meth.Parameters.Add;
   param.DataType:='String';
   param.Name:='v';
   meth.OnEval:=MethodGetArrayIntEval;

   meth:=cls.Methods.Add('MethOverload', 'String');
   meth.Parameters.Add('v', 'Integer');
   meth.Overloaded:=True;
   meth.OnEval:=MethodOverloadIntEval;

   meth:=cls.Methods.Add('MethOverload', 'String');
   meth.Parameters.Add('v', 'String');
   meth.Overloaded:=True;
   meth.OnEval:=MethodOverloadStrEval;

   meth:=cls.Methods.Add;
   meth.Name:='VirtGetArrayProp';
   meth.ResultType:='Integer';
   meth.Overloaded:=True;
   meth.Attributes:=[maVirtual];
   param:=meth.Parameters.Add;
   param.DataType:='String';
   param.Name:='v';
   meth.OnEval:=MethodGetArrayIntEval;

   meth:=cls.Methods.Add('VirtOverload', 'String');
   meth.Parameters.Add('v', 'Integer');
   meth.Overloaded:=True;
   meth.Attributes:=[maVirtual];
   meth.OnEval:=MethodOverloadIntEval;

   meth:=cls.Methods.Add('VirtOverload', 'String');
   meth.Parameters.Add('v', 'String');
   meth.Overloaded:=True;
   meth.Attributes:=[maVirtual];
   meth.OnEval:=MethodOverloadStrEval;

   prop:=cls.Properties.Add;
   prop.Name:='MyReadOnlyProp';
   prop.DataType:='Integer';
   prop.ReadAccess:='GetMyProp';

   prop:=cls.Properties.Add;
   prop.Name:='MyWriteOnlyProp';
   prop.DataType:='Integer';
   prop.WriteAccess:='SetMyProp';

   prop:=cls.Properties.Add;
   prop.Name:='MyReadWriteProp';
   prop.DataType:='Integer';
   prop.ReadAccess:='FField';
   prop.WriteAccess:='FField';

   prop:=cls.Properties.Add;
   prop.Name:='ArrayProp';
   prop.DataType:='Integer';
   prop.ReadAccess:='GetArrayProp';
   param:=prop.Parameters.Add;
   param.DataType:='String';
   param.Name:='v';

   prop:=cls.Properties.Add;
   prop.Name:='DeprecatedProp';
   prop.DataType:='Integer';
   prop.ReadAccess:='FField';
   prop.Deprecated:='Obsolete';

   constant:=cls.Constants.Add;
   constant.Name:='cTest';
   constant.DataType:='String';
   constant.Value:='My class const';

   child:=FUnit.Classes.Add;
   child.Name:='TTestChildClass';
   child.Ancestor:='TTestClass';
   child.OnCleanUp:=ClassCleanup;

   meth:=child.Methods.Add;
   meth.Name:='VirtGetArrayProp';
   meth.ResultType:='Integer';
   param:=meth.Parameters.Add;
   param.DataType:='String';
   param.Name:='v';
   meth.Attributes:=[maOverride];
   meth.OnEval:=MethodGetArrayIntEval;

   meth:=child.Methods.Add('VirtOverload', 'String');
   meth.Parameters.Add('v', 'Integer');
   meth.Overloaded:=True;
   meth.Attributes:=[maOverride];
   meth.OnEval:=MethodOverloadIntEval;

   meth:=child.Methods.Add('VirtOverload', 'String');
   meth.Parameters.Add('v', 'String');
   meth.Attributes:=[maOverride];
   meth.Overloaded:=True;
   meth.OnEval:=MethodOverloadStrEval;
end;

// DeclareTestVars
//
procedure TdwsUnitTestsContext.DeclareTestVars;
var
   v : TdwsGlobal;
begin
   v:=FUnit.Variables.Add;
   v.Name:='xyzVar';
   v.DataType:='String';

   v:=FUnit.Variables.Add;
   v.Name:='magicVar';
   v.DataType:='String';
   v.OnReadVar:=DoReadVar;
   v.OnWriteVar:=DoWriteVar;

   v:=FUnit.Variables.Add;
   v.Name:='magicVarInc';
   v.DataType:='String';
   v.OnReadVar:=DoReadVarIncMagic;

   v:=FUnit.Variables.Add;
   v.Name:='LifeUniverseEverything';
   v.DataType:='Integer';
   v.OnReadVar:=DoReadVar42;

   v:=FUnit.Variables.Add;
   v.Name:='vDateTime';
   v.DataType:='Variant';
   v.OnReadVar:=DoReadVarDateTime;
end;

// DeclareTestArrays
//
procedure TdwsUnitTestsContext.DeclareTestArrays;
var
   a : TdwsArray;
begin
   a:=FUnit.Arrays.Add;
   a.Name:='array_5_10';
   a.DataType:='Integer';
   a.LowBound:=5;
   a.HighBound:=10;

   a:=FUnit.Arrays.Add;
   a.Name:='TPoints';
   a.IsDynamic:=True;
   a.DataType:='TPoint';

   a:=FUnit.Arrays.Add;
   a.Name:='TDynObjects';
   a.IsDynamic:=True;
   a.DataType:='TTestClass';

   a:=FUnit.Arrays.Add;
   a.Name:='TStaticObjects';
   a.LowBound:=0;
   a.HighBound:=1;
   a.DataType:='TTestClass';

   a:=FUnit.Arrays.Add;
   a.Name:='TStringArray';
   a.IsDynamic:=True;
   a.DataType:='String';
end;

// DeclareTestRecords
//
procedure TdwsUnitTestsContext.DeclareTestRecords;
var
   r : TdwsRecord;
   m : TdwsMember;
begin
   r:=FUnit.Records.Add as TdwsRecord;
   r.Name:='TPoint';

   m:=r.Members.Add as TdwsMember;
   m.Name:='X';
   m.DataType:='Integer';

   m:=r.Members.Add as TdwsMember;
   m.Name:='Y';
   m.DataType:='Integer';
end;

// DeclareTestOperators
//
procedure TdwsUnitTestsContext.DeclareTestOperators;
var
   o : TdwsOperator;
begin
   o:=FUnit.Operators.Add;
   o.Operator:=ttCARET;

   o.Params.Add.Name:='Float';
   o.Params.Add.Name:='Float';
   o.ResultType:='Float';

   o.UsesAccess:='FuncFloat';
end;

// Func1Eval
//
procedure TdwsUnitTestsContext.Func1Eval(Info: TProgramInfo);
begin
   Info.ResultAsInteger:=1;
end;

// FuncOneEval
//
procedure TdwsUnitTestsContext.FuncOneEval(Info: TProgramInfo);
begin
   Info.ResultAsString:='One';
end;

// FuncOneDotFiveEval
//
procedure TdwsUnitTestsContext.FuncOneDotFiveEval(Info: TProgramInfo);
begin
   Info.ResultAsFloat:=1.5;
end;

// FuncTrueEval
//
procedure TdwsUnitTestsContext.FuncTrueEval(Info: TProgramInfo);
begin
   Info.ResultAsBoolean:=True;
end;

// FuncIncEval
//
procedure TdwsUnitTestsContext.FuncIncEval(Info: TProgramInfo);
begin
   Info.ResultAsInteger:=Info.ValueAsInteger['v']+1;
end;

// FuncIncNEval
//
procedure TdwsUnitTestsContext.FuncIncNEval(Info: TProgramInfo);
begin
   Info.ResultAsInteger:=Info.ValueAsInteger['v']+Info.ValueAsInteger['n'];
end;

// FuncEnumEval
//
procedure TdwsUnitTestsContext.FuncEnumEval(Info: TProgramInfo);
begin
   Info.ResultAsInteger:=Info.ValueAsInteger['e'];
end;

// FuncVariantEval
//
procedure TdwsUnitTestsContext.FuncVariantEval(Info: TProgramInfo);
begin
   Info.ResultAsVariant:=Info.ParamAsVariant[0];
end;

// FuncVariantDateEval
//
procedure TdwsUnitTestsContext.FuncVariantDateEval(Info: TProgramInfo);
begin
   Info.ResultAsVariant:=Now;
end;

// FuncVarEval
//
procedure TdwsUnitTestsContext.FuncVarEval(Info: TProgramInfo);
begin
   Info.ValueAsInteger['i']:=Info.ParamAsInteger[0]+Info.ParamAsInteger[1];
end;

// FuncFloatEval
//
procedure TdwsUnitTestsContext.FuncFloatEval(Info: TProgramInfo);
begin
   Info.ResultAsFloat:=Info.ParamAsFloat[0]+Info.ValueAsFloat['b'];
end;

// FuncPointEval
//
procedure TdwsUnitTestsContext.FuncPointEval(Info: TProgramInfo);
begin
   Info.Vars['Result'].Member['x'].Value:=12;
   Info.Vars['Result'].Member['y'].Value:=24;
end;

// FuncPointVarParamEval
//
procedure TdwsUnitTestsContext.FuncPointVarParamEval(Info: TProgramInfo);
var
   pIn, pOut : IInfo;
begin
   pIn:=Info.Vars['pIn'];
   pOut:=Info.Vars['pOut'];
   pOut.Member['x'].Value:=pIn.Member['x'].Value+1;
   pOut.Member['y'].Value:=pIn.Member['y'].Value+2;
end;

// FuncPointArrayEval
//
procedure TdwsUnitTestsContext.FuncPointArrayEval(Info: TProgramInfo);
var
   a : IInfo;
   item : IInfo;
begin
   a:=Info.Vars['a'];

   a.Member['length'].Value:=2;

   item:=a.Element([0]);
   item.Member['x'].Value:=1;
   item.Member['y'].Value:=2;

   item:=a.Element([1]);
   item.Member['x'].Value:=3;
   item.Member['y'].Value:=4;
end;

// FuncClassNameEval
//
procedure TdwsUnitTestsContext.FuncClassNameEval(Info: TProgramInfo);
var
   o : IInfo;
begin
   o:=Info.Vars['obj'];
   if o.ScriptObj=nil then
      Info.ResultAsString:=''
   else Info.ResultAsString:=o.ScriptObj.ClassSym.Name;
end;

// FuncMetaClassNameEval
//
procedure TdwsUnitTestsContext.FuncMetaClassNameEval(Info: TProgramInfo);
var
   c : IInfo;
begin
   c:=Info.Vars['cls'];
   Info.ResultAsString:=c.Method['ClassName'].Call.ValueAsString;
end;

// FuncOpenArrayEval
//
procedure TdwsUnitTestsContext.FuncOpenArrayEval(Info: TProgramInfo);
var
   p : IInfo;
   r : String;
   i : Integer;
begin
   p:=Info.Vars['p'];
   r:=IntToStr(p.Member['length'].ValueAsInteger)+':';
   for i:=p.Member['low'].ValueAsInteger to p.Member['high'].ValueAsInteger do begin
      if i>0 then r:=r+',';
      r:=r+p.Element([i]).ValueAsString;
   end;
   Info.ResultAsString:=r;
end;

// FuncOverloadIntEval
//
procedure TdwsUnitTestsContext.FuncOverloadIntEval(Info: TProgramInfo);
begin
   Info.ResultAsString:=IntToStr(Info.ParamAsInteger[0]*2);
end;

// FuncOverloadStrEval
//
procedure TdwsUnitTestsContext.FuncOverloadStrEval(Info: TProgramInfo);
begin
   Info.ResultAsString:='('+Info.ParamAsString[0]+')';
end;

// FuncFastEval
//
function TdwsUnitTestsContext.FuncFastEval(const args : TExprBaseListExec) : Variant;
begin
   Result:=Length(args.AsString[0]);
end;

// FuncFastPointEval
//
function TdwsUnitTestsContext.FuncFastPointEval(const args : TExprBaseListExec) : Variant;
var
   rec : TData;
begin
   SetLength(rec, 2);
   rec[0]:=args.AsInteger[0];
   rec[1]:=rec[0]+1;
   Result:=IDataContext(args.Exec.Stack.CreateDataContext(rec, 0));
end;

// ProcCallLevelsEval
//
procedure TdwsUnitTestsContext.ProcCallLevelsEval(Info: TProgramInfo);
begin
   Info.Execution.ProgramInfo.Func['CallMe'].Call([]);
end;

// FuncReturnStrings
//
procedure TdwsUnitTestsContext.FuncReturnStrings(Info: TProgramInfo);
var
   result : IInfo;
   i : Integer;
begin
   result:=Info.ResultVars;
   i:=Info.ParamAsInteger[0];
   result.Member['Count'].ValueAsInteger:=i;
   while i>0 do begin
      Dec(i);
      result.Element([i]).ValueAsString:=IntToStr(i);
   end;
end;

// FuncReturnStrings2
//
procedure TdwsUnitTestsContext.FuncReturnStrings2(Info: TProgramInfo);
var
   a : TStringDynArray;
   i, n : Integer;
begin
   n:=Info.ParamAsInteger[0];
   SetLength(a, n);
   for i:=0 to n-1 do
      a[i]:=IntToStr(i*2);
   Info.ResultAsStringArray:=a;
end;

// FuncReturnVirtCreate
//
procedure TdwsUnitTestsContext.FuncReturnVirtCreate(Info: TProgramInfo);
begin
   Info.ResultAsVariant := Info.Vars['TTestClass'].GetConstructor('VirtCreate', Pointer(-1)).Call.Value;

   end;
// FuncSetEval
//
procedure TdwsUnitTestsContext.FuncSetEval(Info: TProgramInfo);
begin
   Info.ResultAsInteger:=Info.ValueAsInteger['s'];
end;

// FuncNil
//
procedure TdwsUnitTestsContext.FuncNil(Info: TProgramInfo);
begin
   Info.ResultAsVariant:=IScriptObj(nil);
end;

// ClassConstructor
//
procedure TdwsUnitTestsContext.ClassConstructor(Info: TProgramInfo; var ExtObject: TObject);
begin
   FMagicVar:=Info.ParamAsString[0];
   ExtObject:=TObject.Create;
end;

// ClassVirtConstructor
//
procedure TdwsUnitTestsContext.ClassVirtConstructor(Info: TProgramInfo; var ExtObject: TObject);
begin
   FMagicVar:=IntToStr(NativeInt(ExtObject));
end;

// ClassConstructorInit
//
procedure TdwsUnitTestsContext.ClassConstructorInit(Info: TProgramInfo; var ExtObject: TObject);
begin
   ExtObject:=TObject.Create;
   Info.ValueAsInteger['FField']:=789456;
end;

// ClassCleanup
//
procedure TdwsUnitTestsContext.ClassCleanup(ExternalObject: TObject);
begin
   if ExternalObject<>Pointer(-1) then begin
      FMagicVar:='cleaned up';
      ExternalObject.Free;
   end;
end;

// ClassDestructor
//
procedure TdwsUnitTestsContext.ClassDestructor(Info: TProgramInfo; ExtObject: TObject);
begin
   Info.Execution.Result.AddString('my destructor'#13#10);
end;

// MethodPrintEval
//
procedure TdwsUnitTestsContext.MethodPrintEval(Info: TProgramInfo; ExtObject: TObject);
begin
   Info.Execution.Result.AddString(FMagicVar+#13#10);
end;

// MethodPrintExternalEval
//
procedure TdwsUnitTestsContext.MethodPrintExternalEval(Info: TProgramInfo; ExtObject: TObject);
begin
   Info.Execution.Result.AddString(ExtObject.ToString+#13#10);
end;

// MethodGetIntEval
//
procedure TdwsUnitTestsContext.MethodGetIntEval(Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=Info.ValueAsInteger['FField']*10;
end;

// MethodSetIntEval
//
procedure TdwsUnitTestsContext.MethodSetIntEval(Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ValueAsInteger['FField']:=Info.ValueAsInteger['v'] div 10;
end;

// MethodGetArrayIntEval
//
procedure TdwsUnitTestsContext.MethodGetArrayIntEval(Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=StrToInt(Info.ValueAsString['v'])*2;
end;

// MethodOverloadIntEval
//
procedure TdwsUnitTestsContext.MethodOverloadIntEval(Info: TProgramInfo; ExtObject: TObject);
begin
   FuncOverloadIntEval(Info);
end;

// MethodOverloadStrEval
//
procedure TdwsUnitTestsContext.MethodOverloadStrEval(Info: TProgramInfo; ExtObject: TObject);
begin
   FuncOverloadStrEval(Info);
end;

// FuncExceptionEval
//
procedure TdwsUnitTestsContext.FuncExceptionEval(Info: TProgramInfo);
begin
   raise EDelphiException.Create('Hello, Delphi Exception here!');
end;

// DoReadVar
//
procedure TdwsUnitTestsContext.DoReadVar(info: TProgramInfo; var value : Variant);
begin
   value:=FMagicVar;
end;

// DoWriteVar
//
procedure TdwsUnitTestsContext.DoWriteVar(info: TProgramInfo; const value : Variant);
begin
   FMagicVar:=value;
end;

// DoReadVar42
//
procedure TdwsUnitTestsContext.DoReadVar42(info: TProgramInfo; var value : Variant);
begin
   value:=Int64(42);
end;

// DoReadVarIncMagic
//
procedure TdwsUnitTestsContext.DoReadVarIncMagic(info: TProgramInfo; var value : Variant);
begin
   FMagicVar:=FMagicVar+'+1';
   value:=FMagicVar;
end;

// DoReadVarDateTime
//
procedure TdwsUnitTestsContext.DoReadVarDateTime(info: TProgramInfo; var value : Variant);
var
   t : TDateTime;
begin
   t:=Now;
   value:=t;
end;

// ------------------
// ------------------ TdwsUnitTests ------------------
// ------------------

// SetUp
//
procedure TdwsUnitTests.SetUp;
begin
   if FContext=nil then begin
      FContext:=TdwsUnitTestsContext.Create;
      FContext.SetupUnit;
   end;
   FCompiler:=FContext.FCompiler;
   FUnit:=FContext.FUnit;
end;

// TearDown
//
procedure TdwsUnitTests.TearDown;
begin
   FCompiler:=nil;
   FUnit:=nil;
   FreeAndNil(FContext);
end;

// CompilationExecution
//
procedure TdwsUnitTests.CompilationExecution(execute : Boolean);
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=FCompiler.Compile(cFuncsTestsSource);

   CheckEquals('', prog.Msgs.AsInfo, 'FuncsTest compile');
   if execute then begin
      exec:=prog.Execute;
      CheckEquals('', exec.Result.ToString, 'FuncsTest result');
      CheckEquals('', exec.Msgs.AsInfo, 'FuncsTest Msgs');
   end;
end;

// DesignTimeDisplayValues
//
procedure TdwsUnitTests.DesignTimeDisplayValues;

   function FuncByName(const aName : UnicodeString) : TdwsFunctionCracker;
   var
      i : Integer;
   begin
      i:=FUnit.Functions.IndexOf(aName);
      Result:=TdwsFunctionCracker(FUnit.Functions.Items[i] as TdwsFunction);
   end;

   function ClassByName(const aName : UnicodeString) : TdwsClassCracker;
   var
      i : Integer;
   begin
      i:=FUnit.Classes.IndexOf(aName);
      Result:=TdwsClassCracker(FUnit.Classes.Items[i] as TdwsClass);
   end;

   function PropertyByName(cls : TdwsClass; const aName : UnicodeString) : TdwsPropertyCracker;
   var
      i : Integer;
   begin
      i:=cls.Properties.IndexOf(aName);
      Result:=TdwsPropertyCracker(cls.Properties.Items[i] as TdwsProperty);
   end;

   function MethodByName(cls : TdwsClass; const aName : UnicodeString) : TdwsMethodCracker;
   var
      i : Integer;
   begin
      i:=cls.Methods.IndexOf(aName);
      Result:=TdwsMethodCracker(cls.Methods.Items[i] as TdwsMethod);
   end;

   function ConstByName(cls : TdwsClass; const aName : UnicodeString) : TdwsClassConstantCracker;
   var
      i : Integer;
   begin
      i:=cls.Constants.IndexOf(aName);
      Result:=TdwsClassConstantCracker(cls.Constants.Items[i] as TdwsConstant);
   end;

   function EnumByName(const aName : UnicodeString) : TdwsEnumerationCracker;
   var
      i : Integer;
   begin
      i:=FUnit.Enumerations.IndexOf(aName);
      Result:=TdwsEnumerationCracker(FUnit.Enumerations.Items[i] as TdwsEnumeration);
   end;

var
   cls : TdwsClassCracker;
begin
   CheckEquals('function Func1 : Integer;', FuncByName('Func1').GetDisplayName);
   CheckEquals('function FuncOne : String;', FuncByName('FuncOne').GetDisplayName);
   CheckEquals('function FuncOneDotFive : Float;', FuncByName('FuncOneDotFive').GetDisplayName);
   CheckEquals('function FuncTrue : Boolean;', FuncByName('FuncTrue').GetDisplayName);
   CheckEquals('procedure FuncException;', FuncByName('FuncException').GetDisplayName);
   CheckEquals('function FuncInc(v : Integer) : Integer;', FuncByName('FuncInc').GetDisplayName);

   CheckEquals('TMyEnum = (meOne = 1, meTen = 10);', EnumByName('TMyEnum').GetDisplayName);
   CheckEquals('TAutoEnum = (aeVal9, aeVal8, aeVal7, aeVal6, aeVal5, aeVal4, aeVal3, aeVal2, aeVal1);',
               EnumByName('TAutoEnum').GetDisplayName);

   cls:=ClassByName('TTestClass');
   CheckEquals('TTestClass (TObject)', cls.GetDisplayName);
   CheckEquals('public property MyReadOnlyProp: Integer read GetMyProp;', PropertyByName(cls, 'MyReadOnlyProp').GetDisplayName);
   CheckEquals('public function GetMyProp : Integer;', MethodByName(cls, 'GetMyProp').GetDisplayName);
   CheckEquals('public procedure SetMyProp(v : Integer);', MethodByName(cls, 'SetMyProp').GetDisplayName);
   CheckEquals('public property ArrayProp[v : String]: Integer read GetArrayProp;', PropertyByName(cls, 'ArrayProp').GetDisplayName);
   CheckEquals('public property MyReadOnlyProp: Integer read GetMyProp;', PropertyByName(cls, 'MyReadOnlyProp').GetDisplayName);
   CheckEquals('public property MyWriteOnlyProp: Integer write SetMyProp;', PropertyByName(cls, 'MyWriteOnlyProp').GetDisplayName);
   CheckEquals('public const cTest: String = ''My class const'';', ConstByName(cls, 'cTest').GetDisplayName);

   CheckEquals('operator ^ (Float, Float) : Float uses FuncFloat', TdwsOperatorCracker(FUnit.Operators.Items[0]).GetDisplayName);
end;

// CompiledDescriptions
//
procedure TdwsUnitTests.CompiledDescriptions;
var
   prog : IdwsProgram;
   sym : TSymbol;
   symClass : TClassSymbol;
begin
   prog:=FCompiler.Compile('');

   sym:=prog.Table.FindSymbol('Func1', cvMagic);
   CheckEquals('function Func1(): Integer', sym.Description);
   CheckEquals('function Func1: Integer', sym.Caption);
   sym:=prog.Table.FindSymbol('FuncOne', cvMagic);
   CheckEquals('function FuncOne(): String', sym.Description);
   sym:=prog.Table.FindSymbol('FuncOneDotFive', cvMagic);
   CheckEquals('function FuncOneDotFive(): Float', sym.Description);
   sym:=prog.Table.FindSymbol('FuncTrue', cvMagic);
   CheckEquals('function FuncTrue(): Boolean', sym.Description);
   sym:=prog.Table.FindSymbol('FuncException', cvMagic);
   CheckEquals('procedure FuncException()', sym.Description);
   sym:=prog.Table.FindSymbol('FuncInc', cvMagic);
   CheckEquals('function FuncInc(v: Integer): Integer', sym.Description);
   CheckEquals('function FuncInc(Integer): Integer', sym.Caption);
   sym:=prog.Table.FindSymbol('FuncIncN', cvMagic);
   CheckEquals('function FuncIncN(v: Integer; n: Integer = 1): Integer', sym.Description);
   CheckEquals('function FuncIncN(Integer, Integer): Integer', sym.Caption);
   sym:=prog.Table.FindSymbol('FuncClassName', cvMagic);
   CheckEquals('function FuncClassName(obj: TObject = nil): String', sym.Description);
   CheckEquals('function FuncClassName(TObject): String', sym.Caption);
   sym:=prog.Table.FindSymbol('FuncMetaClassName', cvMagic);
   CheckEquals('function FuncMetaClassName(cls: TClass): String', sym.Description);
   CheckEquals('function FuncMetaClassName(TClass): String', sym.Caption);

   sym:=prog.Table.FindSymbol('TAutoEnum', cvMagic);
   CheckEquals('(aeVal9, aeVal8, aeVal7, aeVal6, aeVal5, aeVal4, aeVal3, aeVal2, aeVal1)', sym.Description);

   symClass:=prog.Table.FindSymbol('TTestClass', cvMagic) as TClassSymbol;
   sym:=symClass.Members.FindLocal('MyReadOnlyProp');
   CheckEquals('property MyReadOnlyProp: Integer read GetMyProp', sym.Description);
   CheckEquals('property MyReadOnlyProp: Integer read GetMyProp', sym.Caption);
   sym:=symClass.Members.FindLocal('GetMyProp');
   CheckEquals('function GetMyProp(): Integer', sym.Description);
   sym:=symClass.Members.FindLocal('SetMyProp');
   CheckEquals('procedure SetMyProp(v: Integer)', sym.Description);
   sym:=symClass.Members.FindLocal('ArrayProp');
   CheckEquals('property ArrayProp[v: String]: Integer read GetArrayProp', sym.Description);
   CheckEquals('property ArrayProp[v: String]: Integer read GetArrayProp', sym.Caption);
end;

// CompilationNormal
//
procedure TdwsUnitTests.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   CompilationExecution(False);
end;

// CompilationWithMapAndSymbols
//
procedure TdwsUnitTests.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=[coSymbolDictionary, coContextMap, coAssertions];
   CompilationExecution(False);
end;

// ExecutionNonOptimized
//
procedure TdwsUnitTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coAssertions];
   CompilationExecution(True);
end;

// ExecutionOptimized
//
procedure TdwsUnitTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize, coAssertions];
   CompilationExecution(True);
end;

// DelphiException
//
procedure TdwsUnitTests.DelphiException;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=FCompiler.Compile('FuncException;');

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');
   exec:=prog.Execute;
   CheckEquals('Runtime Error: Hello, Delphi Exception here! in FuncException [line: 1, column: 1]'#13#10,
               exec.Msgs.AsInfo, 'Execute Msgs');
end;

// DelphiExceptionReRaise
//
procedure TdwsUnitTests.DelphiExceptionReRaise;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=FCompiler.Compile( 'try'#13#10
                           +#9'FuncException;'#13#10
                           +'except'#13#10
                           +#9'raise;'#13#10
                           +'end;'#13#10
                           );
   CheckEquals('', prog.Msgs.AsInfo, 'Compile');
   exec:=prog.Execute;
   CheckEquals('Runtime Error: Hello, Delphi Exception here! in FuncException [line: 2, column: 2]'#13#10,
               exec.Msgs.AsInfo, 'Execute Msgs');
end;

// ListOrdAutoEnum
//
procedure TdwsUnitTests.ListOrdAutoEnum;
var
   i : Integer;
   script : String;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   script:='';
   for i:=1 to 9 do begin
      script:=script+'Print(Ord(aeVal'+IntToStr(i)+'));'#13#10;
   end;
   prog:=FCompiler.Compile(script);

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');
   exec:=prog.Execute;
   CheckEquals('876543210', exec.Result.ToString, 'Enums Ord');
end;

// ParseNameTests
//
procedure TdwsUnitTests.ParseNameTests;
var
   func: TdwsFunction;
   cls: TdwsClass;
   meth: TdwsMethod;
   fld: TdwsField;
   prty: TdwsProperty;
begin
   FUnit.ParseName:=pnAlways;

   // test parsing name of a simple function
   func := FUnit.Functions.Add;
   try
      func.Name := 'TestRetValue: String;';
      CheckEquals('TestRetValue', func.Name,
         'Check function name');
      CheckEquals('String', func.ResultType,
         'Check function result type');
   finally
      FUnit.Functions.Delete(func.Index);
   end;

   // test parsing name of a simple function (without ending semicolon)
   func := FUnit.Functions.Add;
   try
      func.Name := 'TestRetValue: Integer';
      CheckEquals('TestRetValue', func.Name,
         'Check function name');
      CheckEquals('Integer', func.ResultType,
         'Check function result type');
   finally
      FUnit.Functions.Delete(func.Index);
   end;

   // test parsing name of a simple procedure with a var parameter
   func := FUnit.Functions.Add;
   try
      func.Name := 'TestParam(var a: Integer)';
      CheckEquals('TestParam', func.Name,
         'Check function name');
      CheckEquals('a', TdwsParameter(func.Parameters.Items[0]).Name,
         'Check function parameter name');
      CheckEquals(True, TdwsParameter(func.Parameters.Items[0]).IsVarParam,
         'Check function parameter is var');
      CheckEquals(True, TdwsParameter(func.Parameters.Items[0]).IsWritable,
         'Check function parameter is writable');
      CheckEquals('Integer', TdwsParameter(func.Parameters.Items[0]).DataType,
         'Check function parameter data type');
   finally
      FUnit.Functions.Delete(func.Index);
   end;

   // test parsing name of a procedure with a default value
   func := FUnit.Functions.Add;
   try
      func.Name := 'TestParam(a: String = ''Test'');';
      CheckEquals('TestParam', func.Name,
         'Check function name');
      CheckEquals('a', TdwsParameter(func.Parameters.Items[0]).Name,
         'Check function parameter name');
      CheckEquals('String', TdwsParameter(func.Parameters.Items[0]).DataType,
         'Check function parameter data type');
      CheckEquals('Test', TdwsParameter(func.Parameters.Items[0]).DefaultValue,
         'Check function parameter default value');
   finally
      FUnit.Functions.Delete(func.Index);
   end;

   // test parsing name of a procedure with two parameters (one const param.)
   func := FUnit.Functions.Add;
   try
      func.Name := 'TestSeveralParam(a: Float = 2.5; const b: Integer);';
      CheckEquals('TestSeveralParam', func.Name,
         'Check function name');
      CheckEquals('a', TdwsParameter(func.Parameters.Items[0]).Name,
         'Check function parameter data name');
      CheckEquals('Float', TdwsParameter(func.Parameters.Items[0]).DataType,
         'Check function parameter data type');
      CheckEquals(2.5, TdwsParameter(func.Parameters.Items[0]).DefaultValue,
         'Check function parameter default value');
      CheckEquals('b', TdwsParameter(func.Parameters.Items[1]).Name,
         'Check function parameter name');
      CheckEquals(True, TdwsParameter(func.Parameters.Items[1]).IsVarParam,
         'Check function parameter is var');
      CheckEquals(False, TdwsParameter(func.Parameters.Items[1]).IsWritable,
         'Check function parameter is writable');
      CheckEquals('Integer', TdwsParameter(func.Parameters.Items[1]).DataType,
         'Check function parameter data type');
   finally
      FUnit.Functions.Delete(func.Index);
   end;

   // test parsing name of a procedure with empty brackets and semicolon
   func := FUnit.Functions.Add;
   try
      func.Name := 'TestNone();';
      CheckEquals('TestNone', func.Name, 'Check function name');
   finally
      FUnit.Functions.Delete(func.Index);
   end;

   // test reserved word
   func := FUnit.Functions.Add;
   try
      func.Name := 'function: Integer';
      // CheckEquals('', func.Name, 'Check function name');
      // CheckEquals('Integer', func.ResultType, 'Check function result type');
   finally
      FUnit.Functions.Delete(func.Index);
   end;

   // select class that is used for testing
   cls := TdwsClass(FUnit.Classes.Items[0]);

   // test parsing name of a simple method (function)
   meth := cls.Methods.Add;
   try
      meth.Name := 'TestRetValue: String;';
      CheckEquals('TestRetValue', meth.Name, 'Check method name');
      CheckEquals('String', meth.ResultType, 'Check method result type');
   finally
      cls.Methods.Delete(meth.Index);
   end;

   cls := TdwsClass(FUnit.Classes.Items[0]);
   meth := cls.Methods.Add;
   try
      // test parsing name of a method (procedure)
      meth.Name := 'TestParams(const a: Integer = 2; b: ' + cls.Name + ');';
      CheckEquals(True, TdwsParameter(meth.Parameters.Items[0]).IsVarParam,
         'Check parameter is var');
      CheckEquals(False, TdwsParameter(meth.Parameters.Items[0]).IsWritable,
         'Check parameter is writable');
      CheckEquals('a', TdwsParameter(meth.Parameters.Items[0]).Name,
         'Check parameter name');
      CheckEquals('Integer', TdwsParameter(meth.Parameters.Items[0]).DataType,
         'Check parameter data type');
      CheckEquals(2, TdwsParameter(meth.Parameters.Items[0]).DefaultValue,
         'Check parameter default value');
      CheckEquals('b', TdwsParameter(meth.Parameters.Items[1]).Name,
         'Check parameter name');
      CheckEquals(cls.Name, TdwsParameter(meth.Parameters.Items[1]).DataType,
         'Check parameter data type');

      // test parsing name of a parameter
      TdwsParameter(meth.Parameters.Items[1]).Name := 'a: Integer = 2';
      CheckEquals('a', TdwsParameter(meth.Parameters.Items[1]).Name,
         'Check parameter name');
      CheckEquals('Integer', TdwsParameter(meth.Parameters.Items[1]).DataType,
         'Check parameter data type');
      CheckEquals(2, TdwsParameter(meth.Parameters.Items[1]).DefaultValue,
         'Check parameter default value');
   finally
     cls.Methods.Delete(meth.Index);
   end;

   // test parsing name of a field
   fld := cls.Fields.Add;
   try
      fld.Name := 'public field: Integer = 10;';
      CheckEquals('field', fld.Name, 'Check field name');
      CheckEquals('Integer', fld.DataType, 'Check field data type');
      CheckEquals(Integer(cvPublic), Integer(fld.Visibility), 'Check field visibility');
      CheckEquals(10, fld.DefaultValue, 'Check field default value');
   finally
      cls.Fields.Delete(fld.Index);
   end;

   // test parsing name of another field
   fld := cls.Fields.Add;
   try
      fld.Name := 'str: String = ''Test'';';
      CheckEquals('str', fld.Name, 'Check field name');
      CheckEquals('String', fld.DataType, 'Check field data type');
      CheckEquals('Test', fld.DefaultValue, 'Check field default value');
   finally
      cls.Fields.Delete(fld.Index);
   end;

   // test parsing name of a property
   prty := cls.Properties.Add;
   try
      prty.Name := 'public Test: Integer read GetTest write SetTest;';
      CheckEquals('Test', prty.Name, 'Check property name');
      CheckEquals('Integer', prty.DataType, 'Check property data type');
      CheckEquals(Integer(cvPublic), Integer(prty.Visibility), 'Check property visibility');
      CheckEquals('GetTest', prty.ReadAccess, 'Check property read access');
      CheckEquals('SetTest', prty.WriteAccess, 'Check property write access');
      CheckEquals(False, prty.IsDefault);
   finally
      cls.Properties.Delete(prty.Index);
   end;

   // test parsing name of a property
   prty := cls.Properties.Add;
   try
      prty.Name := 'TestDef[Index: Integer]: String read GetTest write SetTest; default;';
      CheckEquals('TestDef', prty.Name, 'Check property name');
      CheckEquals('String', prty.DataType, 'Check property data type');
      CheckEquals('GetTest', prty.ReadAccess, 'Check property read access');
      CheckEquals('SetTest', prty.WriteAccess, 'Check property write access');
      CheckEquals(True, prty.IsDefault, 'Check property default');
   finally
      cls.Properties.Delete(prty.Index);
   end;

   // test parsing name of a property
   prty := cls.Properties.Add;
   try
      prty.Name := 'TestDef: Float Index 0 read GetTest write SetTest;';
      CheckEquals('TestDef', prty.Name);
      CheckEquals(0, prty.IndexValue, 'Check index value');
      CheckEquals('Integer', prty.IndexType, 'Check index type');
      CheckEquals('Float', prty.DataType, 'Check property data type');
      CheckEquals('GetTest', prty.ReadAccess, 'Check property read access');
      CheckEquals('SetTest', prty.WriteAccess, 'Check property write access');
   finally
      cls.Properties.Delete(prty.Index);
   end;
end;

// CallFunc
//
procedure TdwsUnitTests.CallFunc;
var
   prog : IdwsProgram;
   funcInfo, funcResult : IInfo;
   exec : IdwsProgramExecution;
begin
   prog:=FCompiler.Compile( 'function Hello(name : String) : String;'
                           +'begin'
                           +'   Result:=''Hello ''+name;'
                           +'end;');

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   exec:=prog.BeginNewExecution;
   try
      funcInfo:=exec.Info.Func['Func1'];
      CheckEquals(1, funcInfo.Call.Value, 'Func1 call');

      funcInfo:=exec.Info.Func['FuncOne'];
      CheckEquals('One', funcInfo.Call.Value, 'FuncOne call');

      funcInfo:=exec.Info.Func['FuncPoint'];
      funcResult:=funcInfo.Call;
      CheckEquals('12,24', Format('%d,%d', [funcResult.Member['x'].ValueAsInteger,
                                            funcResult.Member['y'].ValueAsInteger]),
                  'FuncPoint call 1');
      funcResult:=funcInfo.Call([]);
      CheckEquals('12,24', Format('%d,%d', [funcResult.Member['x'].ValueAsInteger,
                                            funcResult.Member['y'].ValueAsInteger]),
                  'FuncPoint call 2');

      funcInfo:=exec.Info.Func['Hello'];
      CheckEquals('Hello world', funcInfo.Call(['world']).Value, 'Hello world');
   finally
      exec.EndProgram;
   end;
end;

// CallFuncVarParam
//
procedure TdwsUnitTests.CallFuncVarParam;
var
   prog : IdwsProgram;
   funcInfo : IInfo;
   exec : IdwsProgramExecution;
   paramString : String;
begin
   prog:=FCompiler.Compile( 'function Hello(var name : String) : String;'
                           +'begin'
                           +'   Result:=''was ''+name;'
                           +'   name:=''world'';'
                           +'end;');

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   exec:=prog.BeginNewExecution;
   try
      funcInfo:=exec.Info.Func['FuncVar'];
      funcInfo.Parameter['i'].Value:=10;
      funcInfo.Parameter['n'].Value:=3;
      CheckEquals(10, funcInfo.Parameter['i'].Value, 'FuncVar before call');
      funcInfo.Call;
      CheckEquals(13, funcInfo.Parameter['i'].Value, 'FuncVar after call');

      paramString:='Eric';
      funcInfo:=exec.Info.Func['Hello'];
      funcInfo.Parameter['name'].Value:=paramString;
      CheckEquals('was Eric', funcInfo.Call.Value, 'Hello Eric result');
      paramString:=funcInfo.Parameter['name'].ValueAsString;
      CheckEquals('world', paramString, 'Hello Eric var');
   finally
      exec.EndProgram;
   end;
end;

// CallFuncPointVarParam
//
procedure TdwsUnitTests.CallFuncPointVarParam;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=FCompiler.Compile( 'var p1, p2 : TPoint;'
                           +'p1.X:=10; p1.Y:=20;'
                           +'FuncPointVarParam(p1, p2);'
                           +'PrintLn(p2.X);'
                           +'PrintLn(p2.Y);');

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   exec:=prog.Execute;

   CheckEquals('11'#13#10'22'#13#10, exec.Result.ToString);
end;

// CallFuncPointArray
//
procedure TdwsUnitTests.CallFuncPointArray;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=FCompiler.Compile( 'var a : TPoints;'
                           +'FuncPointArray(a);'
                           +'var i : Integer;'
                           +'for i:=0 to a.High do'
                           +'   PrintLn(IntToStr(a[i].x)+","+IntToStr(a[i].y));');

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   exec:=prog.Execute;

   CheckEquals('1,2'#13#10'3,4'#13#10, exec.Result.ToString);
end;

// PredefinedVar
//
procedure TdwsUnitTests.PredefinedVar;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=FCompiler.Compile( 'PrintLn(xyzVar); xyzVar:=''XYZ''; PrintLn(xyzVar);'#13#10
                           +'PrintLn(magicVar); magicVar:=''MAGIC''; PrintLn(magicVar);'#13#10
                           +'PrintLn(magicVarInc);'#13#10
                           +'PrintLn(LifeUniverseEverything);'#13#10
                           );

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   exec:=prog.BeginNewExecution;
   try
      exec.Info.ValueAsString['xyzVar']:='xyz';
      FContext.FMagicVar:='magic';

      exec.RunProgram(0);

      CheckEquals( 'xyz'#13#10'XYZ'#13#10
                  +'magic'#13#10'MAGIC'#13#10
                  +'MAGIC+1'#13#10
                  +'42'#13#10, exec.Result.ToString, 'Result');
      CheckEquals('XYZ', exec.Info.ValueAsString['xyzVar'], 'xyz var value');
      CheckEquals('MAGIC+1', FContext.FMagicVar, 'magic var value');
      CheckEquals(42, exec.Info.ValueAsInteger['LifeUniverseEverything'], '42 var value');
   finally
      exec.EndProgram;
   end;
end;

// VarDateTime
//
procedure TdwsUnitTests.VarDateTime;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=FCompiler.Compile( 'var t := Now;'#13#10
                           +'PrintLn(Round(t-vDateTime));'#13#10
                           +'if vDateTime>t+1 then PrintLn("bug1");'#13#10
                           +'if vDateTime<t-1 then PrintLn("bug2");'#13#10
                           +'var v := vDateTime;'#13#10
                           +'PrintLn(Round(t-v));'#13#10
                           +'if v>t+1 then PrintLn("bug3");'#13#10
                           +'if v<t-1 then PrintLn("bug4");'#13#10
                           );

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   exec:=prog.Execute;

   CheckEquals('0'#13#10'0'#13#10, exec.Result.ToString, 'Result');
end;

// AssignTest
//
procedure TdwsUnitTests.AssignTest;
var
   otherUnit : TdwsUnit;
begin
   otherUnit:=TdwsUnit.Create(nil);
   try
      otherUnit.Arrays:=FUnit.Arrays;
      otherUnit.Classes:=FUnit.Classes;
      otherUnit.Constants:=FUnit.Constants;
      otherUnit.Enumerations:=FUnit.Enumerations;
      otherUnit.Forwards:=FUnit.Forwards;
      otherUnit.Functions:=FUnit.Functions;
      otherUnit.Instances:=FUnit.Instances;
      otherUnit.Records:=FUnit.Records;
      otherUnit.Synonyms:=FUnit.Synonyms;
      otherUnit.Variables:=FUnit.Variables;
      CheckTrue(True, '');
   finally
      otherUnit.Free;
   end;
end;

// PredefinedArray
//
procedure TdwsUnitTests.PredefinedArray;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   i : Integer;
   a : IInfo;
begin
   prog:=FCompiler.Compile( 'var i : Integer;'#13#10
                           +'var a : array_5_10;'#13#10
                           +'for i:=Low(array_5_10) to High(a) do Print(a[i]);');

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   exec:=prog.BeginNewExecution;
   try
      a:=exec.Info.Vars['a'];
      for i:=a.Member['low'].Value to a.Member['high'].Value do
         a.Element([i]).Value:=100+i;
      a:=nil;

      exec.RunProgram(0);

      CheckEquals( '105106107108109110', exec.Result.ToString, 'Result');
   finally
      exec.EndProgram;
   end;
end;

// PredefinedRecord
//
procedure TdwsUnitTests.PredefinedRecord;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   p : IInfo;
begin
   prog:=FCompiler.Compile( 'var p : TPoint;'#13#10
                           +'Print(Format(''%d, %d'', [p.X, p.Y]));'#13#10
                           +'p.X:=p.Y; p.Y:=789;'#13#10
                           );

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   exec:=prog.BeginNewExecution;
   try
      p:=exec.Info.Vars['p'];
      CheckEquals('X,Y', p.FieldMemberNames.CommaText, 'Fields 1');
      CheckEquals('X,Y', p.FieldMemberNames.CommaText, 'Fields 2');

      p.Member['x'].Value:=123;
      p.Member['y'].Value:=456;
      p:=nil;

      exec.RunProgram(0);

      CheckEquals( '123, 456', exec.Result.ToString, 'Result');

      p:=exec.Info.Vars['p'];
      CheckEquals(456, p.Member['x'].Value, 'After exec Fields 1');
      CheckEquals(789, p.Member['y'].Value, 'After exec Fields 2');
   finally
      exec.EndProgram;
   end;
end;

// DynamicArray
//
procedure TdwsUnitTests.DynamicArray;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   astr : IInfo;
   data : TData;
   myData : TData;
begin
   prog:=FCompiler.Compile( 'var astr : array of String;'#13#10
                           +'astr.Add("hello");'#13#10
                           +'astr.Add("world");'#13#10
                           +'procedure MyTest; begin Print(astr.Length); Print(astr[0]); end;'#13#10
                           );

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   exec:=prog.BeginNewExecution;
   try
      exec.RunProgram(0);

      astr:=exec.Info.Vars['astr'];

      CheckEquals('array of String', astr.ValueAsString, 'as string');

      CheckEquals(0, astr.Member['low'].ValueAsInteger, 'low');
      CheckEquals(1, astr.Member['high'].ValueAsInteger, 'high');
      CheckEquals(2, astr.Member['length'].ValueAsInteger, 'length');

      CheckEquals('hello', astr.Element([0]).ValueAsString, 'item 0');
      CheckEquals('world', astr.Element([1]).ValueAsString, 'item 1');

      data:=astr.Data;
      CheckEquals(2, Length(data), 'data length');
      CheckEquals('hello', data[0], 'data 0');
      CheckEquals('world', data[1], 'data 1');

      SetLength(myData, 1);
      myData[0]:='byebye';
      astr.Data:=myData;

      exec.Info.Func['MyTest'].Call;

      CheckEquals('1byebye', exec.Result.ToString, 'after setdata');
   finally
      exec.EndProgram;
   end;
end;

// DynamicArrayResult
//
procedure TdwsUnitTests.DynamicArrayResult;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=FCompiler.Compile( 'var astr := FuncStrings(2);'#13#10
                           +'PrintLn(astr.Join("-"));'#13#10
                           +'astr := FuncStrings(0);'#13#10
                           +'PrintLn(astr.Join("-"));'#13#10
                           +'astr := FuncStrings(3);'#13#10
                           +'PrintLn(astr.Join("/"));'#13#10
                           );

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   exec:=prog.BeginNewExecution;
   try
      exec.RunProgram(0);

      CheckEquals('', exec.Msgs.AsInfo, 'Run');

      CheckEquals('0-1'#13#10#13#10'0/1/2'#13#10, exec.Result.ToString);
   finally
      exec.EndProgram;
   end;
end;

// DynamicArrayResult2
//
procedure TdwsUnitTests.DynamicArrayResult2;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=FCompiler.Compile( 'var astr := FuncStrings2(2);'#13#10
                           +'PrintLn(astr.Join("-"));'#13#10
                           +'astr := FuncStrings2(0);'#13#10
                           +'PrintLn(astr.Join("-"));'#13#10
                           +'astr := FuncStrings2(3);'#13#10
                           +'PrintLn(astr.Join("/"));'#13#10
                           );

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   exec:=prog.BeginNewExecution;
   try
      exec.RunProgram(0);

      CheckEquals('', exec.Msgs.AsInfo, 'Run');

      CheckEquals('0-2'#13#10#13#10'0/2/4'#13#10, exec.Result.ToString);
   finally
      exec.EndProgram;
   end;
end;

// ClassPropertyInfo
//
procedure TdwsUnitTests.ClassPropertyInfo;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   p, p2 : IInfo;
begin
   prog:=FCompiler.Compile( 'var o := TTestClass.Create;');

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   exec:=prog.BeginNewExecution;
   try
      p:=exec.Info.Vars['o'];
      CheckEquals('(nil)', p.ValueAsString, 'ClassInfo before run');

      exec.RunProgram(0);

      p:=exec.Info.Vars['o'];
      CheckEquals('TTestClass', p.ValueAsString, 'ClassInfo after init');

      CheckTrue(p.ExternalObject=nil, 'External object');

      p.Member['MyReadWriteProp'].Value:=123;
      CheckEquals(123, p.Member['MyReadWriteProp'].Value, 'RW Prop');
      CheckEquals(1230, p.Member['MyReadOnlyProp'].Value, 'RO Prop');

      p.Member['MyWriteOnlyProp'].Value:=123;
      CheckEquals(12, p.Member['FField'].Value, 'direct field');
      CheckEquals('FField', p.FieldMemberNames.CommaText, 'MemberNames 1');
      CheckEquals('FField', p.FieldMemberNames.CommaText, 'MemberNames 2');

      p2:=p.Member['ArrayProp'];
      p2.Parameter['v'].Value:='12';
      CheckEquals(24, p2.ValueAsInteger, 'Array prop read');

      p.Method['Free'].Call;
      CheckEquals('destroyed TTestClass', p.ValueAsString, 'ClassInfo after destroy');
   finally
      exec.EndProgram;
   end;
end;

// ClassInit
//
procedure TdwsUnitTests.ClassInit;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=FCompiler.Compile( 'var o := TTestClass.MyCreateInit; Print(o.FField);');
   exec:=prog.BeginNewExecution;
   try
      exec.RunProgram(0);
      CheckEquals('789456', exec.Result.ToString);
   finally
      exec.EndProgram;
   end;
end;

// DestructorAndExternalObject
//
procedure TdwsUnitTests.DestructorAndExternalObject;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   FContext.FMagicVar:='';
   prog:=FCompiler.Compile( 'var o := TTestClass.MyCreate(''hello'');'
                           +'o.Print;'
                           +'o.Free;'
                           +'PrintLn(magicVar);'
                           +'o.Print;');

   exec:=prog.BeginNewExecution;
   try
      exec.RunProgram(0);

      CheckEquals( 'hello'#13#10'cleaned up'#13#10
                  +'Runtime Error: Object already destroyed [line: 1, column: 74]'#13#10,
                  exec.Result.ToString+exec.Msgs.AsInfo);
   finally
      exec.EndProgram;
   end;
end;

// ExternalObject
//
procedure TdwsUnitTests.ExternalObject;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   p : IInfo;
begin
   FContext.FMagicVar:='';
   prog:=FCompiler.Compile( 'var o := TTestClass.MyCreate(''hello'');');

   exec:=prog.BeginNewExecution;
   try
      exec.RunProgram(0);

      p:=exec.Info.Vars['o'];
      CheckEquals('TObject', p.ExternalObject.ClassName, 'External object');
      p.ExternalObject.Free;
      p.ExternalObject:=nil;
      CheckTrue(p.ExternalObject=nil, 'External object cleared');
   finally
      exec.EndProgram;
   end;
end;

// CustomDestructor
//
procedure TdwsUnitTests.CustomDestructor;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   FContext.FMagicVar:='';
   prog:=FCompiler.Compile( 'var o := TTestClass.MyCreate(''hello'');'
                           +'o.Print;'
                           +'o.MyDestroy;'
                           +'PrintLn(magicVar);'
                           +'o.Print;');

   exec:=prog.BeginNewExecution;
   try
      exec.RunProgram(0);

      CheckEquals( 'hello'#13#10'my destructor'#13#10'cleaned up'#13#10
                  +'Runtime Error: Object already destroyed [line: 1, column: 79]'#13#10,
                  exec.Result.ToString+exec.Msgs.AsInfo);
   finally
      exec.EndProgram;
   end;
end;

// Delegates
//
procedure TdwsUnitTests.Delegates;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   func : IInfo;
begin
   FContext.FMagicVar:='';
   prog:=FCompiler.Compile( 'type TFunc = function (i : Integer) : Integer;'
                           +'var v1 : TFunc = FuncInc;'
                           +'function MyFunc(i : Integer) : Integer; begin Result:=i+10; end;'
                           +'var v2 : TFunc = MyFunc;'
                           +'PrintLn(v1(1));'
                           +'PrintLn(v2(1));');

   exec:=prog.BeginNewExecution;
   try
      exec.RunProgram(0);

      CheckEquals( '2'#13#10'11'#13#10,
                  exec.Result.ToString);

      CheckEquals(124, exec.Info.Vars['v1'].Call([123]).Value, 'Call unit func direct');
      CheckEquals(133, exec.Info.Vars['v2'].Call([123]).Value, 'Call source func direct');

      func:=exec.Info.Vars['v1'];
      func.Parameter['i'].Value:=456;
      CheckEquals(457, func.Call.Value, 'Call unit func with params');

      func:=exec.Info.Vars['v2'];
      func.Parameter['i'].Value:=789;
      CheckEquals(799, func.Call.Value, 'Call source func with params');
   finally
      exec.EndProgram;
   end;
end;

// Operators
//
procedure TdwsUnitTests.Operators;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   FContext.FMagicVar:='';
   prog:=FCompiler.Compile( 'var f := 1.0 ^ 2.5;'
                           +'PrintLn(f);'
                           +'PrintLn(f^(-1.5));');

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   exec:=prog.BeginNewExecution;
   try
      exec.RunProgram(0);

      CheckEquals( '3.5'#13#10'2'#13#10,
                  exec.Result.ToString+exec.Msgs.AsInfo);
   finally
      exec.EndProgram;
   end;
end;

// OpenArray
//
procedure TdwsUnitTests.OpenArray;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   FContext.FMagicVar:='';
   prog:=FCompiler.Compile( 'PrintLn(FuncOpenArray(["one","two"]));'#13#10
                           +'PrintLn(FuncOpenArray([]));');

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   exec:=prog.BeginNewExecution;
   try
      exec.RunProgram(0);

      CheckEquals( '2:one,two'#13#10'0:'#13#10,
                  exec.Result.ToString+exec.Msgs.AsInfo);
   finally
      exec.EndProgram;
   end;
end;

// CallPrint
//
procedure TdwsUnitTests.CallPrint;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   print : IInfo;
begin
   FContext.FMagicVar:='';
   prog:=FCompiler.Compile( 'PrintLn("Hello");');

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   exec:=prog.BeginNewExecution;
   try
      exec.RunProgram(0);
      print:=exec.Info.Func['Print'];
      print.Call(['world']);

      CheckEquals( 'Hello'#13#10'world',
                  exec.Result.ToString+exec.Msgs.AsInfo);
   finally
      exec.EndProgram;
   end;
end;

// CreateExternally
//
procedure TdwsUnitTests.CreateExternally;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   printit : IInfo;
   v : IInfo;
begin
   FContext.FMagicVar:='';
   prog:=FCompiler.Compile( 'procedure PrintIt(o : TTestClass);'#13#10
                           +'begin o.PrintExternal end');

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   exec:=prog.BeginNewExecution;
   try
      v:=exec.Info.Vars['TTestClass'].Method['Create'].Call();
      v.ScriptObj.ExternalObject:=TObject.Create;

      printit:=exec.Info.Func['PrintIt'];
      printit.Call([v.Value]);

      CheckEquals('TObject'#13#10,
                  exec.Result.ToString+exec.Msgs.AsInfo);
   finally
      exec.EndProgram;
   end;
end;

// DeprecatedProp
//
procedure TdwsUnitTests.DeprecatedProp;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile( 'var t := new TTestClass;'#13#10
                           +'var i := t.DeprecatedProp;');

   CheckEquals('Warning: "DeprecatedProp" has been deprecated: Obsolete [line: 2, column: 12]'#13#10,
               prog.Msgs.AsInfo, 'Compile');
end;

// ReservedNameMethod
//
procedure TdwsUnitTests.ReservedNameMethod;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile( 'var t := new TTestClass;'#13#10
                           +'var i := t.Function;');

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');
end;

// CallInNested
//
procedure TdwsUnitTests.CallInNested;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile( 'function Test1 : Integer; begin;'#13#10
                           +'procedure Test2; begin;'#13#10
                           +'Result := Func1;'#13#10
                           +'end; Test2; end;'#13#10
                           +'Print(Test1);');

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   CheckEquals('1', prog.Execute.Result.ToString, 'exec');
end;

// OverloadedFunc
//
procedure TdwsUnitTests.OverloadedFunc;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile( 'Print(FuncOverload(123));'#13#10
                           +'Print(FuncOverload("123"));'#13#10
                           +'var t := TTestClass.Create;'#13#10
                           +'Print(t.MethOverload(234));'#13#10
                           +'Print(t.MethOverload("234"));');

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   CheckEquals('246(123)468(234)', prog.Execute.Result.ToString, 'exec');
end;

// FastEvalTest
//
procedure TdwsUnitTests.FastEvalTest;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile( 'Print(FuncFast("hello"));'#13#10
                           +'var f := @FuncFast;'#13#10
                           +'Print(f("test"));');

   CheckEquals('', prog.Msgs.AsInfo, 'Compile 1');

   CheckEquals('54', prog.Execute.Result.ToString, 'exec 1');

   prog:=FCompiler.Compile( 'var p := FuncFastPoint(2);'#13#10
                           +'PrintLn(p.X);'#13#10
                           +'PrintLn(p.Y);');

   CheckEquals('', prog.Msgs.AsInfo, 'Compile 2');

   CheckEquals('2'#13#10'3'#13#10, prog.Execute.Result.ToString, 'exec 2');
end;

// ArrayOfObjects
//
procedure TdwsUnitTests.ArrayOfObjects;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile( 'var a : TDynObjects; a.SetLength(1);'#13#10
                           +'a[0] := nil;'#13#10
                           +'a.Add(nil);'#13#10
                           +'var s : TStaticObjects;'#13#10
                           +'s[0] := nil;'#13#10
                           );

   CheckEquals('', prog.Msgs.AsInfo, 'Compile 1');

   CheckEquals('', prog.Execute.Msgs.AsInfo, 'exec errs');
   CheckEquals('', prog.Execute.Result.ToString, 'exec result');
end;

// FuncVariantTest
//
procedure TdwsUnitTests.FuncVariantTest;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile( 'var i : Integer; var f : Float; var s : String; var b : Boolean;'#13#10
                           +'i:=FuncVariant(123);'#13#10
                           +'Print(i);'#13#10
                           +'f:=FuncVariant(12.5);'#13#10
                           +'Print(f);'#13#10
                           +'f:=FuncVariant(456);'#13#10
                           +'Print(f);'#13#10
                           +'s:=FuncVariant("hello");'#13#10
                           +'Print(s);'#13#10
                           +'b:=FuncVariant(True);'#13#10
                           +'Print(b);'#13#10
                           );

   CheckEquals('', prog.Msgs.AsInfo, 'Compile 1');

   CheckEquals('', prog.Execute.Msgs.AsInfo, 'exec errs');
   CheckEquals('12312.5456helloTrue', prog.Execute.Result.ToString, 'exec result');
end;

// FuncVariantDateTest
//
procedure TdwsUnitTests.FuncVariantDateTest;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile( 'var f : Float; var s : String;'#13#10
                           +'f:=FuncVariantDate;'#13#10
                           +'if Now-f>1/86400 then Print("bug");'#13#10
                           +'s:=FuncVariantDate;'#13#10
                           +'if s="" then Print("rebug");'#13#10
                           );

   CheckEquals('', prog.Msgs.AsInfo, 'Compile 1');

   CheckEquals('', prog.Execute.Msgs.AsInfo, 'exec errs');
   CheckEquals('', prog.Execute.Result.ToString, 'exec result');
end;

// FuncNilTest
//
procedure TdwsUnitTests.FuncNilTest;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile( 'var o := TObject.Create;'#13#10
                           +'if o=nil then Print("bug");'#13#10
                           +'o:=FuncNil;'#13#10
                           +'if o<>nil then Print("rebug");'#13#10
                           );

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   CheckEquals('', prog.Execute.Msgs.AsInfo, 'exec errs');
   CheckEquals('', prog.Execute.Result.ToString, 'exec result');
end;

// SetTest
//
procedure TdwsUnitTests.SetTest;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile( 'var s : TMyEnums;'#13#10
                           +'Include(s, meOne);'#13#10
                           +'if meOne in s then Print("1");'#13#10
                           +'if meTen in s then Print("A");'#13#10
                           +'Include(s, meTen);'#13#10
                           +'if meOne not in s then Print("B");'#13#10
                           +'if meTen in s then Print("2");'#13#10
                           +'s.Exclude(meOne);'#13#10
                           +'if meOne in s then Print("C");'#13#10
                           +'if meTen in s then Print("3");'#13#10
                           );

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   CheckEquals('', prog.Execute.Msgs.AsInfo, 'exec errs');
   CheckEquals('123', prog.Execute.Result.ToString, 'exec result');
end;

// ClassNameTest
//
procedure TdwsUnitTests.ClassNameTest;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile( 'PrintLn(FuncClassName);'#13#10
                           +'Print(FuncClassName(TObject.Create));'#13#10
                           );

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   CheckEquals('', prog.Execute.Msgs.AsInfo, 'exec errs');
   CheckEquals(#13#10'TObject', prog.Execute.Result.ToString, 'exec result');
end;

// VirtCreateFunc
//
procedure TdwsUnitTests.VirtCreateFunc;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile( 'Print(FuncReturnVirtCreate.ClassName);'#13#10
                           );

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   FContext.FMagicVar:='';

   CheckEquals('', prog.Execute.Msgs.AsInfo, 'exec errs');
   CheckEquals('TTestClass', prog.Execute.Result.ToString, 'exec result');
   CheckEquals('-1', FContext.FMagicVar, 'magic');
end;

// ExplicitUses
//
procedure TdwsUnitTests.ExplicitUses;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   FCompiler.Config.CompilerOptions:=FCompiler.Config.CompilerOptions+[coExplicitUnitUses];
   try
      prog:=FCompiler.Compile( 'var f := Func1;');

      CheckEquals('Syntax Error: Unknown name "Func1" [line: 1, column: 10]'#13#10, prog.Msgs.AsInfo, 'Compile no uses');

      prog:=FCompiler.Compile( 'uses Test;'#13#10
                              +'var f := Func1;'#13#10
                              +'PrintLn(f);');

      CheckEquals('', prog.Msgs.AsInfo, 'Compile no uses');

      exec:=prog.Execute;
      CheckEquals( '1'#13#10, exec.Result.ToString+exec.Msgs.AsInfo);

   finally
      FCompiler.Config.CompilerOptions:=FCompiler.Config.CompilerOptions-[coExplicitUnitUses];
   end;
end;

// UnknownUnit
//
procedure TdwsUnitTests.UnknownUnit;
var
   un : TdwsUnit;
   prog : IdwsProgram;
begin
   un:=TdwsUnit.Create(nil);
   try
      un.UnitName:='TestBug';
      un.Dependencies.Add('Bogus');
      un.Script:=FCompiler;

      prog:=FCompiler.Compile('');
      CheckEquals('Syntax Error: Unit "Bogus" referenced in unit "TestBug" not found'#13#10, prog.Msgs.AsInfo);
   finally
      un.Free;
   end;
end;

// CircularUnit
//
procedure TdwsUnitTests.CircularUnit;
var
   un : TdwsUnit;
   prog : IdwsProgram;
begin
   un:=TdwsUnit.Create(nil);
   try
      un.UnitName:='TestBug';
      un.Dependencies.Add('TestBug');
      un.Script:=FCompiler;

      prog:=FCompiler.Compile('');
      CheckEquals('Syntax Error: Circular referencing units detected'#13#10, prog.Msgs.AsInfo);
   finally
      un.Free;
   end;
end;

// DuplicateUnit
//
procedure TdwsUnitTests.DuplicateUnit;
var
   un : TdwsUnit;
   prog : IdwsProgram;
begin
   un:=TdwsUnit.Create(nil);
   try
      un.UnitName:='Test';
      un.Script:=FCompiler;

      prog:=FCompiler.Compile('');
      CheckEquals('Syntax Error: Unit "Test" redeclared'#13#10, prog.Msgs.AsInfo);
   finally
      un.Free;
   end;
end;

// CallLevels
//
procedure TdwsUnitTests.CallLevels;
var
   un : TdwsUnit;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   un:=TdwsUnit.Create(nil);
   try
      un.UnitName:='TestCallLevels';
      un.Script:=FCompiler;
      with un.Functions.Add do begin
         Name:='UnitProc';
         OnEval:=FContext.ProcCallLevelsEval;
      end;

      prog:=FCompiler.Compile(
          'var v := "a";'#13#10
         +'var i := Ord("b");'#13#10
         +'procedure CallMe; begin v+=Chr(i); i+=1; end;'#13#10
         +'procedure RunMe; begin CallMe; UnitProc; CallMe; end;');
      CheckEquals('', prog.Msgs.AsInfo);

      exec:=prog.BeginNewExecution;
      try
         exec.RunProgram(0);
         CheckEquals('a', exec.Info.ValueAsString['v']);
         exec.Info.Func['CallMe'].Call;
         CheckEquals('ab', exec.Info.ValueAsString['v']);
         exec.Info.Func['RunMe'].Call;
         CheckEquals('abcde', exec.Info.ValueAsString['v']);
         exec.Info.Func['CallMe'].Call;
         CheckEquals('abcdef', exec.Info.ValueAsString['v']);
      finally
         exec.EndProgram;
         exec:=nil;
         prog:=nil;
      end;
   finally
      un.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('dwsUnit', TdwsUnitTests);

end.
