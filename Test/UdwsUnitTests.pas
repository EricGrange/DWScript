unit UdwsUnitTests;

interface

uses Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs,
   dwsTokenizer, dwsSymbols, dwsUtils, dwsStack;

type

   TdwsUnitTests = class (TTestCase)
      private
         FCompiler : TDelphiWebScript;
         FUnit : TdwsUnit;
         FMagicVar : String;

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure DeclareTestEnumerate;
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
         procedure FuncVarEval(Info: TProgramInfo);
         procedure FuncFloatEval(Info: TProgramInfo);
         procedure FuncPointEval(Info: TProgramInfo);
         procedure FuncPointVarParamEval(Info: TProgramInfo);
         procedure FuncPointArrayEval(Info: TProgramInfo);
         procedure FuncClassNameEval(Info: TProgramInfo);
         procedure FuncOpenArrayEval(Info: TProgramInfo);

         procedure ClassConstructor(Info: TProgramInfo; var ExtObject: TObject);
         procedure ClassCleanup(ExternalObject: TObject);
         procedure ClassDestructor(Info: TProgramInfo; ExtObject: TObject);
         procedure MethodPrintEval(Info: TProgramInfo; ExtObject: TObject);
         procedure MethodPrintExternalEval(Info: TProgramInfo; ExtObject: TObject);
         procedure MethodGetIntEval(Info: TProgramInfo; ExtObject: TObject);
         procedure MethodSetIntEval(Info: TProgramInfo; ExtObject: TObject);
         procedure MethodGetArrayIntEval(Info: TProgramInfo; ExtObject: TObject);

         procedure FuncExceptionEval(Info: TProgramInfo);

         procedure DoReadVar(info: TProgramInfo; var value : Variant);
         procedure DoWriteVar(info: TProgramInfo; const value : Variant);

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
         procedure AssignTest;
         procedure PredefinedArray;
         procedure PredefinedRecord;
         procedure DynamicArray;
         procedure ClassPropertyInfo;
         procedure DestructorAndExternalObject;
         procedure ExternalObject;
         procedure CustomDestructor;
         procedure Delegates;
         procedure Operators;
         procedure OpenArray;
         procedure CallPrint;
         procedure CreateExternally;
         procedure DeprecatedProp;

         procedure ExplicitUses;
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
// ------------------ TdwsUnitTests ------------------
// ------------------

// SetUp
//
procedure TdwsUnitTests.SetUp;
begin
   FCompiler:=TDelphiWebScript.Create(nil);

   FUnit:=TdwsUnit.Create(nil);
   FUnit.UnitName:='Test';
   FUnit.Script:=FCompiler;

   DeclareTestEnumerate;
   DeclareTestRecords;
   DeclareTestClasses;
   DeclareTestVars;
   DeclareTestArrays;
   DeclareTestFuncs;
   DeclareTestOperators;
end;

// TearDown
//
procedure TdwsUnitTests.TearDown;
begin
   FUnit.Free;
   FCompiler.Free;
end;

// DeclareTestEnumerate
//
procedure TdwsUnitTests.DeclareTestEnumerate;
var
   i : Integer;
   enum : TdwsEnumeration;
   elem : TdwsElement;
begin
   enum:=FUnit.Enumerations.Add as TdwsEnumeration;
   enum.Name:='TMyEnum';
   elem:=enum.Elements.Add as TdwsElement;
   elem.Name:='meOne';
   elem.UserDefValue:=1;
   elem:=enum.Elements.Add as TdwsElement;
   elem.Name:='meTen';
   elem.UserDefValue:=10;

   enum:=FUnit.Enumerations.Add as TdwsEnumeration;
   enum.Name:='TAutoEnum';
   for i:=1 to 9 do begin
      elem:=enum.Elements.Add as TdwsElement;
      elem.Name:='aeVal'+IntToStr(10-i);
   end;
end;

// DeclareTestFuncs
//
procedure TdwsUnitTests.DeclareTestFuncs;
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
   func.Name:='FuncOpenArray';
   func.ResultType:='String';
   param:=func.Parameters.Add;
   param.Name:='p';
   param.DataType:='array of const';
   func.OnEval:=FuncOpenArrayEval;
end;

// DeclareTestClasses
//
procedure TdwsUnitTests.DeclareTestClasses;
var
   cls : TdwsClass;
   cst : TdwsConstructor;
   meth : TdwsMethod;
   fld : TdwsField;
   prop : TdwsProperty;
   param : TdwsParameter;
   constant : TdwsConstant;
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
   meth.Name:='GetArrayProp';
   meth.ResultType:='Integer';
   param:=meth.Parameters.Add;
   param.DataType:='String';
   param.Name:='v';
   meth.OnEval:=MethodGetArrayIntEval;

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
end;

// DeclareTestVars
//
procedure TdwsUnitTests.DeclareTestVars;
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
end;

// DeclareTestArrays
//
procedure TdwsUnitTests.DeclareTestArrays;
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
end;

// DeclareTestRecords
//
procedure TdwsUnitTests.DeclareTestRecords;
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
procedure TdwsUnitTests.DeclareTestOperators;
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
procedure TdwsUnitTests.Func1Eval(Info: TProgramInfo);
begin
   Info.ResultAsInteger:=1;
end;

// FuncOneEval
//
procedure TdwsUnitTests.FuncOneEval(Info: TProgramInfo);
begin
   Info.ResultAsString:='One';
end;

// FuncOneDotFiveEval
//
procedure TdwsUnitTests.FuncOneDotFiveEval(Info: TProgramInfo);
begin
   Info.ResultAsFloat:=1.5;
end;

// FuncTrueEval
//
procedure TdwsUnitTests.FuncTrueEval(Info: TProgramInfo);
begin
   Info.ResultAsBoolean:=True;
end;

// FuncIncEval
//
procedure TdwsUnitTests.FuncIncEval(Info: TProgramInfo);
begin
   Info.ResultAsInteger:=Info.ValueAsInteger['v']+1;
end;

// FuncIncNEval
//
procedure TdwsUnitTests.FuncIncNEval(Info: TProgramInfo);
begin
   Info.ResultAsInteger:=Info.ValueAsInteger['v']+Info.ValueAsInteger['n'];
end;

// FuncEnumEval
//
procedure TdwsUnitTests.FuncEnumEval(Info: TProgramInfo);
begin
   Info.ResultAsInteger:=Info.ValueAsInteger['e'];
end;

// FuncVarEval
//
procedure TdwsUnitTests.FuncVarEval(Info: TProgramInfo);
begin
   Info.ValueAsInteger['i']:=Info.ParamAsInteger[0]+Info.ParamAsInteger[1];
end;

// FuncFloatEval
//
procedure TdwsUnitTests.FuncFloatEval(Info: TProgramInfo);
begin
   Info.ResultAsFloat:=Info.ParamAsFloat[0]+Info.ValueAsFloat['b'];
end;

// FuncPointEval
//
procedure TdwsUnitTests.FuncPointEval(Info: TProgramInfo);
begin
   Info.Vars['Result'].Member['x'].Value:=12;
   Info.Vars['Result'].Member['y'].Value:=24;
end;

// FuncPointVarParamEval
//
procedure TdwsUnitTests.FuncPointVarParamEval(Info: TProgramInfo);
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
procedure TdwsUnitTests.FuncPointArrayEval(Info: TProgramInfo);
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
procedure TdwsUnitTests.FuncClassNameEval(Info: TProgramInfo);
var
   o : IInfo;
begin
   o:=Info.Vars['obj'];
   if o.ScriptObj=nil then
      Info.ResultAsString:=''
   else Info.ResultAsString:=o.ScriptObj.ClassSym.Name;
end;

// FuncOpenArrayEval
//
procedure TdwsUnitTests.FuncOpenArrayEval(Info: TProgramInfo);
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

// ClassConstructor
//
procedure TdwsUnitTests.ClassConstructor(Info: TProgramInfo; var ExtObject: TObject);
begin
   FMagicVar:=Info.ParamAsString[0];
   ExtObject:=TObject.Create;
end;

// ClassCleanup
//
procedure TdwsUnitTests.ClassCleanup(ExternalObject: TObject);
begin
   FMagicVar:='cleaned up';
   ExternalObject.Free;
end;

// ClassDestructor
//
procedure TdwsUnitTests.ClassDestructor(Info: TProgramInfo; ExtObject: TObject);
begin
   Info.Execution.Result.AddString('my destructor'#13#10);
end;

// MethodPrintEval
//
procedure TdwsUnitTests.MethodPrintEval(Info: TProgramInfo; ExtObject: TObject);
begin
   Info.Execution.Result.AddString(FMagicVar+#13#10);
end;

// MethodPrintExternalEval
//
procedure TdwsUnitTests.MethodPrintExternalEval(Info: TProgramInfo; ExtObject: TObject);
begin
   Info.Execution.Result.AddString(ExtObject.ToString+#13#10);
end;

// MethodGetIntEval
//
procedure TdwsUnitTests.MethodGetIntEval(Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=Info.ValueAsInteger['FField']*10;
end;

// MethodSetIntEval
//
procedure TdwsUnitTests.MethodSetIntEval(Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ValueAsInteger['FField']:=Info.ValueAsInteger['v'] div 10;
end;

// MethodGetArrayIntEval
//
procedure TdwsUnitTests.MethodGetArrayIntEval(Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=StrToInt(Info.ValueAsString['v'])*2;
end;

// FuncExceptionEval
//
procedure TdwsUnitTests.FuncExceptionEval(Info: TProgramInfo);
begin
   raise EDelphiException.Create('Hello, Delphi Exception here!');
end;

// DoReadVar
//
procedure TdwsUnitTests.DoReadVar(info: TProgramInfo; var value : Variant);
begin
   value:=FMagicVar;
end;

// DoWriteVar
//
procedure TdwsUnitTests.DoWriteVar(info: TProgramInfo; const value : Variant);
begin
   FMagicVar:=value;
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

   function FuncByName(const aName : String) : TdwsFunctionCracker;
   var
      i : Integer;
   begin
      i:=FUnit.Functions.IndexOf(aName);
      Result:=TdwsFunctionCracker(FUnit.Functions.Items[i] as TdwsFunction);
   end;

   function ClassByName(const aName : String) : TdwsClassCracker;
   var
      i : Integer;
   begin
      i:=FUnit.Classes.IndexOf(aName);
      Result:=TdwsClassCracker(FUnit.Classes.Items[i] as TdwsClass);
   end;

   function PropertyByName(cls : TdwsClass; const aName : String) : TdwsPropertyCracker;
   var
      i : Integer;
   begin
      i:=cls.Properties.IndexOf(aName);
      Result:=TdwsPropertyCracker(cls.Properties.Items[i] as TdwsProperty);
   end;

   function MethodByName(cls : TdwsClass; const aName : String) : TdwsMethodCracker;
   var
      i : Integer;
   begin
      i:=cls.Methods.IndexOf(aName);
      Result:=TdwsMethodCracker(cls.Methods.Items[i] as TdwsMethod);
   end;

   function ConstByName(cls : TdwsClass; const aName : String) : TdwsClassConstantCracker;
   var
      i : Integer;
   begin
      i:=cls.Constants.IndexOf(aName);
      Result:=TdwsClassConstantCracker(cls.Constants.Items[i] as TdwsConstant);
   end;

   function EnumByName(const aName : String) : TdwsEnumerationCracker;
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
                           +'PrintLn(magicVar); magicVar:=''MAGIC''; PrintLn(magicVar);'#13#10);

   CheckEquals('', prog.Msgs.AsInfo, 'Compile');

   exec:=prog.BeginNewExecution;
   try
      exec.Info.ValueAsString['xyzVar']:='xyz';
      FMagicVar:='magic';

      exec.RunProgram(0);

      CheckEquals( 'xyz'#13#10'XYZ'#13#10
                  +'magic'#13#10'MAGIC'#13#10, exec.Result.ToString, 'Result');
      CheckEquals('XYZ', exec.Info.ValueAsString['xyzVar'], 'xyz var value');
      CheckEquals('MAGIC', FMagicVar, 'magic var value');
   finally
      exec.EndProgram;
   end;
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

// DestructorAndExternalObject
//
procedure TdwsUnitTests.DestructorAndExternalObject;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   FMagicVar:='';
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
   FMagicVar:='';
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
   FMagicVar:='';
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
   FMagicVar:='';
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
   FMagicVar:='';
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
   FMagicVar:='';
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
   FMagicVar:='';
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
   FMagicVar:='';
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

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TestFramework.RegisterTest('dwsUnitTests', TdwsUnitTests.Suite);

end.
