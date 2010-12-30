unit UdwsUnitTests;

interface

uses Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs,
   dwsTokenizer, dwsSymbols;

type

   TdwsUnitTests = class (TTestCase)
      private
         FCompiler : TDelphiWebScript;
         FUnit : TdwsUnit;

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure DeclareTestEnumerate;
         procedure DeclareTestFuncs;
         procedure DeclareTestClasses;

         procedure Func1Eval(Info: TProgramInfo);
         procedure FuncOneEval(Info: TProgramInfo);
         procedure FuncOneDotFiveEval(Info: TProgramInfo);
         procedure FuncTrueEval(Info: TProgramInfo);
         procedure FuncIncEval(Info: TProgramInfo);
         procedure FuncIncNEval(Info: TProgramInfo);
         procedure FuncEnumEval(Info: TProgramInfo);
         procedure FuncVarEval(Info: TProgramInfo);
         procedure FuncFloatEval(Info: TProgramInfo);

         procedure FuncExceptionEval(Info: TProgramInfo);

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
      ;

type
   TdwsFunctionCracker = class (TdwsFunction)
   end;

   TdwsClassCracker = class (TdwsClass)
   end;

   TdwsPropertyCracker = class (TdwsProperty)
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
   DeclareTestFuncs;
   DeclareTestClasses;
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
   func:=FUnit.Functions.Add as TdwsFunction;
   func.Name:='Func1';
   func.ResultType:='Integer';
   func.OnEval:=Func1Eval;

   func:=FUnit.Functions.Add as TdwsFunction;
   func.Name:='FuncOne';
   func.ResultType:='String';
   func.OnEval:=FuncOneEval;

   func:=FUnit.Functions.Add as TdwsFunction;
   func.Name:='FuncOneDotFive';
   func.ResultType:='Float';
   func.OnEval:=FuncOneDotFiveEval;

   func:=FUnit.Functions.Add as TdwsFunction;
   func.Name:='FuncTrue';
   func.ResultType:='Boolean';
   func.OnEval:=FuncTrueEval;

   func:=FUnit.Functions.Add as TdwsFunction;
   func.Name:='FuncException';
   func.ResultType:='';
   func.OnEval:=FuncExceptionEval;

   func:=FUnit.Functions.Add as TdwsFunction;
   func.Name:='FuncInc';
   func.ResultType:='Integer';
   func.OnEval:=FuncIncEval;
   param:=func.Parameters.Add as TdwsParameter;
   param.Name:='v';
   param.DataType:='Integer';

   func:=FUnit.Functions.Add as TdwsFunction;
   func.Name:='FuncIncN';
   func.ResultType:='Integer';
   func.OnEval:=FuncIncNEval;
   param:=func.Parameters.Add as TdwsParameter;
   param.Name:='v';
   param.DataType:='Integer';
   param:=func.Parameters.Add as TdwsParameter;
   param.Name:='n';
   param.DataType:='Integer';
   param.DefaultValue:='1';

   func:=FUnit.Functions.Add as TdwsFunction;
   func.Name:='FuncEnum';
   func.ResultType:='Integer';
   func.OnEval:=FuncEnumEval;
   param:=func.Parameters.Add as TdwsParameter;
   param.Name:='e';
   param.DataType:='TMyEnum';
   param.DefaultValue:='meOne';

   func:=FUnit.Functions.Add as TdwsFunction;
   func.Name:='FuncVar';
   func.OnEval:=FuncVarEval;
   param:=func.Parameters.Add as TdwsParameter;
   param.Name:='i';
   param.DataType:='Integer';
   param.IsVarParam:=True;
   param.IsWritable:=True;
   param:=func.Parameters.Add as TdwsParameter;
   param.Name:='n';
   param.DataType:='Integer';
   param.DefaultValue:='1';

   func:=FUnit.Functions.Add as TdwsFunction;
   func.Name:='FuncFloat';
   func.OnEval:=FuncFloatEval;
   func.ResultType:='Float';
   param:=func.Parameters.Add as TdwsParameter;
   param.Name:='a';
   param.DataType:='Float';
   param:=func.Parameters.Add as TdwsParameter;
   param.Name:='b';
   param.DataType:='Float';
   param.DefaultValue:='0.5';
end;

// DeclareTestClasses
//
procedure TdwsUnitTests.DeclareTestClasses;
var
   cls : TdwsClass;
   meth : TdwsMethod;
   prop : TdwsProperty;
begin
   cls:=FUnit.Classes.Add as TdwsClass;
   cls.Name:='TTestClass';

   meth:=cls.Methods.Add as TdwsMethod;
   meth.Name:='GetMyProp';
   meth.ResultType:='Integer';

   prop:=cls.Properties.Add as TdwsProperty;
   prop.Name:='MyReadOnlyProp';
   prop.DataType:='Integer';
   prop.ReadAccess:='GetMyProp';
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

// FuncExceptionEval
//
procedure TdwsUnitTests.FuncExceptionEval(Info: TProgramInfo);
begin
   raise EDelphiException.Create('Hello, Delphi Exception here!');
end;

// CompilationExecution
//
procedure TdwsUnitTests.CompilationExecution(execute : Boolean);
var
   prog : TdwsProgram;
begin
   prog:=FCompiler.Compile(cFuncsTestsSource);
   try
      CheckEquals('', prog.Msgs.AsInfo, 'FuncsTest compile');
      if execute then begin
         prog.Execute;
         CheckEquals('', (prog.Result as TdwsDefaultResult).Text, 'FuncsTest result');
         CheckEquals('', prog.Msgs.AsInfo, 'FuncsTest Msgs');
      end;
   finally
      prog.Free;
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
var
   cls : TdwsClassCracker;
begin
   CheckEquals('function Func1 : Integer;', FuncByName('Func1').GetDisplayName);
   CheckEquals('function FuncOne : String;', FuncByName('FuncOne').GetDisplayName);
   CheckEquals('function FuncOneDotFive : Float;', FuncByName('FuncOneDotFive').GetDisplayName);
   CheckEquals('function FuncTrue : Boolean;', FuncByName('FuncTrue').GetDisplayName);
   CheckEquals('procedure FuncException;', FuncByName('FuncException').GetDisplayName);
   CheckEquals('function FuncInc(v : Integer) : Integer;', FuncByName('FuncInc').GetDisplayName);

   cls:=ClassByName('TTestClass');
   CheckEquals('TTestClass (TObject)', cls.GetDisplayName);
   CheckEquals('property MyReadOnlyProp: Integer read GetMyProp;', PropertyByName(cls, 'MyReadOnlyProp').GetDisplayName);
end;

// CompiledDescriptions
//
procedure TdwsUnitTests.CompiledDescriptions;
var
   prog : TdwsProgram;
   sym : TSymbol;
begin
   prog:=FCompiler.Compile('');
   try
      sym:=prog.Table.FindSymbol('Func1');
      CheckEquals('function Func1(): Integer', sym.Description);
      sym:=prog.Table.FindSymbol('FuncOne');
      CheckEquals('function FuncOne(): String', sym.Description);
      sym:=prog.Table.FindSymbol('FuncOneDotFive');
      CheckEquals('function FuncOneDotFive(): Float', sym.Description);
      sym:=prog.Table.FindSymbol('FuncTrue');
      CheckEquals('function FuncTrue(): Boolean', sym.Description);
      sym:=prog.Table.FindSymbol('FuncException');
      CheckEquals('procedure FuncException()', sym.Description);
      sym:=prog.Table.FindSymbol('FuncInc');
      CheckEquals('function FuncInc(v: Integer): Integer', sym.Description);
      sym:=prog.Table.FindSymbol('FuncIncN');
      CheckEquals('function FuncIncN(v: Integer; n: Integer = 1): Integer', sym.Description);

      sym:=prog.Table.FindSymbol('TAutoEnum');
      CheckEquals('(aeVal9, aeVal8, aeVal7, aeVal6, aeVal5, aeVal4, aeVal3, aeVal2, aeVal1)', sym.Description);

      sym:=prog.Table.FindSymbol('TTestClass');
      sym:=(sym as TClassSymbol).Members.FindLocal('MyReadOnlyProp');
      CheckEquals('property MyReadOnlyProp: Integer read GetMyProp', sym.Description);
   finally
      prog.Free;
   end;
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
   FCompiler.Config.CompilerOptions:=[coSymbolDictionary, coContextMap];
   CompilationExecution(False);
end;

// ExecutionNonOptimized
//
procedure TdwsUnitTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=[];
   CompilationExecution(True);
end;

// ExecutionOptimized
//
procedure TdwsUnitTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   CompilationExecution(True);
end;

// DelphiException
//
procedure TdwsUnitTests.DelphiException;
var
   prog : TdwsProgram;
begin
   prog:=FCompiler.Compile('FuncException;');
   try
      CheckEquals('', prog.Msgs.AsInfo, 'Compile');
      prog.Execute;
      CheckEquals('Runtime Error: Hello, Delphi Exception here! [line: 1, column: 1]'#13#10,
                  prog.Msgs.AsInfo, 'Execute Msgs');
   finally
      prog.Free;
   end;
end;

// DelphiExceptionReRaise
//
procedure TdwsUnitTests.DelphiExceptionReRaise;
var
   prog : TdwsProgram;
begin
   prog:=FCompiler.Compile( 'try'#13#10
                           +#9'FuncException;'#13#10
                           +'except'#13#10
                           +#9'raise;'#13#10
                           +'end;'#13#10
                           );
   try
      CheckEquals('', prog.Msgs.AsInfo, 'Compile');
      prog.Execute;
      CheckEquals('Runtime Error: Hello, Delphi Exception here! [line: 2, column: 2]'#13#10,
                  prog.Msgs.AsInfo, 'Execute Msgs');
   finally
      prog.Free;
   end;
end;

// ListOrdAutoEnum
//
procedure TdwsUnitTests.ListOrdAutoEnum;
var
   i : Integer;
   script : String;
   prog : TdwsProgram;
begin
   script:='';
   for i:=1 to 9 do begin
      script:=script+'Print(Ord(aeVal'+IntToStr(i)+'));'#13#10;
   end;
   prog:=FCompiler.Compile(script);
   try
      CheckEquals('', prog.Msgs.AsInfo, 'Compile');
      prog.Execute;
      CheckEquals('876543210', (prog.Result as TdwsDefaultResult).Text, 'Enums Ord');
   finally
      prog.Free;
   end;
end;

// CallFunc
//
procedure TdwsUnitTests.CallFunc;
var
   prog : TdwsProgram;
   funcInfo : IInfo;
begin
   prog:=FCompiler.Compile( 'function Hello(name : String) : String;'
                           +'begin'
                           +'   Result:=''Hello ''+name;'
                           +'end;');
   try
      CheckEquals('', prog.Msgs.AsInfo, 'Compile');

      prog.BeginProgram;
      try
         funcInfo:=prog.Info.Func['Func1'];
         CheckEquals(1, funcInfo.Call.Value, 'Func1 call');

         funcInfo:=prog.Info.Func['FuncOne'];
         CheckEquals('One', funcInfo.Call.Value, 'FuncOne call');

         funcInfo:=prog.Info.Func['Hello'];
         CheckEquals('Hello world', funcInfo.Call(['world']).Value, 'Hello world');
      finally
         prog.EndProgram;
      end;
   finally
      prog.Free;
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
