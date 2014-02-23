unit UExternalFunctionTests;

interface

uses
   Classes,
   dwsXPlatformTests, dwsComp, dwsErrors, dwsExprList, dwsCompiler, dwsExprs,
   dwsExternalFunctions;

type
   TExternalFunctionTests = class(TTestCase)
      private
         FTests : TStringList;
         FCompiler : TDelphiWebScript;
         FUnit: TdwsUnit;
         procedure RegisterExternalRoutines(const manager : IdwsExternalFunctionsManager);
         procedure GetBoxedStringEval(info : TProgramInfo);
    procedure FreeBoxedString(ExternalObject: TObject);

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure Execution;
         procedure Compilation;
      published
         procedure Test;
   end;

implementation

uses
   SysUtils,
   dwsXPlatform,
   dwsSymbols, dwsUtils;

{ TExternalFunctionTests }

procedure TExternalFunctionTests.Compilation;
var
   source : TStringList;
   i : Integer;
   prog : IdwsProgram;
begin
   source:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);
         CheckEquals('', prog.Msgs.AsInfo, FTests[i]);

      end;

   finally
      source.Free;
   end;
end;

type
   TBoxedString = class
      value: string;
   end;

procedure Blank;
begin
end;

procedure Ints3(a, b, c: integer);
begin
   assert(a = 1);
   assert(b = 5);
   assert(c = 6);
end;

procedure TestString(a: integer; b: string);
begin
   assert(a = 5);
   assert(b = 'Testing');
end;

procedure TestStringExc(a: integer; b: string);
begin
   TestString(a, b);
   Abort;
end;

procedure TestBool(a: integer; b: boolean);
begin
   assert(a = 5);
   assert(b = true);
end;

procedure TestStack(a, b, c, d: integer);
begin
   assert(a = 1);
   assert(b = 5);
   assert(c = 12);
   assert(d = -57);
end;

procedure TestFloat(a: integer; b: double);
begin
   assert(a = 1);
   assert(b = 0.5);
end;

procedure TestObject(a: integer; b: TBoxedString);
begin
   assert(a = 1);
   assert(b.value = 'Boxed String');
end;

procedure TestObjectExc(a: integer; b: TBoxedString);
begin
   TestObject(a, b);
   Abort;
end;

procedure TExternalFunctionTests.RegisterExternalRoutines(const manager : IdwsExternalFunctionsManager);
begin
   manager.RegisterExternalFunction('Blank', @Blank);
   manager.RegisterExternalFunction('Ints3', @Ints3);
   manager.RegisterExternalFunction('TestString', @TestString);
   manager.RegisterExternalFunction('TestStringExc', @TestStringExc);
   manager.RegisterExternalFunction('TestBool', @TestBool);
   manager.RegisterExternalFunction('TestStack', @TestStack);
   manager.RegisterExternalFunction('TestFloat', @TestFloat);
   manager.RegisterExternalFunction('TestObject', @TestObject);
   manager.RegisterExternalFunction('TestObjectExc', @TestObjectExc);
end;

procedure TExternalFunctionTests.Execution;
var
   source, expectedResult : TStringList;
   i : Integer;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   manager : IdwsExternalFunctionsManager;
   resultText, resultsFileName : String;
begin
   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         manager:=TExternalFunctionManager.Create;

         // TODO: IdwsExternalFunctionsManager being low-level
         // it shouldn't be exposed at the TDelphiWebScript level
         // (need to have a TComponent property be exposed there)
         FCompiler.Compiler.ExternalFunctionsManager:=manager;

         prog:=FCompiler.Compile(source.Text);
         CheckEquals('', prog.Msgs.AsInfo, FTests[i]);

         // TODO: ideally should happen before compilation
         // and registration should be able to be program-independent
         RegisterExternalRoutines(manager);

         exec:=prog.Execute;

         // TODO: make compiler program independent from manager
         FCompiler.Compiler.ExternalFunctionsManager:=nil;

         resultText:=exec.Result.ToString;
         if exec.Msgs.Count>0 then
            resultText:=resultText+#13#10'>>>> Error(s): '#13#10+exec.Msgs.AsInfo;

         resultsFileName:=ChangeFileExt(FTests[i], '.txt');
         if FileExists(resultsFileName) then begin
            expectedResult.LoadFromFile(resultsFileName);
            CheckEquals(expectedResult.Text, resultText, FTests[i]);
         end else CheckEquals('', resultText, FTests[i]);
         CheckEquals('', exec.Msgs.AsInfo, FTests[i]);

      end;

   finally
      expectedResult.Free;
      source.Free;
   end;
end;

procedure TExternalFunctionTests.SetUp;
var
   fun: TdwsFunction;
   tbs: TdwsClass;
begin
   FTests:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'External'+PathDelim, '*.pas', FTests);

   FCompiler:=TDelphiWebScript.Create(nil);
   FUnit := TdwsUnit.Create(FCompiler);
   FUnit.ParseName := pnAlways;
   FUnit.UnitName := 'Helper';
   FUnit.script := FCompiler;

   tbs := FUnit.Classes.Add;
   tbs.Name := 'TBoxedString';
   tbs.OnCleanUp := self.FreeBoxedString;

   fun := FUnit.Functions.Add;
   fun.Name := 'GetBoxedString: TBoxedString';
   fun.OnEval := self.GetBoxedStringEval;
   FUnit.ImplicitUse := true;
end;

procedure TExternalFunctionTests.TearDown;
begin
   FCompiler.Free;

   FTests.Free;
end;

procedure TExternalFunctionTests.GetBoxedStringEval(info : TProgramInfo);
var
   str: TBoxedString;
begin
   str := TBoxedString.Create;
   str.value := 'Boxed String';
   info.ResultAsVariant := info.RegisterExternalObject(str, true);
end;

procedure TExternalFunctionTests.FreeBoxedString(ExternalObject: TObject);
begin
   ExternalObject.free;
end;

procedure TExternalFunctionTests.Test;
begin
   Compilation;
   Execution;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('dwsExternalFunctionTests', TExternalFunctionTests);

end.
