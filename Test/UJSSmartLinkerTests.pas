unit UJSSmartLinkerTests;

interface

uses
  Forms, Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs,
  dwsXPlatform, dwsUtils, dwsJSLibModule, StrUtils, dwsFunctions, dwsErrors,
  dwsJSCodeGen, dwsCodeGen, dwsUnitSymbols, dwsCompilerContext;

type

   TJSSmartLinkerTests = class(TTestCase)
      private
         FJSCompiler: TDelphiWebScript;
         FJSCodeGen : TdwsJSCodeGen;
         FASMModule : TdwsJSLibModule;

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure DoInclude(const scriptName: string; var scriptSource: string);
         function  DoNeedUnit(const unitName : String; var unitSource : String) : IdwsUnit;

      published
         procedure ClassSimple;
         procedure AliasedRecord;
         procedure UnusedRecord;
         procedure AnonymousProc;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TJSSmartLinkerTests ------------------
// ------------------

// SetUp
//
procedure TJSSmartLinkerTests.SetUp;
begin
   FJSCompiler := TDelphiWebScript.Create(nil);
   FJSCompiler.Config.CompilerOptions:=[coVariablesAsVarOnly, coAllowClosures, coOptimize,
                                        coSymbolDictionary, coContextMap];
   FJSCompiler.OnInclude := DoInclude;
   FJSCompiler.OnNeedUnit := DoNeedUnit;

   FJSCodeGen := TdwsJSCodeGen.Create;
   FJSCodeGen.Options:=[cgoNoRangeChecks, cgoNoCheckInstantiated, cgoNoCheckLoopStep,
                        cgoNoConditions, cgoSmartLink];

   FASMModule:=TdwsJSLibModule.Create(nil);
   FASMModule.Script:=FJSCompiler;
end;

// TearDown
//
procedure TJSSmartLinkerTests.TearDown;
begin
   FJSCodeGen.Free;
   FASMModule.Free;
   FJSCompiler.Free;
end;

// DoInclude
//
procedure TJSSmartLinkerTests.DoInclude(const scriptName: string; var scriptSource: string);
var
   sl : TStringList;
begin
   sl := TStringList.Create;
   try
      sl.LoadFromFile('SimpleScripts\' + scriptName);
      scriptSource := sl.Text;
   finally
      sl.Free;
   end;
end;

// DoNeedUnit
//
function TJSSmartLinkerTests.DoNeedUnit(const unitName : String; var unitSource : String) : IdwsUnit;
var
   sl : TStringList;
begin
   sl := TStringList.Create;
   try
      sl.LoadFromFile('JSFilterScripts\' + unitName + '.pas');
      unitSource := sl.Text;
   finally
      sl.Free;
   end;
   Result:=nil;
end;

// ClassSimple
//
procedure TJSSmartLinkerTests.ClassSimple;
var
   buf : String;
   prog : IdwsProgram;
begin
   prog:=FJSCompiler.Compile(
       'type TClass1 = class'#13#10
      +'c1:string;'#13#10
      +'end;'#13#10
      +'type TClass2 = class'#13#10
      +'c2:string;'#13#10
      +'end;'#13#10
      +'var A : TClass1;'#13#10);
   CheckEquals(0, prog.Msgs.Count, prog.Msgs.AsInfo);

   FJSCodeGen.Clear;
   FJSCodeGen.CompileProgram(prog);

   buf:=FJSCodeGen.CompiledOutput(prog);

   CheckEquals(0, Pos('TClass2', buf), 'TClass2');
end;

// AliasedRecord
//
procedure TJSSmartLinkerTests.AliasedRecord;
var
   buf : String;
   prog : IdwsProgram;
begin
   prog:=FJSCompiler.Compile(
       'type TRec = record'#13#10
      +'x:Integer;'#13#10
      +'end;'#13#10
      +'type TAlias = TRec;'#13#10
      +'var A, B : TAlias;'#13#10
      +'a:=B;'#13#10);
   CheckEquals(0, prog.Msgs.Count, prog.Msgs.AsInfo);

   FJSCodeGen.Clear;
   FJSCodeGen.CompileProgram(prog);

   buf:=FJSCodeGen.CompiledOutput(prog);

   Check((Pos('TRec', buf)>0), 'TRec alias');
end;

// UnusedRecord
//
procedure TJSSmartLinkerTests.UnusedRecord;
var
   buf : String;
   prog : IdwsProgram;
begin
   prog:=FJSCompiler.Compile(
       'type TRec = record'#13#10
      +'x:Integer;'#13#10
      +'procedure Test; begin end;'#13#10
      +'end;'#13#10
      +'procedure Bogus(p : TRec);'#13#10
      +'var r : TRec;'#13#10
      +'begin end'#13#10
      );
   CheckEquals('Hint: Variable "r" declared but not used [line: 6, column: 5]'#13#10, prog.Msgs.AsInfo);

   FJSCodeGen.Clear;
   FJSCodeGen.CompileProgram(prog);

   buf:=FJSCodeGen.CompiledOutput(prog);

   Check((Pos('TRec', buf)<=0), 'TRec still present');
end;

// AnonymousProc
//
procedure TJSSmartLinkerTests.AnonymousProc;
var
   buf : String;
   prog : IdwsProgram;
begin
   prog:=FJSCompiler.Compile(
       'procedure Marker(i : Integer); begin  end;'#10
      +'procedure Test1; begin Marker(123); end;'#10
      +'procedure Test2; begin Marker(456); end;'#10
      +'procedure Test3; begin Marker(789); end;'#10
      +'var a external "abc" : Variant := class'#10
      +'field := procedure begin Test1 end;'#10
      +'end;'#10
      +'var b : Variant := class'#10
      +'field := procedure begin Test3 end;'#10
      +'end;'#10
      );

   FJSCodeGen.Clear;
   FJSCodeGen.CompileProgram(prog);

   buf:=FJSCodeGen.CompiledOutput(prog);

   Check((Pos('123', buf)> 0), 'Test1 missing');
   Check((Pos('456', buf)<=0), 'Test2 is still present');
   Check((Pos('789', buf)> 0), 'Test3 missing');
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TestFrameWork.RegisterTest('SmartLinkerTests', TJSSmartLinkerTests.Suite);

end.
