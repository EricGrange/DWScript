unit UJSSmartLinkerTests;

interface

uses
  Forms, Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs,
  dwsXPlatform, dwsUtils, dwsJSLibModule, StrUtils, dwsFunctions,
  dwsJSCodeGen, dwsCodeGen;

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

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TestFrameWork.RegisterTest('SmartLinkerTests', TJSSmartLinkerTests.Suite);

end.
