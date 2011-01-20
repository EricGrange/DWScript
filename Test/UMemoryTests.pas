unit UMemoryTests;

interface

uses Windows, Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs,
   dwsComConnector, Variants, ActiveX, ComObj, dwsXPlatform, dwsUtils;

type

   TMemoryTests = class (TTestCase)
      private
         FTests : TStringList;
         FCompiler : TDelphiWebScript;
         FUnits : TdwsUnit;
         FTestObjects : TList;

         procedure DoCreateExternal(Info: TProgramInfo; var ExtObject: TObject);
         procedure DoCreateBoomExternal(Info: TProgramInfo; var ExtObject: TObject);
         procedure DoCleanupExternal(externalObject : TObject);

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure Execution;
         procedure Compilation;

      published

         procedure CompilationNormal;
         procedure CompilationWithMapAndSymbols;
         procedure ExecutionNonOptimized;
         procedure ExecutionOptimized;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   TTestObject = class
   end;

// ------------------
// ------------------ TMemoryTests ------------------
// ------------------

// SetUp
//
procedure TMemoryTests.SetUp;
var
   cls : TdwsClass;
   cst : TdwsConstructor;
begin
   FTests:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'Memory'+PathDelim, '*.pas', FTests);

   FCompiler:=TDelphiWebScript.Create(nil);
   FUnits:=TdwsUnit.Create(nil);
   FUnits.UnitName:='TestUnit';
   FUnits.Script:=FCompiler;

   cls:=FUnits.Classes.Add as TdwsClass;
   cls.Name:='TExposedClass';
   cls.OnCleanUp:=DoCleanupExternal;
   cst:=cls.Constructors.Add as TdwsConstructor;
   cst.Name:='Create';
   cst.OnEval:=DoCreateExternal;

   cls:=FUnits.Classes.Add as TdwsClass;
   cls.Name:='TExposedBoomClass';
   cls.OnCleanUp:=DoCleanupExternal;
   cst:=cls.Constructors.Add as TdwsConstructor;
   cst.Name:='Create';
   cst.OnEval:=DoCreateBoomExternal;

   FTestObjects:=TList.Create;
end;

// TearDown
//
procedure TMemoryTests.TearDown;
begin
   FTestObjects.Free;

   FUnits.Free;
   FCompiler.Free;

   FTests.Free;
end;

// DoCreateExternal
//
procedure TMemoryTests.DoCreateExternal(Info: TProgramInfo; var ExtObject: TObject);
begin
   ExtObject:=TTestObject.Create;
   FTestObjects.Add(ExtObject);
end;

// DoCreateBoomExternal
//
procedure TMemoryTests.DoCreateBoomExternal(Info: TProgramInfo; var ExtObject: TObject);
begin
   raise ETestFailure.Create('boom');
end;

// DoCleanupExternal
//
procedure TMemoryTests.DoCleanupExternal(externalObject : TObject);
begin
   if externalObject<>nil then begin
      CheckTrue(FTestObjects.IndexOf(externalObject)>=0, 'Invalid object ref');
      FTestObjects.Remove(externalObject);
      externalObject.Free;
   end;
end;

// Compilation
//
procedure TMemoryTests.Compilation;
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

// CompilationNormal
//
procedure TMemoryTests.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Compilation;
end;

// CompilationWithMapAndSymbols
//
procedure TMemoryTests.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=[coSymbolDictionary, coContextMap, coAssertions];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TMemoryTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coAssertions];
   Execution;
end;

// ExecutionOptimized
//
procedure TMemoryTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize, coAssertions];
   Execution;
end;

// Execution
//
procedure TMemoryTests.Execution;
var
   source, expectedResult : TStringList;
   i : Integer;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   resultsFileName : String;
begin
   FTestObjects.Clear;

   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);

         CheckEquals('', prog.Msgs.AsInfo, FTests[i]);
         exec:=prog.Execute;
         resultsFileName:=ChangeFileExt(FTests[i], '.txt');
         if FileExists(resultsFileName) then begin
            expectedResult.LoadFromFile(resultsFileName);
            CheckEquals(expectedResult.Text, exec.Result.ToString, FTests[i]);
         end else CheckEquals('', exec.Result.ToString, FTests[i]);
         CheckEquals('', exec.Msgs.AsInfo, FTests[i]);
         CheckEquals(0, exec.ObjectCount, FTests[i]+', leaked '+IntToStr(exec.ObjectCount)+' script objects');
         CheckEquals(0, FTestObjects.Count, FTests[i]+', leaked '+IntToStr(FTestObjects.Count)+' external objects');

      end;

   finally
      expectedResult.Free;
      source.Free;
   end;
end;



// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TestFramework.RegisterTest('MemoryTests', TMemoryTests.Suite);

end.
