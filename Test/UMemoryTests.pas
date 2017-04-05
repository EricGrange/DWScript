unit UMemoryTests;

interface

uses
   Classes, SysUtils, Types, Variants, ComObj,
   dwsXPlatform, dwsUtils, dwsCompilerContext,
   dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsSymbols;

type

   TMemoryTests = class (TTestCase)
      private
         FTests : TStringList;
         FCompiler : TDelphiWebScript;
         FUnits : TdwsUnit;
         FTestObjects : TList;
         FExternalRef : IScriptObj;

         procedure DoCreateExternal(Info: TProgramInfo; var ExtObject: TObject);
         procedure DoCreateBoomExternal(Info: TProgramInfo; var ExtObject: TObject);
         procedure DoCleanupExternal(externalObject : TObject);
         procedure DoKeepExternalRef(Info: TProgramInfo; ExtObject: TObject);

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

         procedure KeepExternalRefAfterExecution;
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
      private
         FAlreadyDestroyed : Boolean;
      public
         constructor Create;
         destructor Destroy; override;
   end;

var
   vTestObjetCount : Integer;

// Create
//
constructor TTestObject.Create;
begin
   inherited;
   Inc(vTestObjetCount);
end;

// Destroy
//
destructor TTestObject.Destroy;
begin
   Assert(not FAlreadyDestroyed);
   FAlreadyDestroyed:=True;
   Dec(vTestObjetCount);
   inherited;
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
   meth : TdwsMethod;
begin
   FTests:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'Memory'+PathDelim, '*.pas', FTests);

   FCompiler:=TDelphiWebScript.Create(nil);
   FUnits:=TdwsUnit.Create(nil);
   FUnits.UnitName:='TestUnit';
   FUnits.Script:=FCompiler;

   cls:=FUnits.Classes.Add;
   cls.Name:='TExposedClass';
   cls.OnCleanUp:=DoCleanupExternal;
   cst:=cls.Constructors.Add as TdwsConstructor;
   cst.Name:='Create';
   cst.OnEval:=DoCreateExternal;

   meth:=cls.Methods.Add as TdwsMethod;
   meth.Name:='KeepExternalRef';
   meth.OnEval:=DoKeepExternalRef;

   cls:=FUnits.Classes.Add;
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

// DoKeepExternalRef
//
procedure TMemoryTests.DoKeepExternalRef(Info: TProgramInfo; ExtObject: TObject);
begin
   // don't do that at home, you're not really supposed to,
   // it's just to make sure things won't bomb even if you do
   FExternalRef:=Info.Vars['Self'].ScriptObj;
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
            CheckEquals(expectedResult.Text, exec.Result.ToUnicodeString, FTests[i]);
         end else CheckEquals('', exec.Result.ToUnicodeString, FTests[i]);
         CheckEquals('', exec.Msgs.AsInfo, FTests[i]);
         CheckEquals(0, exec.ObjectCount, FTests[i]+', leaked '+IntToStr(exec.ObjectCount)+' script objects');
         CheckEquals(0, FTestObjects.Count, FTests[i]+', leaked '+IntToStr(FTestObjects.Count)+' external objects');
         CheckEquals(0, vTestObjetCount, 'test object count');

      end;

   finally
      expectedResult.Free;
      source.Free;
   end;
end;

// KeepExternalRefAfterExecution
//
procedure TMemoryTests.KeepExternalRefAfterExecution;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=FCompiler.Compile('var v = TExposedClass.Create; v.KeepExternalRef;');
   CheckEquals('', prog.Msgs.AsInfo, 'compile');

   exec:=prog.BeginNewExecution;
   try
      exec.RunProgram(0);

      CheckEquals('', exec.Msgs.AsInfo, 'run');
      CheckEquals(1, exec.ObjectCount, 'object count');
      CheckTrue(FExternalRef<>nil, 'got external ref');
      CheckEquals('TExposedClass', FExternalRef.ClassSym.Name, 'external ref classname');
      CheckEquals(False, (FExternalRef.ExternalObject as TTestObject).FAlreadyDestroyed, 'external ref ok');
   finally
      exec.EndProgram;
   end;

   CheckEquals(0, exec.ObjectCount, 'object count after exec ended');
   CheckEquals(1, vTestObjetCount, 'test object count after exec ended');
   CheckTrue(FExternalRef.ClassSym=nil, 'external ref disconnected');
   CheckEquals(False, (FExternalRef.ExternalObject as TTestObject).FAlreadyDestroyed, 'external not dead just yet');

   exec:=nil;

   CheckEquals(1, vTestObjetCount, 'test object count after exec death');
   CheckTrue(FExternalRef.ClassSym=nil, 'external ref disconnected after death');
   CheckEquals(False, (FExternalRef.ExternalObject as TTestObject).FAlreadyDestroyed, 'external still not dead');

   FExternalRef:=nil;

   CheckEquals(0, vTestObjetCount, 'test object count after external ref dropped');
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('MemoryTests', TMemoryTests);

end.
