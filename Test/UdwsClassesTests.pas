unit UdwsClassesTests;

interface

uses Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs,
   dwsClassesLibModule;

type

   TdwsClassesTests = class (TTestCase)
      private
         FTests : TStringList;
         FCompiler : TDelphiWebScript;
         FClassesLib : TdwsClassesLib;

      public
         procedure CollectFiles(const directory, fileMask : String; list : TStrings);

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

// ------------------
// ------------------ TdwsClassesTests ------------------
// ------------------

// CollectFiles
//
procedure TdwsClassesTests.CollectFiles(const directory, fileMask : String; list : TStrings);
var
   searchRec : TSearchRec;
   found : Integer;
begin
   found:=FindFirst(directory+'*.pas', faArchive or faReadOnly or faHidden, searchRec);
   while found=0 do begin
      if (searchRec.Attr and faDirectory)=0 then begin
         list.Add(directory+searchRec.Name);
      end;
      found:=FindNext(searchRec);
   end;
   FindClose(searchRec);
end;

// SetUp
//
procedure TdwsClassesTests.SetUp;
begin
   FTests:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'ClassesLib'+PathDelim, '*.pas', FTests);

   FCompiler:=TDelphiWebScript.Create(nil);

   FClassesLib:=TdwsClassesLib.Create(nil);
   FClassesLib.Script:=FCompiler;
end;

// TearDown
//
procedure TdwsClassesTests.TearDown;
begin
   FClassesLib.Free;

   FCompiler.Free;

   FTests.Free;
end;

// Compilation
//
procedure TdwsClassesTests.Compilation;
var
   source : TStringList;
   i : Integer;
   prog : TdwsProgram;
begin
   source:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);
         try
            CheckEquals('', prog.Msgs.AsInfo, FTests[i]);
         finally
            prog.Free;
         end;

      end;

   finally
      source.Free;
   end;
end;

// CompilationNormal
//
procedure TdwsClassesTests.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Compilation;
end;

// CompilationWithMapAndSymbols
//
procedure TdwsClassesTests.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=[coSymbolDictionary, coContextMap];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TdwsClassesTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=[];
   Execution;
end;

// ExecutionOptimized
//
procedure TdwsClassesTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Execution;
end;

// Execution
//
procedure TdwsClassesTests.Execution;
var
   source, expectedResult : TStringList;
   i : Integer;
   prog : TdwsProgram;
   resultsFileName : String;
begin
   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);
         try
            CheckEquals('', prog.Msgs.AsInfo, FTests[i]);
            prog.Execute;
            resultsFileName:=ChangeFileExt(FTests[i], '.txt');
            if FileExists(resultsFileName) then begin
               expectedResult.LoadFromFile(resultsFileName);
               CheckEquals(expectedResult.Text, (prog.Result as TdwsDefaultResult).Text, FTests[i]);
            end else CheckEquals('', (prog.Result as TdwsDefaultResult).Text, FTests[i]);
            CheckEquals('', prog.Msgs.AsInfo, FTests[i]);
         finally
            prog.Free;
         end;

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

   TestFramework.RegisterTest('dwsClassesLibTests', TdwsClassesTests.Suite);

end.
