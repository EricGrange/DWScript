unit UScriptTests;

interface

uses Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs;

type

   TScriptTests = class (TTestCase)
      private
         FTests : TStringList;
         FFailures : TStringList;
         FCompiler : TDelphiWebScript;

      public
         procedure CollectFiles(const directory, fileMask : String; list : TStrings);

         procedure SetUp; override;
         procedure TearDown; override;

      published

         procedure Compilation;
         procedure Execution;
         procedure CompilationFailure;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TScriptTests ------------------
// ------------------

// CollectFiles
//
procedure TScriptTests.CollectFiles(const directory, fileMask : String; list : TStrings);
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
procedure TScriptTests.SetUp;
begin
   FTests:=TStringList.Create;
   FFailures:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'SimpleScripts'+PathDelim, '*.pas', FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'FailureScripts'+PathDelim, '*.pas', FFailures);

   FCompiler:=TDelphiWebScript.Create(nil);
end;

// TearDown
//
procedure TScriptTests.TearDown;
begin
   FCompiler.Free;

   FTests.Free;
   FFailures.Free;
end;

// Compilation
//
procedure TScriptTests.Compilation;
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

// Execution
//
procedure TScriptTests.Execution;
var
   source, exprectedResult : TStringList;
   i : Integer;
   prog : TdwsProgram;
   resultsFileName : String;
begin
   source:=TStringList.Create;
   exprectedResult:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);
         try
            CheckEquals('', prog.Msgs.AsInfo, FTests[i]);
            prog.Execute;
            resultsFileName:=ChangeFileExt(FTests[i], '.txt');
            if FileExists(resultsFileName) then begin
               exprectedResult.LoadFromFile(resultsFileName);
               CheckEquals(exprectedResult.Text, (prog.Result as TdwsDefaultResult).Text, FTests[i]);
            end else CheckEquals('', (prog.Result as TdwsDefaultResult).Text, FTests[i]);
         finally
            prog.Free;
         end;

      end;

   finally
      exprectedResult.Free;
      source.Free;
   end;
end;

// CompilationFailure
//
procedure TScriptTests.CompilationFailure;
var
   source : TStringList;
   i : Integer;
   prog : TdwsProgram;
begin
   source:=TStringList.Create;
   try

      for i:=0 to FFailures.Count-1 do begin

         source.LoadFromFile(FFailures[i]);

         prog:=FCompiler.Compile(source.Text);
         try
            Check(prog.Msgs.AsInfo<>'', FFailures[i]+': undetected error');
         finally
            prog.Free;
         end;

      end;

   finally
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

   TestFramework.RegisterTest('ScriptTests', TScriptTests.Suite);

end.
