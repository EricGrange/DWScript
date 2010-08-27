unit UAlgorithmsTests;

interface

uses Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs;

type

   TAlgorithmsTests = class (TTestCase)
      private
         FTests : TStringList;
         FCompiler : TDelphiWebScript;

      public
         procedure CollectFiles(const directory, fileMask : String; list : TStrings);

         procedure SetUp; override;
         procedure TearDown; override;

         procedure Execution;

      published

         procedure Compilation;
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
// ------------------ TAlgorithmsTests ------------------
// ------------------

// CollectFiles
//
procedure TAlgorithmsTests.CollectFiles(const directory, fileMask : String; list : TStrings);
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
procedure TAlgorithmsTests.SetUp;
begin
   FTests:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'Algorithms'+PathDelim, '*.pas', FTests);

   FCompiler:=TDelphiWebScript.Create(nil);
end;

// TearDown
//
procedure TAlgorithmsTests.TearDown;
begin
   FCompiler.Free;

   FTests.Free;
end;

// Compilation
//
procedure TAlgorithmsTests.Compilation;
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

// ExecutionNonOptimized
//
procedure TAlgorithmsTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=[];
   Execution;
end;

// ExecutionOptimized
//
procedure TAlgorithmsTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Execution;
end;

// Execution
//
procedure TAlgorithmsTests.Execution;
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

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TestFramework.RegisterTest('AlgorithmsTests', TAlgorithmsTests.Suite);

end.
