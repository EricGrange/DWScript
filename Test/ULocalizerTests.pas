unit ULocalizerTests;

interface

uses Windows, Classes, SysUtils, dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs,
   dwsTokenizer, dwsXPlatform, dwsFileSystem, dwsErrors, dwsUtils, Variants,
   dwsSymbols, dwsPascalTokenizer, dwsStrings, dwsStack, dwsJSON;

type

   TLocalizerTests = class (TTestCase)
      private
         FCompiler : TDelphiWebScript;

      public
         procedure SetUp; override;
         procedure TearDown; override;
         procedure DoLocalize(Sender : TObject; const aString : UnicodeString; var Result : UnicodeString);

      published
         procedure SimpleTest;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TLocalizerTests ------------------
// ------------------

// SetUp
//
procedure TLocalizerTests.SetUp;
begin
   FCompiler:=TDelphiWebScript.Create(nil);
end;

// TearDown
//
procedure TLocalizerTests.TearDown;
begin
   FCompiler.Free;
end;

// DoLocalize
//
procedure TLocalizerTests.DoLocalize(Sender : TObject; const aString : UnicodeString; var Result : UnicodeString);
begin
   Result:='['+aString+']';
end;

// SimpleTest
//
procedure TLocalizerTests.SimpleTest;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   customLoc : TdwsCustomLocalizer;
begin
   customLoc:=TdwsCustomLocalizer.Create(nil);
   try
      customLoc.OnLocalizeString:=DoLocalize;
      FCompiler.Config.Localizer:=customLoc;
      prog:=FCompiler.Compile('resourcestring hello="hello"; Print(hello);');
      CheckEquals(0, prog.Msgs.Count, 'compile errors');

      exec:=prog.CreateNewExecution;
      exec.Execute;
      CheckEquals('[hello]', exec.Result.ToUnicodeString, 'localized');
      exec.Localizer:=nil;
      exec.Execute;
      CheckEquals('hello', exec.Result.ToUnicodeString, 'localizer off');

      exec:=prog.CreateNewExecution;
      exec.Execute;
      CheckEquals('[hello]', exec.Result.ToUnicodeString, 'localized2');
   finally
      customLoc.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('LocalizerTests', TLocalizerTests);

end.
