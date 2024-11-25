unit UAutoFixTests;

{$I ..\Source\dws.inc}

interface

uses
   System.Classes, System.SysUtils, System.Variants,
   dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsDataContext,
   dwsTokenizer, dwsErrors, dwsUtils, dwsSymbols, dwsSuggestions,
   dwsFunctions, dwsCaseNormalizer, dwsScriptSource, dwsSymbolDictionary,
   dwsCompilerContext, dwsUnicode, dwsJSONConnector, dwsUnitSymbols;

type

   TAutoFixTests = class (TTestCase)
      private
         FCompiler : TDelphiWebScript;

      public
         procedure SetUp; override;
         procedure TearDown; override;

      published
         procedure AddImplemProcedure;
         procedure AddImplemMethod;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   TStringsBuffer = class (TInterfacedObject, IAutoFixSourceBuffer)
      protected
         FStrings : TStrings;
         FCursorCol : Integer;
         FCursorRow : Integer;

      public
         constructor Create;
         destructor Destroy; override;

         function GetLines(idx : Integer) : String;
         procedure SetLines(idx : Integer; const newLine : String);
         function GetCount : Integer;

         property Lines[idx : Integer] : String read GetLines write SetLines;
         property Count : Integer read GetCount;

         function CodeAt(col, line, nb : Integer) : String;
         procedure ReplaceAt(col, line, nb : Integer; const newText : String);
         procedure InsertLine(line : Integer; const lineText : String);
         procedure DeleteLine(line : Integer);

         procedure MoveCursorTo(col, row : Integer);
         procedure MoveCursorToMark;
   end;

// Create
//
constructor TStringsBuffer.Create;
begin
   inherited;
   FStrings := TStringList.Create;
end;

// Destroy
//
destructor TStringsBuffer.Destroy;
begin
   inherited;
   FreeAndNil(FStrings);
end;

// GetLines
//
function TStringsBuffer.GetLines(idx : Integer) : String;
begin
   Result := FStrings[idx];
end;

// SetLines
//
procedure TStringsBuffer.SetLines(idx : Integer; const newLine : String);
begin
   FStrings[idx] := newLine;
end;

// GetCount
//
function TStringsBuffer.GetCount : Integer;
begin
   Result := FStrings.Count;
end;

// CodeAt
//
function TStringsBuffer.CodeAt(col, line, nb : Integer) : String;
begin
   Result := Copy(FStrings[line], col, nb);
end;

// ReplaceAt
//
procedure TStringsBuffer.ReplaceAt(col, line, nb : Integer; const newText : String);
begin
   FStrings[line] := Copy(FStrings[line], 1, col-1) + newText + Copy(FStrings[line], col + nb);
end;

// InsertLine
//
procedure TStringsBuffer.InsertLine(line : Integer; const lineText : String);
begin
   FStrings.Insert(line, lineText);
end;

// DeleteLine
//
procedure TStringsBuffer.DeleteLine(line : Integer);
begin
   FStrings.Delete(line);
end;

// MoveCursorTo
//
procedure TStringsBuffer.MoveCursorTo(col, row : Integer);
begin
   FCursorCol := col;
   FCursorRow := row;
end;

// MoveCursorToMark
//
procedure TStringsBuffer.MoveCursorToMark;
begin
   FCursorCol := 0;
   FCursorRow := 0;
end;

// ------------------
// ------------------ TAutoFixTests ------------------
// ------------------

// SetUp
//
procedure TAutoFixTests.SetUp;
begin
   FCompiler := TDelphiWebScript.Create(nil);
   FCompiler.Config.CompilerOptions := FCompiler.Config.CompilerOptions + [coSymbolDictionary, coContextMap];
end;

// TearDown
//
procedure TAutoFixTests.TearDown;
begin
   FCompiler.Free;
end;

// AddImplemProcedure
//
procedure TAutoFixTests.AddImplemProcedure;
var
   prog : IdwsProgram;
   buffer : TStringsBuffer;
   autoBuffer : IAutoFixSourceBuffer;
begin
   buffer := TStringsBuffer.Create;
   autoBuffer := buffer;
   buffer.FStrings.Text :=
      '''
      procedure Test; forward;
      procedure Hello; begin end;
      ''';

   prog := FCompiler.Compile(buffer.FStrings.Text);

   CheckEquals('Syntax Error: The function "Test" was forward declared but not implemented [line: 1, column: 11]'#13#10, prog.Msgs.AsInfo);

   var sm := (prog.Msgs[0] as TScriptMessage);
   sm.AutoFix.Apply(autoBuffer);

   CheckEquals(
        'procedure Test; forward;'#13#10
      + 'procedure Hello; begin end;'#13#10
      + #13#10
      + 'procedure Test;'#13#10
      + 'begin'#13#10
         + #9'|'#13#10
      + 'end;'#13#10
      , buffer.FStrings.Text
   );
end;

// AddImplemMethod
//
procedure TAutoFixTests.AddImplemMethod;
var
   prog : IdwsProgram;
   buffer : TStringsBuffer;
   autoBuffer : IAutoFixSourceBuffer;
begin
   buffer := TStringsBuffer.Create;
   autoBuffer := buffer;
   buffer.FStrings.Text :=
      '''
      type TTest = class
         class function Hello(a : Integer) : String;
      end;
      ''';

   prog := FCompiler.Compile(buffer.FStrings.Text);

   CheckEquals('Syntax Error: Method "Hello" of class "TTest" not implemented [line: 2, column: 19]'#13#10, prog.Msgs.AsInfo);

   var sm := (prog.Msgs[0] as TScriptMessage);
   sm.AutoFix.Apply(autoBuffer);

   CheckEquals(
        'type TTest = class'#13#10
      + '   class function Hello(a : Integer) : String;'#13#10
      + 'end;'#13#10
      + #13#10
      + 'class function TTest.Hello(a: Integer): String;'#13#10
      + 'begin'#13#10
         + #9'|'#13#10
      + 'end;'#13#10
      , buffer.FStrings.Text
   );
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('AutoFixTests', TAutoFixTests);

end.
