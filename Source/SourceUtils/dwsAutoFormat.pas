unit dwsAutoFormat;

{$I ../dws.inc}

interface

uses
   System.Classes, System.SysUtils,
   dwsUtils, dwsTokenizer, dwsCodeDOM, dwsScriptSource, dwsCodeDOMParser;

type

   TdwsAutoFormat = class
      private
         FIndentChar : Char;
         FIndentSize : Integer;
         FParser : TdwsParser;

      protected

      public
         constructor Create(aParser : TdwsParser);
         destructor Destroy; override;

         function Process(const sourceCode : String) : String; overload;
         function Process(const sourceFile : TSourceFile) : String; overload;

         property Parser : TdwsParser read FParser;

         property IndentChar : Char read FIndentChar write FIndentChar;
         property IndentSize : Integer read FIndentSize write FIndentSize;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsErrors;

// ------------------
// ------------------ TdwsAutoFormat ------------------
// ------------------

// Create
//
constructor TdwsAutoFormat.Create(aParser : TdwsParser);
begin
   inherited Create;
   IndentChar := #9;
   IndentSize := 1;
   FParser := aParser;
end;

// Destroy
//
destructor TdwsAutoFormat.Destroy;
begin
   inherited;
   FParser.Free;
end;

// Process (String)
//
function TdwsAutoFormat.Process(const sourceCode : String) : String;
begin
   var sourceFile := TSourceFile.Create;
   try
      sourceFile.Code := sourceCode;
      Result := Process(sourceFile);
   finally
      sourceFile.Free;
   end;
end;

// Process (TSourceFile)
//
function TdwsAutoFormat.Process(const sourceFile : TSourceFile) : String;
begin
   var dom := Parser.Parse(sourceFile);
   try
      if Parser.Messages.Count > 0 then
         Exit(sourceFile.Code);
      try
         var output := TdwsCodeDOMOutput.Create;
         try
            output.IndentChar := IndentChar;
            output.IndentSize := IndentSize;
            if dom.Root <> nil then
               dom.Root.WriteToOutput(output);
            Result := output.ToString;
         finally
            output.Free;
         end;
      except
         on E: Exception do
            Parser.Messages.AddInfo(E.ClassName + ': ' + E.Message);
      end;
   finally
      dom.Free;
   end;
end;

end.

