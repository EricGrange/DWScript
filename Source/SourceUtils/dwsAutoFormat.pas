unit dwsAutoFormat;

{$I ../dws.inc}

interface

uses
   Classes, SysUtils,
   dwsUtils, dwsTokenizer, dwsCodeDOM, dwsScriptSource;
type

   TdwsAutoFormat = class
      private
         FIndent : String;
         FRules : TTokenizerRules;

      protected

      public
         constructor Create(aRules : TTokenizerRules);
         destructor Destroy; override;

         function Process(const sourceCode : String) : String; overload;
         function Process(const sourceFile : TSourceFile) : String; overload;

         property Rules : TTokenizerRules read FRules;
         property Indent : String read FIndent write FIndent;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsAutoFormat ------------------
// ------------------

// Create
//
constructor TdwsAutoFormat.Create(aRules : TTokenizerRules);
begin
   inherited Create;
   Indent := #9;
   FRules := aRules;
end;

// Destroy
//
destructor TdwsAutoFormat.Destroy;
begin
   inherited;
   FRules.Free;
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
   var dom := TdwsCodeDOM.Create(Rules.CreateTokenizer(nil, nil));
   try
      dom.Parse(sourceFile);
      var output := TdwsCodeDOMOutput.Create;
      try
         output.Indent := Indent;
         dom.Root.WriteToOutput(output);
         Result := output.ToString;
      finally
         output.Free;
      end;
   finally
      dom.Free;
   end;
end;

end.

