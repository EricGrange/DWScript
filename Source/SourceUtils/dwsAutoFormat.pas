unit dwsAutoFormat;

{$I ../dws.inc}

interface

uses
   Classes, SysUtils,
   dwsUtils, dwsTokenizer, dwsCodeDOM;

type

   TdwsAutoFormat = class
      private
         FIndent : String;
         FRules : TTokenizerRules;

      protected

      public
         constructor Create(aRules : TTokenizerRules);
         destructor Destroy; override;

         function Process(const sourceCode : String) : String;

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

// Process
//
function TdwsAutoFormat.Process(const sourceCode : String) : String;
begin
   var dom := TdwsCodeDOM.Create(Rules.CreateTokenizer(nil, nil));
   try
      dom.Parse(sourceCode);
      var output := TdwsCodeDOMOutput.Create;
      try
         output.Indent := Indent;
         dom.WriteToOutput(output);
         Result := output.ToString;
      finally
         output.Free;
      end;
   finally
      dom.Free;
   end;
end;

end.
