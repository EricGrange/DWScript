unit dwsAutoFormat;

{$I ../dws.inc}

interface

uses
   Classes, SysUtils,
   dwsUtils, dwsTokenizer, dwsCodeDOM, dwsScriptSource, dwsCodeDOMParser;

type

   TdwsAutoFormat = class
      private
         FIndent : String;
         FParser : TdwsParser;

      protected

      public
         constructor Create(aParser : TdwsParser);
         destructor Destroy; override;

         function Process(const sourceCode : String) : String; overload;
         function Process(const sourceFile : TSourceFile) : String; overload;

         property Parser : TdwsParser read FParser;

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
constructor TdwsAutoFormat.Create(aParser : TdwsParser);
begin
   inherited Create;
   Indent := #9;
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
      var output := TdwsCodeDOMOutput.Create;
      try
         output.Indent := Indent;
         if dom.Root <> nil then
            dom.Root.WriteToOutput(output);
         Result := output.ToString;
      finally
         output.Free;
      end;
   finally
      dom.Free;
   end;

//   var dom := TdwsCodeDOM.Create(Rules.CreateTokenizer(nil, nil));
//   try
//      dom.Parse(sourceFile);
//      var output := TdwsCodeDOMOutput.Create;
//      try
//         output.Indent := Indent;
//         dom.Root.WriteToOutput(output);
//         Result := output.ToString;
//      finally
//         output.Free;
//      end;
//   finally
//      dom.Free;
//   end;
end;

end.

