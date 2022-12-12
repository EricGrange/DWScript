{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsCodeDOM;

{$I ../dws.inc}

interface

uses
   Classes, SysUtils,
   dwsUtils, dwsTokenizer;

type

   TdwsCodeDOMOutput = class
      private
         FStream : TWriteOnlyBlockStream;
         FIndent : String;
         FIndentBuf : String;
         FIndentDepth : Integer;

      protected

      public
         constructor Create;
         destructor Destroy; override;

         procedure WriteString(const s : String);
         procedure WriteNewLine;

         property Indent : String read FIndent write FIndent;
         property IndentDepth : Integer read FIndentDepth;

         procedure IncIndent;
         procedure DecIndent;

         function ToString : String; override;
   end;

   TdwsCodeDOMNode = class (TRefCountedObject)
      private
         FParent : TdwsCodeDOMNode;
         FChildren : TObjectList<TdwsCodeDOMNode>;

      protected
         function GetChild(idx : Integer) : TdwsCodeDOMNode;

      public
         constructor Create;
         destructor Destroy; override;

         procedure WriteToOutput(output : TdwsCodeDOMOutput); virtual;

         property Parent : TdwsCodeDOMNode read FParent;
         property Child[idx : Integer] : TdwsCodeDOMNode read GetChild;
         function ChildCount : Integer;
   end;

   TdwsCodeDOM = class (TdwsCodeDOMNode)
      private
         FTokenizer : TTokenizer;
         FSourceCode : String;

      protected

      public
         constructor Create(aTokenizer : TTokenizer);
         destructor Destroy; override;

         procedure Parse(const sourceCode : String);

   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsCodeDOMOutput ------------------
// ------------------

// Create
//
constructor TdwsCodeDOMOutput.Create;
begin
   inherited;
   FStream := TWriteOnlyBlockStream.AllocFromPool;
end;

// Destroy
//
destructor TdwsCodeDOMOutput.Destroy;
begin
   inherited;
   FStream.ReturnToPool;
end;

// WriteString
//
procedure TdwsCodeDOMOutput.WriteString(const s : String);
begin
   FStream.WriteString(s);
end;

// WriteNewLine
//
procedure TdwsCodeDOMOutput.WriteNewLine;
begin
   FStream.WriteChar(#10);
   FStream.WriteString(FIndentBuf);
end;

// IncIndent
//
procedure TdwsCodeDOMOutput.IncIndent;
begin
   Inc(FIndentDepth);
   FIndentBuf := FIndentBuf + FIndent;
end;

// DecIndent
//
procedure TdwsCodeDOMOutput.DecIndent;
begin
   Assert(FIndentDepth > 0);
   Dec(FIndentDepth);
   SetLength(FIndentBuf, Length(FIndentBuf) - Length(FIndent));
end;

// ToString
//
function TdwsCodeDOMOutput.ToString : String;
begin
   Result := FStream.ToString;
end;

// ------------------
// ------------------ TdwsCodeDOMNode ------------------
// ------------------

// Create
//
constructor TdwsCodeDOMNode.Create;
begin
   inherited;
   FChildren := TObjectList<TdwsCodeDOMNode>.Create;
end;

// Destroy
//
destructor TdwsCodeDOMNode.Destroy;
begin
   inherited;
   FChildren.Free;
end;

// WriteToOutput
//
procedure TdwsCodeDOMNode.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   for var i := 0 to FChildren.Count-1 do
      FChildren[i].WriteToOutput(output);
end;

// GetChild
//
function TdwsCodeDOMNode.GetChild(idx : Integer) : TdwsCodeDOMNode;
begin
   Result := FChildren[idx];
end;

// ChildCount
//
function TdwsCodeDOMNode.ChildCount : Integer;
begin
   Result := FChildren.Count;
end;

// ------------------
// ------------------ TdwsCodeDOM ------------------
// ------------------

// Create
//
constructor TdwsCodeDOM.Create(aTokenizer : TTokenizer);
begin
   inherited Create;
   FTokenizer := aTokenizer;
end;

// Destroy
//
destructor TdwsCodeDOM.Destroy;
begin
   inherited;
   FTokenizer.Free;
end;

// Parse
//
procedure TdwsCodeDOM.Parse(const sourceCode : String);
begin
   FSourceCode := sourceCode;
   // todo !
end;

end.
