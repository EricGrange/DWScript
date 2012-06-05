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
{    Copyright Eric Grange / Creative IT                               }
{                                                                      }
{**********************************************************************}
unit dwsSynEditUtils;

{$I ../dws.inc}

interface

uses SynEdit, SynEditTypes, SynEditKeyCmds, dwsErrors;

type

   TSynEditSourceBuffer = class (TInterfacedObject, IAutoFixSourceBuffer)
      private
         FSynEdit : TCustomSynEdit;
         FMark : TBufferCoord;

      protected
         procedure EditorGoTo(col, row : Integer);

      public
         constructor Create(aSynEdit : TCustomSynEdit);

         function GetLines(idx : Integer) : String;
         procedure SetLines(idx : Integer; const newLine : String);
         function GetCount : Integer;

         function CodeAt(col, line, nb : Integer) : String;
         procedure ReplaceAt(col, line, nb : Integer; const newText : String);
         procedure InsertLine(line : Integer; const lineText : String);
         procedure DeleteLine(line : Integer);

         procedure MoveCursorTo(col, row : Integer);
         procedure MoveCursorToMark;
   end;

implementation

// ------------------
// ------------------ TSynEditSourceBuffer ------------------
// ------------------

// Create
//
constructor TSynEditSourceBuffer.Create(aSynEdit : TCustomSynEdit);
begin
   inherited Create;
   FSynEdit:=aSynEdit;
end;

// GetLines
//
function TSynEditSourceBuffer.GetLines(idx : Integer) : String;
begin
   if Cardinal(idx)<Cardinal(FSynEdit.Lines.Count) then
      Result:=FSynEdit.Lines[idx]
   else Result:=''
end;

// SetLines
//
procedure TSynEditSourceBuffer.SetLines(idx : Integer; const newLine : String);
begin
   Assert(idx>=0);
   while FSynEdit.Lines.Count<=idx do
      FSynEdit.Lines.Add('');
   FSynEdit.Lines[idx]:=newLine;
end;

// GetCount
//
function TSynEditSourceBuffer.GetCount : Integer;
begin
   Result:=FSynEdit.Lines.Count;
end;

// CodeAt
//
function TSynEditSourceBuffer.CodeAt(col, line, nb : Integer) : String;
begin
   Result:=Copy(GetLines(line), col, nb);
end;

// ReplaceAt
//
procedure TSynEditSourceBuffer.ReplaceAt(col, line, nb : Integer; const newText : String);
begin
   FSynEdit.BlockBegin:=BufferCoord(col, line);
   FSynEdit.BlockEnd:=BufferCoord(col+nb, line);
   FSynEdit.SelText:=newText;
end;

// InsertLine
//
procedure TSynEditSourceBuffer.InsertLine(line : Integer; const lineText : String);
var
   i : Integer;
   c : Char;
begin
   EditorGoTo(1, line);
   for i:=1 to Length(lineText) do begin
      c:=lineText[i];
      case c of
         #9 : FSynEdit.ExecuteCommand(ecTab, #0, nil);
         '|' : FMark:=FSynEdit.CaretXY;
      else
         FSynEdit.ExecuteCommand(ecChar, c, nil);
      end;
   end;
   FSynEdit.ExecuteCommand(ecLineBreak, #0, nil);
end;

// DeleteLine
//
procedure TSynEditSourceBuffer.DeleteLine(line : Integer);
begin
   EditorGoTo(1, line);
   FSynEdit.ExecuteCommand(ecDeleteLine, #0, nil);
end;

// MoveCursorTo
//
procedure TSynEditSourceBuffer.MoveCursorTo(col, row : Integer);
begin
   if (row<FSynEdit.TopLine) or (row>FSynEdit.TopLine+FSynEdit.LinesInWindow-1) then
      FSynEdit.TopLine:=row-(FSynEdit.LinesInWindow-1) div 2;

   EditorGoTo(col, row);
end;

// MoveCursorToMark
//
procedure TSynEditSourceBuffer.MoveCursorToMark;
begin
   FSynEdit.ExecuteCommand(ecGotoXY, #0, @FMark);
end;

// EditorGoTo
//
procedure TSynEditSourceBuffer.EditorGoTo(col, row : Integer);
var
   p : TBufferCoord;
begin
   p.Char:=col;
   p.Line:=row;
   FSynEdit.ExecuteCommand(ecGotoXY, #0, @p);
end;

end.
