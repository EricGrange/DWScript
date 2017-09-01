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
unit dwsBCEditorUtils;

{$I ../dws.inc}

interface

uses BCEditor.Editor, BCEditor.Types, BCEditor.Editor.KeyCommands, dwsErrors;

type

   TBCEditorSourceBuffer = class (TInterfacedObject, IAutoFixSourceBuffer)
      private
         FEditor : TBCEditor;
         FMark : TBCEditorTextPosition;

      protected
         procedure EditorGoTo(col, row : Integer);

      public
         constructor Create(anEditor : TBCEditor);

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

function BCEditorTextPosition(col, line : Integer) : TBCEditorTextPosition;

function BCEditorTextPositionEqual(const p1, p2 : TBCEditorTextPosition) : Boolean;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// BCEditorTextPosition
//
function BCEditorTextPosition(col, line : Integer) : TBCEditorTextPosition;
begin
   Result.Char:=col;
   Result.Line:=line;
end;

// BCEditorTextPositionEqual
//
function BCEditorTextPositionEqual(const p1, p2 : TBCEditorTextPosition) : Boolean;
begin
   Result:=(p1.Char=p2.Char) and (p1.Line=p2.Line);
end;

// ------------------
// ------------------ TBCEditorSourceBuffer ------------------
// ------------------

// Create
//
constructor TBCEditorSourceBuffer.Create(anEditor : TBCEditor);
begin
   inherited Create;
   FEditor:=anEditor;
end;

// GetLines
//
function TBCEditorSourceBuffer.GetLines(idx : Integer) : String;
begin
   if Cardinal(idx)<Cardinal(FEditor.Lines.Count) then
      Result:=FEditor.Lines[idx]
   else Result:=''
end;

// SetLines
//
procedure TBCEditorSourceBuffer.SetLines(idx : Integer; const newLine : String);
begin
   Assert(idx>=0);
   while FEditor.Lines.Count<=idx do
      FEditor.Lines.Add('');
   FEditor.Lines[idx]:=newLine;
end;

// GetCount
//
function TBCEditorSourceBuffer.GetCount : Integer;
begin
   Result:=FEditor.Lines.Count;
end;

// CodeAt
//
function TBCEditorSourceBuffer.CodeAt(col, line, nb : Integer) : String;
begin
   Result:=Copy(GetLines(line), col, nb);
end;

// ReplaceAt
//
procedure TBCEditorSourceBuffer.ReplaceAt(col, line, nb : Integer; const newText : String);
begin
   FEditor.SelectionBeginPosition:=BCEditorTextPosition(col, line);
   FEditor.SelectionEndPosition:=BCEditorTextPosition(col+nb, line);
   FEditor.SelectedText:=newText;
end;

// InsertLine
//
procedure TBCEditorSourceBuffer.InsertLine(line : Integer; const lineText : String);
var
   i, nTabs : Integer;
   c : Char;
begin
   EditorGoTo(1, line);
   nTabs := 0;
   for i:=1 to Length(lineText) do begin
      c:=lineText[i];
      case c of
         #9 : begin
            FEditor.ExecuteCommand(ecTab, #0, nil);
            Inc(nTabs);
         end;
         '|' : FMark:=FEditor.TextCaretPosition;
      else
         FEditor.ExecuteCommand(ecChar, c, nil);
      end;
   end;
   FEditor.ExecuteCommand(ecLineBreak, #0, nil);
   while nTabs > 0 do begin
      FEditor.ExecuteCommand(ecShiftTab, #0, nil);
      Dec(nTabs);
   end;
end;

// DeleteLine
//
procedure TBCEditorSourceBuffer.DeleteLine(line : Integer);
begin
   EditorGoTo(1, line);
   FEditor.ExecuteCommand(ecDeleteLine, #0, nil);
end;

// MoveCursorTo
//
procedure TBCEditorSourceBuffer.MoveCursorTo(col, row : Integer);
begin
   if (row<FEditor.TopLine) or (row>FEditor.TopLine+FEditor.VisibleLines-1) then
      FEditor.TopLine:=row-(FEditor.VisibleLines-1) div 2;

   EditorGoTo(col, row);
end;

// MoveCursorToMark
//
procedure TBCEditorSourceBuffer.MoveCursorToMark;
begin
   FEditor.ExecuteCommand(ecGotoXY, #0, @FMark);
end;

// EditorGoTo
//
procedure TBCEditorSourceBuffer.EditorGoTo(col, row : Integer);
begin
   FEditor.ExecuteCommand(ecLineStart, #0, nil);
   FEditor.ExecuteCommand(ecEditorBottom, #0, nil);
end;

end.
