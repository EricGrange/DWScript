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
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. For other initial contributors, see contributors.txt   }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
{$I dws.inc}
unit dwsXPlatform;

//
// This unit should concentrate all cross-platform aspects, croos-Delphi versions,
// ifdefs and other conditionals
//
// no ifdefs in the main code.

interface

uses Classes, SysUtils;

const
{$IFDEF LINUX}
   cLineTerminator  = #10;
{$ELSE}
   cLineTerminator  = #13#10;
{$ENDIF}

procedure SetDecimalSeparator(c : Char);
function GetDecimalSeparator : Char;

procedure CollectFiles(const directory, fileMask : String; list : TStrings);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// SetDecimalSeparator
//
procedure SetDecimalSeparator(c : Char);
begin
   {$IF CompilerVersion >= 22.0}
   FormatSettings.DecimalSeparator:=c;
   {$ELSE}
   DecimalSeparator:=c;
   {$IFEND}
end;

// GetDecimalSeparator
//
function GetDecimalSeparator : Char;
begin
   {$IF CompilerVersion >= 22.0}
   Result:=FormatSettings.DecimalSeparator;
   {$ELSE}
   Result:=DecimalSeparator;
   {$IFEND}
end;

// CollectFiles
//
procedure CollectFiles(const directory, fileMask : String; list : TStrings);
var
   searchRec : TSearchRec;
   found : Integer;
begin
   found:=FindFirst(directory+fileMask, faArchive or faReadOnly or faHidden, searchRec);
   while found=0 do begin
      if (searchRec.Attr and faDirectory)=0 then begin
         list.Add(directory+searchRec.Name);
      end;
      found:=FindNext(searchRec);
   end;
   FindClose(searchRec);
end;

end.
