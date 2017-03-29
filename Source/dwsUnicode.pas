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
unit dwsUnicode;

{$I dws.inc}

interface

uses SysUtils, dwsUtils;

type

   TUnicodeStringListFlag = (usflSorted, usflCaseInsensitive);
   TUnicodeStringListFlags = set of TUnicodeStringListFlag;

   // Simple Unicode string list, serves both to alleviate the lack of
   // Unicode TStringList in FreePascal and performance issues in both
   // Delphi and FPC implementations
   TUnicodeStringList = class
      private
         FItems : array of UnicodeString;
         FCount : Integer;
         FFlags : TUnicodeStringListFlags;

      protected
         function GetString(index : Integer) : UnicodeString; inline;
         procedure SetString(index : Integer; const v : UnicodeString); inline;

         function Compare(const s1, s2 : UnicodeString) : Integer; virtual;
         function CompareIndex(index1, index2 : Integer) : Integer;

         function GetSorted : Boolean;
         procedure SetSorted(const val : Boolean);

      public
         function Add(const s : UnicodeString) : Integer;
         procedure Insert(index : Integer; const s : UnicodeString);

         procedure Delete(index : Integer);
         procedure Clear;

         function Find(const s : UnicodeString; var index : Integer) : Boolean;
         function IndexOf(const s : UnicodeString) : Integer;
         function Contains(const s : UnicodeString) : Boolean; inline;

         procedure Exchange(index1, index2 : Integer);
         procedure Sort;

         property Strings[index : Integer] : String read GetString write SetString; default;
         property Count : Integer read FCount;

         property Sorted : Boolean read GetSorted write SetSorted;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TUnicodeStringList ------------------
// ------------------

// GetString
//
function TUnicodeStringList.GetString(index : Integer) : UnicodeString;
begin
   Result := FItems[index];
end;

// SetString
//
procedure TUnicodeStringList.SetString(index : Integer; const v : UnicodeString);
begin
   FItems[index] := v;
end;

// Compare
//
function TUnicodeStringList.Compare(const s1, s2 : UnicodeString) : Integer;
begin
   if usflCaseInsensitive in FFlags then
      Result := UnicodeCompareText(s1, s2)
   else Result := CompareStr(s1, s2);
end;

// CompareIndex
//
function TUnicodeStringList.CompareIndex(index1, index2 : Integer) : Integer;
begin
   Result := Compare(FItems[index1], FItems[index2]);
end;

// GetSorted
//
function TUnicodeStringList.GetSorted : Boolean;
begin
   Result := usflSorted in FFlags;
end;

// SetSorted
//
procedure TUnicodeStringList.SetSorted(const val : Boolean);
begin
   if val then begin
      if not (usflSorted in FFlags) then begin
         Sort;
         Include(FFlags, usflSorted);
      end;
   end else Exclude(FFlags, usflSorted);
end;

// IndexOf
//
function TUnicodeStringList.IndexOf(const s : UnicodeString) : Integer;
begin
   if usflSorted in FFlags then begin
      if Find(s, Result) then Exit;
   end else begin
      for Result := 0 to Count-1 do
         if FItems[Result] = s then Exit;
   end;
   Result := -1;
end;

// Contains
//
function TUnicodeStringList.Contains(const s : UnicodeString) : Boolean;
begin
   Result := (IndexOf(s) >= 0);
end;

// Add
//
function TUnicodeStringList.Add(const s : UnicodeString) : Integer;
begin
   if usflSorted in FFlags then begin
      Find(s, Result);
      Insert(Result, s);
   end else begin
      if FCount = Length(FItems) then
         SetLength(FItems, (FCount div 4)+4);
      FItems[FCount] := s;
      Result := FCount;
      Inc(FCount);
   end;
end;

// Insert
//
procedure TUnicodeStringList.Insert(index : Integer; const s : UnicodeString);
begin
   if FCount = Length(FItems) then
      SetLength(FItems, (FCount div 4)+4);
   if index < FCount then begin
      System.Move(FItems[index], FItems[index+1], (FCount-index)*SizeOf(String));
      PPointer(FItems[index])^ := nil;
   end;
   FItems[index] := s;
end;

// Delete
//
procedure TUnicodeStringList.Delete(index : Integer);
var
   n : Integer;
begin
   FItems[index] := '';
   n := FCount-index-1;
   if n > 0 then
      System.Move(FItems[index+1], FItems[index], n*SizeOf(String));
end;

// Clear
//
procedure TUnicodeStringList.Clear;
begin
   SetLength(FItems, 0);
   FCount := 0;
end;

// Find
//
function TUnicodeStringList.Find(const s : UnicodeString; var index : Integer) : Boolean;
var
   low, high, mid, cmp : Integer;
begin
   Result := False;
   low := 0;
   high := Count-1;
   while low <= high do begin
      mid := (low + high) shr 1;
      cmp := Compare(FItems[mid], s);
      if cmp < 0 then
         low := mid + 1
      else begin
         high := mid - 1;
         if cmp = 0 then begin
            Result := True;
         end;
      end;
   end;
   index := low;
end;

// Exchange
//
procedure TUnicodeStringList.Exchange(index1, index2 : Integer);
var
   p1, p2 : PPointer;
   buf : Pointer;
begin
   p1 := @FItems[index1];
   p2 := @FItems[index2];
   buf := p1^;
   p1^ := p2^;
   p2^ := buf;
end;

// Sort
//
procedure TUnicodeStringList.Sort;
var
   qs : TQuickSort;
begin
   case FCount of
      0, 1 : ;
      2 : if Compare(FItems[0], FItems[1]) > 0 then
         Exchange(0, 1);
   else
      qs.CompareMethod := CompareIndex;
      qs.SwapMethod := Exchange;
      qs.Sort(0, FCount-1);
   end;
end;

end.
