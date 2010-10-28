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
unit dwsUtils;

interface

uses Classes, SysUtils, Variants;

type

   // TVarRecArrayContainer
   //
   TVarRecArrayContainer = class
      private
         FIntegers : array of Int64;
         FFloats : array of Extended;
         FStrings : array of String;

         function AddVarRec : PVarRec;

      public
         VarRecArray : array of TVarRec;

         constructor Create; overload;
         constructor Create(const variantArray : array of Variant); overload;

         procedure Add(const v : Variant);
         procedure AddBoolean(const b : Boolean);
         procedure AddInteger(const i : Int64);
         procedure AddFloat(const f : Double);
         procedure AddString(const s : String);
   end;

procedure UnifyAssignString(const fromStr : String; var toStr : String);
procedure TidyStringsUnifier;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   TStringListCracker = class (TStrings)
      private
         FList: PStringItemList;
   end;

   TFastCompareStringList = class (TStringList)
      function CompareStrings(const S1, S2: string): Integer; override;
   end;

var
   vCharStrings : array [0..127] of TStringList;

// CompareStrings
//
function TFastCompareStringList.CompareStrings(const S1, S2: string): Integer;
begin
   Result:=CompareStr(S1, S2);
end;

// UnifyAssignString
//
procedure UnifyAssignString(const fromStr : String; var toStr : String);
var
   i : Integer;
   sl : TStringList;
begin
   if fromStr='' then
      toStr:=''
   else begin
      i:=Ord(fromStr[1]);
      if i<=High(vCharStrings) then begin
         sl:=vCharStrings[i];
         System.MonitorEnter(sl);
         i:=sl.IndexOf(fromStr);
         if i<0 then
            i:=sl.Add(fromStr);
         toStr:=TStringListCracker(sl).FList[i].FString;
         System.MonitorExit(sl);
      end else toStr:=fromStr;
   end;
end;

// TidyStringsUnifier
//
procedure TidyStringsUnifier;
var
   i : Integer;
   sl : TStringList;
begin
   for i:=Low(vCharStrings) to High(vCharStrings) do begin
      sl:=vCharStrings[i];
      System.MonitorEnter(sl);
      sl.Clear;
      System.MonitorExit(sl);
   end;
end;

// InitializeStringsUnifier
//
procedure InitializeStringsUnifier;
var
   i : Integer;
begin
   for i:=Low(vCharStrings) to High(vCharStrings) do begin
      vCharStrings[i]:=TFastCompareStringList.Create;
      vCharStrings[i].Sorted:=True;
   end;
end;

// FinalizeStringsUnifier
//
procedure FinalizeStringsUnifier;
var
   i : Integer;
begin
   for i:=Low(vCharStrings) to High(vCharStrings) do
      FreeAndNil(vCharStrings[i]);
end;

// ------------------
// ------------------ TVarRecArrayContainer ------------------
// ------------------

// Create
//
constructor TVarRecArrayContainer.Create;
begin
end;

// Create
//
constructor TVarRecArrayContainer.Create(const variantArray : array of Variant);
var
   i : Integer;
begin
   Create;
   for i:=Low(variantArray) to High(variantArray) do
      Add(variantArray[i]);
end;

// AddVarRec
//
function TVarRecArrayContainer.AddVarRec : PVarRec;
var
   n : Integer;
begin
   n:=Length(VarRecArray);
   SetLength(VarRecArray, n+1);
   Result:=@VarRecArray[n];
end;

// Add
//
procedure TVarRecArrayContainer.Add(const v : Variant);
begin
   if VarType(v)=varBoolean then
      AddBoolean(v)
   else if VarIsOrdinal(v) then
      AddInteger(v)
   else if VarIsNumeric(v) then
      AddFloat(v)
   else if VarIsStr(v) then
      AddString(v)
   else begin
      // not really supported yet, use a nil placeholder
      with AddVarRec^ do begin
         VType:=vtPointer;
         VPointer:=nil;
      end;
   end;
end;

// AddBoolean
//
procedure TVarRecArrayContainer.AddBoolean(const b : Boolean);
begin
   with AddVarRec^ do begin
      VType:=vtBoolean;
      VBoolean:=b;
   end;
end;

// AddInteger
//
procedure TVarRecArrayContainer.AddInteger(const i : Int64);
var
   n : Integer;
begin
   n:=Length(FIntegers);
   SetLength(FIntegers, n+1);
   FIntegers[n]:=i;
   with AddVarRec^ do begin
      VType:=vtInt64;
      VInt64:=@FIntegers[n];
   end;
end;

// AddFloat
//
procedure TVarRecArrayContainer.AddFloat(const f : Double);
var
   n : Integer;
begin
   n:=Length(FFloats);
   SetLength(FFloats, n+1);
   FFloats[n]:=f;
   with AddVarRec^ do begin
      VType:=vtExtended;
      VExtended:=@FFloats[n];
   end;
end;

// AddString
//
procedure TVarRecArrayContainer.AddString(const s : String);
var
   n : Integer;
begin
   n:=Length(FStrings);
   SetLength(FStrings, n+1);
   FStrings[n]:=s;
   with AddVarRec^ do begin
      VType:=vtUnicodeString;
      VUnicodeString:=Pointer(FStrings[n]);
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   InitializeStringsUnifier;

finalization

   FinalizeStringsUnifier;

end.
