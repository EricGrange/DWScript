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
unit dwsScriptSource;

{$I dws.inc}

interface

uses
   SysUtils, dwsStrings, dwsUtils, dwsXPlatform;

type

   // TSourceFile
   //
   TSourceFile = class (TRefCountedObject)
      private
         FLineCount : Integer;
         FCode : String;

      public
         Name : String;
         property Code : String read FCode write FCode;
         function LineCount : Integer;
   end;

   // TScriptPos
   //
   PScriptPos = ^TScriptPos;
   TScriptPos = packed record
      public
         Line : Integer;
         Col : Integer;
         SourceFile : TSourceFile;

         const cLineMask = $FFFFF;

         class function Create(aSourceFile : TSourceFile; aLine, aCol : Integer) : TScriptPos; static;

         procedure Clear; inline;

         function SamePosAs(const aPos : TScriptPos) : Boolean;
         function IsMainModule : Boolean;
         function IsSourceFile(const name : String) : Boolean;
         function SourceName : String; inline;
         function SourceCode : String; inline;
         function Defined : Boolean;

         procedure IncCol; inline;
         procedure NewLine; inline;
         procedure SetColLine(aCol, aLine : Integer); inline;
         procedure SetLineCol(const aPos : TScriptPos); inline;

         function IsBeforeOrEqual(const aPos : TScriptPos) : Boolean;
         function Compare(const aPos : TScriptPos) : Integer;

         function AsInfo : String;
   end;
   TScriptPosArray = array of TScriptPos; // dynamic array that can hold ScriptPos settings (needed for ReadNameList)

   TScriptSourceType = (stMain, stUnit, stUnitNamespace, stInclude, stRecompile);

   // A specific ScriptSource entry. The text of the script contained in that unit.
   TScriptSourceItem = class (TRefCountedObject)
      private
         FNameReference : String;
         FSourceFile : TSourceFile;
         FSourceType : TScriptSourceType;

      public
         constructor Create(const ANameReference: String; ASourceFile: TSourceFile; ASourceType: TScriptSourceType);
         destructor Destroy; override;

         property NameReference : String read FNameReference write FNameReference;
         property SourceFile : TSourceFile read FSourceFile;
         property SourceType : TScriptSourceType read FSourceType;
   end;

   // Manage a list of all the different Script Texts (files) used in the program.
   TScriptSourceList = class
      private
         FSourceList : TTightList;
         FMainScript : TScriptSourceItem;

      protected
         function GetSourceItem(index : Integer) : TScriptSourceItem; inline;

      public
         constructor Create;
         destructor Destroy; override;

         procedure Clear;
         function Add(const nameReference : String; const code: String; sourceType: TScriptSourceType) : TSourceFile;

         function FindScriptSourceItem(const sourceFileName: String): TScriptSourceItem; overload;

         function IndexOf(const sourceFileName: String): Integer; overload;

         property Count : Integer read FSourceList.FCount;

         property Items[index: Integer] : TScriptSourceItem read GetSourceItem; default;
         property MainScript: TScriptSourceItem read FMainScript;
   end;

procedure ConcatScriptPosArray(var dest : TScriptPosarray; const src : TScriptPosarray; nb : Integer);

const
   cNullPos: TScriptPos = (Line: 0; Col: 0; SourceFile: nil);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ConcatScriptPosArray
//
procedure ConcatScriptPosArray(var dest : TScriptPosarray; const src : TScriptPosarray; nb : Integer);
var
   nd : Integer;
begin
   if nb=0 then Exit;
   nd:=Length(dest);
   SetLength(dest, nd+nb);
   System.Move(src[0], dest[nd], SizeOf(TScriptPos)*nb);
end;

// ------------------
// ------------------ TScriptPos ------------------
// ------------------

// Create
//
class function TScriptPos.Create(aSourceFile : TSourceFile; aLine, aCol : Integer) : TScriptPos;
begin
   Result.SourceFile:=aSourceFile;
   Result.Line:=aLine;
   Result.Col:=aCol;
end;

// Clear
//
procedure TScriptPos.Clear;
begin
   SourceFile:=nil;
   Line:=0;
   Col:=Line;
end;

// SamePosAs
//
function TScriptPos.SamePosAs(const aPos : TScriptPos) : Boolean;
begin
   Result:=    (Line=aPos.Line) and (Col=aPos.Col)
           and (SourceFile=aPos.SourceFile);
end;

// IsMainModule
//
function TScriptPos.IsMainModule : Boolean;
begin
   Result:=(SourceFile=nil) or (SourceFile.Name=MSG_MainModule);
end;

// IsSourceFile
//
function TScriptPos.IsSourceFile(const name : String) : Boolean;
begin
   Result:=(SourceFile<>nil) and UnicodeSameText(SourceFile.Name, name);
end;

// SourceName
//
function TScriptPos.SourceName : String;
begin
   if SourceFile<>nil then
      Result:=SourceFile.Name
   else Result:='';
end;

// SourceCode
//
function TScriptPos.SourceCode : String;
begin
   if SourceFile<>nil then
      Result:=SourceFile.Code
   else Result:='';
end;

// Defined
//
function TScriptPos.Defined : Boolean;
begin
   Result:=(SourceFile<>nil) and ((Line or Col)<>0);
end;

// IncCol
//
procedure TScriptPos.IncCol;
begin
   Inc(Col);
end;

// NewLine
//
procedure TScriptPos.NewLine;
begin
   Inc(Line);
   Col:=1;
end;

// SetColLine
//
procedure TScriptPos.SetColLine(aCol, aLine : Integer);
begin
   Col:=aCol;
   Line:=aLine;
end;

// SetLineCol
//
procedure TScriptPos.SetLineCol(const aPos : TScriptPos);
begin
   PUInt64(@Self)^:=PUInt64(@aPos)^;
end;

// IsBeforeOrEqual
//
function TScriptPos.IsBeforeOrEqual(const aPos : TScriptPos) : Boolean;
begin
   Result:=    (SourceFile=aPos.SourceFile)
           and (   (Line<aPos.Line)
                or ((Line=aPos.Line) and (Col<=aPos.Col)));
end;

// Compare
//
function TScriptPos.Compare(const aPos : TScriptPos) : Integer;
begin
   if SourceFile <> aPos.SourceFile then begin
      if SourceFile.Name < aPos.SourceName then
         Result := 1
      else Result := -1;
   end else if Line < aPos.Line then
      Result := -1
   else if Line > aPos.Line then
      Result := 1
   else if Col < aPos.Col then
      Result := -1
   else if Col > aPos.Col then
      Result := 1
   else Result := 0;
end;

// AsInfo
//
function TScriptPos.AsInfo : String;
begin
   if SourceFile=nil then
      Result:=''
   else begin
      if not IsMainModule then
         Result:=Format(MSG_ScriptPosFile, [SourceFile.Name])
      else Result:='';
      if Col<>cNullPos.Col then begin
         if Result<>'' then
            Result:=', '+Result;
         Result:=Format(MSG_ScriptPosColumn, [Col])+Result;
      end;
      if Line<>cNullPos.Line then begin
         if Result<>'' then
            Result:=', '+Result;
         Result:=Format(MSG_ScriptPosLine, [Line])+Result;
      end;
      if Result<>'' then
         Result:=' ['+Result+']';
   end;
end;

// ------------------
// ------------------ TSourceFile ------------------
// ------------------

// LineCount
//
function TSourceFile.LineCount : Integer;
var
   i : Integer;
begin
   if FLineCount=0 then begin
      FLineCount:=1;
      for i:=1 to Length(Code) do
         if Code[i]=#10 then
            Inc(FLineCount);
   end;
   Result:=FLineCount;
end;

// ------------------
// ------------------ TScriptSourceItem ------------------
// ------------------

constructor TScriptSourceItem.Create(const ANameReference: String; ASourceFile: TSourceFile;
  ASourceType: TScriptSourceType);
begin
   FNameReference := ANameReference;
   FSourceFile := ASourceFile;
   FSourceType := ASourceType;
end;

// Destroy
//
destructor TScriptSourceItem.Destroy;
begin
   FSourceFile.Free;
   inherited;
end;

// ------------------
// ------------------ TScriptSourceList ------------------
// ------------------

// Create
//
constructor TScriptSourceList.Create;
begin
   inherited;
   FMainScript:=nil;
end;

// Destroy
//
destructor TScriptSourceList.Destroy;
begin
   Clear;
   FSourceList.Free;
   inherited;
end;

// Add
//
function TScriptSourceList.Add(
      const nameReference : String; const code : String;
      sourceType: TScriptSourceType) : TSourceFile;
var
   srcItem : TScriptSourceItem;
begin
   srcItem:=FindScriptSourceItem(nameReference);
   if srcItem=nil then begin
      Result:=TSourceFile.Create;
      Result.Name:=nameReference;
      Result.Code:=code;
      srcItem:=TScriptSourceItem.Create(nameReference, Result, sourceType);
      FSourceList.Add(srcItem);
      // get a pointer to the 'main' script item
      if sourceType=stMain then
         FMainScript:=srcItem;
   end else begin
      Result:=srcItem.SourceFile;
   end;
end;

// Clear
//
procedure TScriptSourceList.Clear;
begin
   FSourceList.Clean;
   FMainScript:=nil;
end;

// GetSourceItem
//
function TScriptSourceList.GetSourceItem(Index: Integer): TScriptSourceItem;
begin
   Result := TScriptSourceItem(FSourceList.List[Index]);
end;

// FindScriptSourceItem
//
function TScriptSourceList.FindScriptSourceItem(const sourceFileName: String): TScriptSourceItem;
var
   x : Integer;
begin
   x:=IndexOf(SourceFileName);
   if x>=0 then
      Result:=Items[x]
   else Result:=nil;
end;

function TScriptSourceList.IndexOf(const SourceFileName: String): Integer;
var
   x: Integer;
begin
   for x := 0 to FSourceList.Count-1 do
      if UnicodeSameText(Items[x].NameReference, SourceFileName) then
         Exit(x);
   Result:=-1;
end;

end.
