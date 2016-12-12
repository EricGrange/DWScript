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
unit dwsSymbolDictionary;

{$I dws.inc}

interface

uses
   dwsUtils, dwsXPlatform, dwsScriptSource, dwsSymbols;

type

   { Describe how the symbol at the position is being used. suReference would be
     a typical usage of the symbol.
     suImplicit indicates that the symbol was only implicitly present
     suRTTI indicates explicit RTTI access of the symbol }
   TSymbolUsage = (suForward, suDeclaration, suImplementation, suReference,
                   suRead, suWrite, suImplicit, suRTTI );
   TSymbolUsages = set of TSymbolUsage;

   // Records a symbol's position in source and usage at that position
   //
   TSymbolPositionRec = record
      private
         FScriptPos : TScriptPos;     // location of symbol instance in script
         FSymUsages : TSymbolUsages;  // how symbol is used at this location (mutiple uses possible, Functions are Delcared/Implemented at same spot)

      public
         property ScriptPos : TScriptPos read FScriptPos;
         property SymbolUsages : TSymbolUsages read FSymUsages write FSymUsages;
   end;
   TSymbolPosition = ^TSymbolPositionRec;

   {Re-list every symbol (pointer to it) and every position it is in in the script }
   TSymbolPositionList = class (TRefCountedObject)
      type
         TSymbolPositionListEnumerator = record
            Index : Integer;
            PosList : TSymbolPositionList;
            function MoveNext : Boolean; inline;
            function GetCurrent : TSymbolPosition; inline;
            property Current : TSymbolPosition read GetCurrent;
         end;

      private
         FSymbol : TSymbol;                     // pointer to the symbol
         FPosList : array of TSymbolPosition;   // list of positions where symbol is declared and used
         FCount : Integer;
         FSourceFile : TSourceFile;             // not nil only if all positions are in that file

      protected
         function GetPosition(index : Integer) : TSymbolPosition; inline;

         // Used by TSymbolDictionary. Not meaningful to make public (symbol is known).
         function FindSymbolAtPosition(aCol, aLine : Integer; const sourceFile : UnicodeString) : TSymbol; overload;

      public
         constructor Create(ASymbol: TSymbol);
         destructor Destroy; override;

         procedure Add(const scriptPos : TScriptPos; const useTypes : TSymbolUsages);
         procedure Delete(index : Integer);
         procedure Clear;

         function FindUsage(const symbolUse : TSymbolUsage) : TSymbolPosition;
         function FindAnyUsage(const symbolUses : TSymbolUsages) : TSymbolPosition;
         function IndexOfPosition(const scriptPos : TScriptPos) : Integer;
         procedure RemoveInRange(const startPos, endPos : TScriptPos);

         function GetEnumerator : TSymbolPositionListEnumerator;

         property Items[index : Integer] : TSymbolPosition read GetPosition; default;
         function Count : Integer; inline;

         property Symbol: TSymbol read FSymbol;
   end;

   TSymbolPositionListList = class(TSortedList<TSymbolPositionList>)
      protected
         function Compare(const item1, item2 : TSymbolPositionList) : Integer; override;
   end;

   TdwsSymbolDictionaryProc = procedure (sym : TSymbol) of object;
   TdwsSymbolDictionaryRef = reference to procedure (sym : TSymbol);

   { List all symbols in the script. Each symbol list contains a list of the
     positions where it was used. }
   TdwsSymbolDictionary = class
      type
         TdwsSymbolDictionaryEnumerator = record
            Index : Integer;
            Dict : TdwsSymbolDictionary;
            function MoveNext : Boolean; inline;
            function GetCurrent : TSymbolPositionList; inline;
            property Current : TSymbolPositionList read GetCurrent;
         end;

      protected
         FSymbolList : TSymbolPositionListList;
         FSearchSymbolPositionList : TSymbolPositionList;

         function GetList(Index: Integer): TSymbolPositionList; inline;

      public
         constructor Create;
         destructor Destroy; override;

         procedure Clear;  // clear the lists
         procedure AddSymbol(sym : TSymbol; const scriptPos : TScriptPos; const useTypes : TSymbolUsages);

         // remove all references to the symbol
         procedure Remove(sym : TSymbol);
         procedure RemoveInRange(const startPos, endPos : TScriptPos);
         procedure EnumerateInRange(const startPos, endPos : TScriptPos; const callBack : TdwsSymbolDictionaryProc); overload;
         procedure EnumerateInRange(const startPos, endPos : TScriptPos; const callBack : TdwsSymbolDictionaryRef); overload;

         procedure ReplaceSymbolAt(oldSym, newSym : TSymbol; const scriptPos : TScriptPos);
         procedure ChangeUsageAt(const scriptPos : TScriptPos; const addUsages, removeUsages : TSymbolUsages);

         function FindSymbolAtPosition(aCol, aLine: Integer; const sourceFile : UnicodeString): TSymbol; overload;
         function FindSymbolAtPosition(const aScriptPos : TScriptPos) : TSymbol; overload;
         function FindSymbolPosList(sym : TSymbol) : TSymbolPositionList; overload;  // return list of symbol
         function FindSymbolPosList(const symName : UnicodeString) : TSymbolPositionList; overload;  // return list of symbol
         function FindSymbolPosListOfType(const symName : UnicodeString; symbolType : TSymbolClass) : TSymbolPositionList; // return list of symbol given the desired type
         function FindSymbolUsage(symbol : TSymbol; symbolUse: TSymbolUsage) : TSymbolPosition; overload;
         function FindSymbolUsage(const symName : UnicodeString; symbolUse: TSymbolUsage) : TSymbolPosition; overload;
         function FindSymbolUsageOfType(const symName : UnicodeString; symbolType : TSymbolClass; symbolUse : TSymbolUsage) : TSymbolPosition;
         function FindSymbolByUsageAtLine(const scriptPos : TScriptPos; symbolUse : TSymbolUsage) : TSymbol;

         function GetEnumerator : TdwsSymbolDictionaryEnumerator;

         function Count : Integer; inline;
         property Items[index: Integer] : TSymbolPositionList read GetList; default;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TSymbolPositionList ------------------
// ------------------

// Create
//
constructor TSymbolPositionList.Create(ASymbol: TSymbol);
begin
   FSymbol:=ASymbol;
end;

// Destroy
//
destructor TSymbolPositionList.Destroy;
begin
   Clear;
   inherited;
end;

// Add
//
procedure TSymbolPositionList.Add(const scriptPos : TScriptPos; const useTypes : TSymbolUsages);
var
   symPos : TSymbolPosition;
   capacity : Integer;
begin
   if (scriptPos.Line<=0) or (scriptPos.SourceFile=nil) then Exit;

   if FCount=0 then
      FSourceFile:=scriptPos.SourceFile
   else if FSourceFile<>scriptPos.SourceFile then
      FSourceFile:=nil;

   New(symPos);
   symPos.FScriptPos:=scriptPos;
   symPos.FSymUsages:=useTypes;

   capacity := Length(FPosList);
   if FCount = capacity then
      SetLength(FPosList, capacity + 8 + (capacity shr 2));

   FPosList[FCount] := symPos;
   Inc(FCount);
end;

// Delete
//
procedure TSymbolPositionList.Delete(index : Integer);
var
   n : Integer;
begin
   Dispose(FPosList[index]);

   n := FCount-index-1;
   if n > 0 then begin
      Move(FPosList[index+1], FPosList[index], n*SizeOf(TSymbolPosition));
      FillChar(FPosList[FCount-1], SizeOf(TSymbolPosition), 0);
   end;
   Dec(FCount);
end;

// Clear
//
procedure TSymbolPositionList.Clear;
var
   i : Integer;
begin
   for i:=0 to FCount-1 do
      Dispose(FPosList[i]);
   SetLength(FPosList, 0);
end;

// FindSymbolAtPosition
//
function TSymbolPositionList.FindSymbolAtPosition(aCol, aLine : Integer; const sourceFile : UnicodeString): TSymbol;
var
   i : Integer;
   symPos : TSymbolPosition;
begin
   for i:=0 to FCount-1 do begin
      symPos:=FPosList[i];
      if     (symPos.ScriptPos.Line=ALine)
         and (symPos.ScriptPos.Col=ACol)
         and UnicodeSameText(symPos.ScriptPos.SourceFile.Name, sourceFile) then begin
         Exit(FSymbol);
      end;
   end;
   Result:=nil;
end;

// GetPosition
//
function TSymbolPositionList.GetPosition(Index: Integer): TSymbolPosition;
begin
   Result:=FPosList[Index];
end;

// Count
//
function TSymbolPositionList.Count: Integer;
begin
   Result:=FCount;
end;

// FindUsage
//
function TSymbolPositionList.FindUsage(const symbolUse : TSymbolUsage) : TSymbolPosition;
var
   i : Integer;
begin
   if Self<>nil then begin
      for i:=0 to Count-1 do begin
         Result:=FPosList[i];
         if symbolUse in Result.SymbolUsages then Exit;
      end;
   end;
   Result:=nil;
end;

// FindAnyUsage
//
function TSymbolPositionList.FindAnyUsage(const symbolUses : TSymbolUsages) : TSymbolPosition;
var
   i : Integer;
begin
   if Self<>nil then begin
      for i:=0 to Count-1 do begin
         Result:=FPosList[i];
         if (symbolUses*Result.SymbolUsages)<>[] then Exit;
      end;
   end;
   Result:=nil;
end;

// IndexOfPosition
//
function TSymbolPositionList.IndexOfPosition(const scriptPos : TScriptPos) : Integer;
var
   i : Integer;
begin
   for i:=0 to Count-1 do begin
      if FPosList[i].ScriptPos.SamePosAs(scriptPos) then
         Exit(i);
   end;
   Result:=-1;
end;

// RemoveInRange
//
procedure TSymbolPositionList.RemoveInRange(const startPos, endPos : TScriptPos);
var
   i : Integer;
   symPos : TSymbolPosition;
begin
   for i := FCount-1 downto 0 do begin
      symPos := FPosList[i];
      if     startPos.IsBeforeOrEqual(symPos.ScriptPos)
         and symPos.ScriptPos.IsBeforeOrEqual(endPos) then begin
         Delete(i);
      end;
   end;
end;

// GetEnumerator
//
function TSymbolPositionList.GetEnumerator: TSymbolPositionListEnumerator;
begin
   if Self=nil then begin
      Result.PosList:=nil;
      Result.Index:=0;
   end else begin
      Result.PosList:=Self;
      Result.Index:=Count;
   end;
end;

// TSymbolPositionListEnumerator.GetCurrent
//
function TSymbolPositionList.TSymbolPositionListEnumerator.GetCurrent: TSymbolPosition;
begin
   Result:=PosList[Index];
end;

// TSymbolPositionListEnumerator.MoveNext
//
function TSymbolPositionList.TSymbolPositionListEnumerator.MoveNext: Boolean;
begin
   Dec(Index);
   Result:=(Index>=0);
end;

// ------------------
// ------------------ TSymbolPositionListList ------------------
// ------------------

// Compare
//
function TSymbolPositionListList.Compare(const item1, item2 : TSymbolPositionList) : Integer;
var
   p1, p2 : NativeInt;
begin
   p1 := NativeInt(item1.Symbol);
   p2 := NativeInt(item2.Symbol);
   Result := Integer(p1 > p2) - Integer(p1 < p2);

//   if NativeInt(item1.Symbol)<NativeInt(item2.Symbol) then
//      Result:=-1
//   else if NativeInt(item1.Symbol)>NativeInt(item2.Symbol) then
//      Result:=1
//   else Result:=0;
end;

// ------------------
// ------------------ TdwsSymbolDictionary ------------------
// ------------------

// Create
//
constructor TdwsSymbolDictionary.Create;
begin
   FSymbolList:=TSymbolPositionListList.Create;
   FSearchSymbolPositionList:=TSymbolPositionList.Create(nil);
end;

// Destroy
//
destructor TdwsSymbolDictionary.Destroy;
begin
   Clear;
   FSymbolList.Free;
   FSearchSymbolPositionList.Free;
   inherited;
end;

// AddSymbol
//
procedure TdwsSymbolDictionary.AddSymbol(sym : TSymbol; const scriptPos : TScriptPos; const useTypes : TSymbolUsages);
var
   symPosList : TSymbolPositionList;
begin
   if sym=nil then Exit;         // don't add a nil pointer
   if sym.IsBaseType then Exit;  // don't store references to base symbols

   // Check to see if symbol list already exists, if not create it
   symPosList:=FindSymbolPosList(sym);
   if symPosList=nil then begin
      symPosList:=TSymbolPositionList.Create(sym);
      FSymbolList.Add(symPosList);
   end;

   // add the instance of the symbol to the position list
   symPosList.Add(scriptPos, UseTypes);
end;

// FindSymbolAtPosition
//
function TdwsSymbolDictionary.FindSymbolAtPosition(aCol, aLine : Integer; const sourceFile : UnicodeString) : TSymbol;
var
   i : Integer;
begin
   Result:=nil;
   for i:=0 to FSymbolList.Count-1 do begin
      Result:=FSymbolList[i].FindSymbolAtPosition(aCol, aLine, sourceFile);
      if Assigned(Result) then Break;
   end;
end;

// FindSymbolAtPosition
//
function TdwsSymbolDictionary.FindSymbolAtPosition(const aScriptPos: TScriptPos): TSymbol;
begin
   Result:=FindSymbolAtPosition(aScriptPos.Col, aScriptPos.Line, aScriptPos.SourceName);
end;

// GetEnumerator
//
function TdwsSymbolDictionary.GetEnumerator: TdwsSymbolDictionaryEnumerator;
begin
   if Self=nil then begin
      Result.Dict:=nil;
      Result.Index:=0;
   end else begin
      Result.Dict:=Self;
      Result.Index:=Count;
   end;
end;

// GetList
//
function TdwsSymbolDictionary.GetList(Index: Integer): TSymbolPositionList;
begin
   Result:=FSymbolList[Index];
end;

// Count
//
function TdwsSymbolDictionary.Count: Integer;
begin
  Result:=FSymbolList.Count;
end;

// FindSymbolPosList
//
function TdwsSymbolDictionary.FindSymbolPosList(Sym: TSymbol): TSymbolPositionList;
var
   i : Integer;
begin
   FSearchSymbolPositionList.FSymbol:=sym;
   if FSymbolList.Find(FSearchSymbolPositionList, i) then
      Result:=FSymbolList[i]
   else Result:=nil;
end;

// FindSymbolPosList
//
function TdwsSymbolDictionary.FindSymbolPosList(const symName : UnicodeString) : TSymbolPositionList;
var
   i : Integer;
begin
   for i:=0 to FSymbolList.Count-1 do begin
      // same name (not case-sensitive)
      Result:=FSymbolList[i];
      if AnsiCompareText(Result.Symbol.Name, SymName)=0 then Exit;
   end;
   Result:=nil;
end;

// Remove
//
procedure TdwsSymbolDictionary.Remove(sym: TSymbol);
var
   idx, i : Integer;
   symList : TSymbolPositionList;
   funcSym : TFuncSymbol;
begin
   // TFuncSymbol - remove params
   funcSym:=sym.AsFuncSymbol;
   if funcSym<>nil then begin
      for i := 0 to funcSym.Params.Count - 1 do
         Remove(funcSym.Params[i]);
   // TPropertySymbol - remove array indices
   end else if sym is TPropertySymbol then begin
      for i := 0 to TPropertySymbol(sym).ArrayIndices.Count - 1 do
         Remove(TPropertySymbol(sym).ArrayIndices[i]);
   // TStructuredTypeSymbol - remove members (methods, fields, properties)
   end else if sym is TCompositeTypeSymbol then begin
      for i := 0 to TCompositeTypeSymbol(sym).Members.Count - 1 do
         Remove(TCompositeTypeSymbol(sym).Members[i]);
   end;

   // basic entry to remove
   symList := FindSymbolPosList(sym);
   if Assigned(symList) then begin
      // remove symList from internal list
      idx:=FSymbolList.Extract(symList);
      Assert(idx>=0);
      SuppressH2077ValueAssignedToVariableNeverUsed(idx);
      symList.Free;
   end;
end;

// RemoveInRange
//
procedure TdwsSymbolDictionary.RemoveInRange(const startPos, endPos : TScriptPos);
var
   i : Integer;
   symPosList : TSymbolPositionList;
begin
   if startPos.SourceFile<>endPos.SourceFile then Exit;

   for i:=0 to FSymbolList.Count-1 do begin
      symPosList:=FSymbolList[i];
      if symPosList.FSourceFile=startPos.SourceFile then
         FSymbolList[i].RemoveInRange(startPos, endPos);
   end;
end;

// EnumerateInRange
//
procedure TdwsSymbolDictionary.EnumerateInRange(const startPos, endPos : TScriptPos; const callBack : TdwsSymbolDictionaryProc);
var
   i, j : Integer;
   list : TSymbolPositionList;
   symPos : TSymbolPosition;
begin
   if startPos.SourceFile<>endPos.SourceFile then Exit;

   for i:=0 to FSymbolList.Count-1 do begin
      list:=FSymbolList[i];
      for j:=list.Count-1 downto 0 do begin
         symPos:=list[j];
         if     startPos.IsBeforeOrEqual(symPos.ScriptPos)
            and symPos.ScriptPos.IsBeforeOrEqual(endPos) then begin
            callBack(list.Symbol);
            Break;
         end;
      end;
   end;
end;

// EnumerateInRange
//
procedure TdwsSymbolDictionary.EnumerateInRange(const startPos, endPos : TScriptPos; const callBack : TdwsSymbolDictionaryRef);
var
   i, j : Integer;
   list : TSymbolPositionList;
   symPos : TSymbolPosition;
begin
   if startPos.SourceFile<>endPos.SourceFile then Exit;

   for i:=0 to FSymbolList.Count-1 do begin
      list:=FSymbolList[i];
      for j:=list.Count-1 downto 0 do begin
         symPos:=list[j];
         if     startPos.IsBeforeOrEqual(symPos.ScriptPos)
            and symPos.ScriptPos.IsBeforeOrEqual(endPos) then begin
            callBack(list.Symbol);
            Break;
         end;
      end;
   end;
end;

// ReplaceSymbolAt
//
procedure TdwsSymbolDictionary.ReplaceSymbolAt(oldSym, newSym : TSymbol; const scriptPos : TScriptPos);
var
   i : Integer;
   list : TSymbolPositionList;
   symPos : TSymbolPosition;
begin
   list:=FindSymbolPosList(oldSym);
   i:=list.IndexOfPosition(scriptPos);
   Assert(i>=0);
   symPos:=list[i];
   AddSymbol(newSym, scriptPos, symPos.SymbolUsages);
   list.Delete(i);
end;

// ChangeUsageAt
//
procedure TdwsSymbolDictionary.ChangeUsageAt(const scriptPos : TScriptPos; const addUsages, removeUsages : TSymbolUsages);
var
   i, k : Integer;
   symPosList : TSymbolPositionList;
   symPos : TSymbolPosition;
begin
   for i:=0 to FSymbolList.Count-1 do begin
      symPosList:=FSymbolList[i];
      k:=symPosList.IndexOfPosition(scriptPos);
      if k>=0 then begin
         symPos:=symPosList[k];
         symPos.SymbolUsages:=symPos.SymbolUsages+addUsages-removeUsages;
      end;
   end;
end;

// Clear
//
procedure TdwsSymbolDictionary.Clear;
begin
   FSymbolList.Clean;
end;

// FindSymbolUsage
//
function TdwsSymbolDictionary.FindSymbolUsage(symbol : TSymbol; symbolUse : TSymbolUsage) : TSymbolPosition;
var
   list : TSymbolPositionList;
begin
   list:=FindSymbolPosList(Symbol);
   if Assigned(list) then
      Result:=list.FindUsage(SymbolUse)
   else Result:=nil;
end;

function TdwsSymbolDictionary.FindSymbolUsage(const SymName: UnicodeString;
  SymbolUse: TSymbolUsage): TSymbolPosition;
var
  list: TSymbolPositionList;
begin
  Result := nil;
  list := FindSymbolPosList(SymName);
  if Assigned(list) then
    Result := list.FindUsage(SymbolUse);
end;

function TdwsSymbolDictionary.FindSymbolUsageOfType(const SymName: UnicodeString;
  SymbolType: TSymbolClass; SymbolUse: TSymbolUsage): TSymbolPosition;
var
  list: TSymbolPositionList;
begin
  Result := nil;
  list := FindSymbolPosListOfType(SymName, SymbolType);
  if Assigned(list) then
    Result := list.FindUsage(SymbolUse);
end;

// FindSymbolByUsageAtLine
//
function TdwsSymbolDictionary.FindSymbolByUsageAtLine(const scriptPos : TScriptPos; symbolUse: TSymbolUsage) : TSymbol;
var
   i, j : Integer;
   list : TSymbolPositionList;
   symPos : TSymbolPosition;
begin
   for i:=0 to FSymbolList.Count-1 do begin
      list:=FSymbolList[i];
      for j:=0 to list.Count-1 do begin
         symPos:=list[j];
         if     (symbolUse in symPos.SymbolUsages)
            and (symPos.ScriptPos.SourceFile=scriptPos.SourceFile)
            and (symPos.ScriptPos.Line=scriptPos.Line) then begin
            Exit(list.Symbol);
         end;
      end;
   end;
   Result:=nil;
end;

function TdwsSymbolDictionary.FindSymbolPosListOfType(const SymName: UnicodeString;
  SymbolType: TSymbolClass): TSymbolPositionList;
var
  x: Integer;
begin
  Result := nil;
  for x := 0 to Self.Count - 1 do
    if UnicodeSameText(Self.Items[x].Symbol.Name, SymName) and (Self.Items[x].Symbol is SymbolType) then // same name (not case-sensitive)
    begin
      Result := Self.Items[x];
      Break;
    end;
end;

function TdwsSymbolDictionary.TdwsSymbolDictionaryEnumerator.GetCurrent: TSymbolPositionList;
begin
   Result:=Dict[Index];
end;

function TdwsSymbolDictionary.TdwsSymbolDictionaryEnumerator.MoveNext: Boolean;
begin
   Dec(Index);
   Result:=(Index>=0);
end;

end.
