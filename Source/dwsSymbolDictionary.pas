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
   SysUtils, Classes, TypInfo,
   dwsUtils, dwsXPlatform, dwsScriptSource, dwsSymbols, dwsJSON;

type

   { Describe how the symbol at the position is being used. suReference would be
     a typical usage of the symbol.
     suImplicit indicates that the symbol was only implicitly present
     suRTTI indicates explicit RTTI access of the symbol }
   TSymbolUsage = (suForward, suDeclaration, suImplementation, suReference,
                   suRead, suWrite, suImplicit, suRTTI );
   TSymbolUsages = set of TSymbolUsage;

   TdwsSymbolDictionary = class;

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

   TSymbolPositionBlock = array [0..127] of TSymbolPositionRec;
   PSymbolPositionBlock = ^TSymbolPositionBlock;

   // dedicated class to suballocate symbol positions
   TSymbolPositionbSubAllocator = record
      private
         FBlocks : array of PSymbolPositionBlock;
         FStack : array of TSymbolPosition;
         FStackCount : Integer;
         FStackCapacity : Integer;

         procedure AllocateNewSymbolPositions;

      public
         procedure Initialize; inline;
         procedure Finalize; inline;
         function  Finalized : Boolean; inline;

         function  AllocateSymbolPosition : TSymbolPosition;
         procedure ReleaseSymbolPosition(symPos : TSymbolPosition);
         procedure CleanupSymbolPositions;
   end;

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
         FDictionary : TdwsSymbolDictionary;
         FSymbol : TSymbol;                     // pointer to the symbol
         FPosList : array of TSymbolPosition;   // list of positions where symbol is declared and used
         FCount : Integer;
         FSourceFile : TSourceFile;             // not nil only if all positions are in that file

      protected
         function GetPosition(index : Integer) : TSymbolPosition; inline;

         // Used by TSymbolDictionary. Not meaningful to make public (symbol is known).
         function FindSymbolAtPosition(aCol, aLine : Integer; const sourceFile : String) : TSymbol; overload;

      public
         constructor Create(aDictionary : TdwsSymbolDictionary; aSymbol: TSymbol);
         destructor Destroy; override;

         procedure Add(const scriptPos : TScriptPos; const useTypes : TSymbolUsages);
         procedure Delete(index : Integer);
         procedure Clear;

         function FindUsage(const symbolUse : TSymbolUsage) : TSymbolPosition;
         function FindAnyUsage(const symbolUses : TSymbolUsages) : TSymbolPosition;
         function FindAnyUsageInFile(const symbolUses : TSymbolUsages; const sourceFile : TSourceFile) : TSymbolPosition;
         function IndexOfPosition(const scriptPos : TScriptPos) : Integer;
         procedure RemoveInRange(const startPos, endPos : TScriptPos);

         function GetEnumerator : TSymbolPositionListEnumerator;

         procedure WriteToJSON(wr : TdwsJSONWriter);

         property Items[index : Integer] : TSymbolPosition read GetPosition; default;
         function Count : Integer; inline;

         property Symbol: TSymbol read FSymbol;
   end;

   TSymbolPositionListHash = class(TSimpleHash<TSymbolPositionList>)
      protected
         function SameItem(const item1, item2 : TSymbolPositionList) : Boolean; override;
         function GetItemHashCode(const item1 : TSymbolPositionList) : Cardinal; override;

         function MatchSymbol(sym : TSymbol) : TSymbolPositionList;
   end;

   TdwsSymbolDictionaryProc = procedure (sym : TSymbol) of object;

   { List all symbols in the script. Each symbol list contains a list of the
     positions where it was used. }
   TdwsSymbolDictionary = class
      type
         TdwsSymbolDictionaryEnumerator = record
            Index : Integer;
            Hash : TSymbolPositionListHash;
            FCurrent : TSymbolPositionList;
            function MoveNext : Boolean; inline;
            function GetCurrent : TSymbolPositionList; inline;
            property Current : TSymbolPositionList read GetCurrent;
         end;

      protected
         FHash : TSymbolPositionListHash;
         FRemovingSymbols : Integer;
         FSymPosAllocator : TSymbolPositionbSubAllocator;

         function PackCallback(const item : TSymbolPositionList) : TSimpleHashAction;

      public
         constructor Create;
         destructor Destroy; override;

         procedure Clear;  // clear the lists
         procedure Pack;   // remove entries with zero script positions

         procedure AddSymbol(sym : TSymbol; const scriptPos : TScriptPos; const useTypes : TSymbolUsages);

         // remove all references to the symbol
         procedure Remove(sym : TSymbol);
         procedure RemoveInRange(const startPos, endPos : TScriptPos);
         procedure EnumerateInRange(const startPos, endPos : TScriptPos; const callBack : TdwsSymbolDictionaryProc); overload;

         procedure ReplaceSymbolAt(oldSym, newSym : TSymbol; const scriptPos : TScriptPos);
         procedure ChangeUsageAt(const scriptPos : TScriptPos; const addUsages, removeUsages : TSymbolUsages);

         function FindSymbolAtPosition(aCol, aLine: Integer; const sourceFile : String): TSymbol; overload;
         function FindSymbolAtPosition(const aScriptPos : TScriptPos) : TSymbol; overload;
         function FindSymbolPosList(sym : TSymbol) : TSymbolPositionList; overload; // return list of symbol
         function FindSymbolPosList(const symName : String) : TSymbolPositionList; overload;  // return list of symbol
         function FindSymbolPosListOfType(const symName : String; symbolType : TSymbolClass) : TSymbolPositionList; // return list of symbol given the desired type
         function FindSymbolUsage(symbol : TSymbol; symbolUse: TSymbolUsage) : TSymbolPosition; overload;
         function FindSymbolUsage(const symName : String; symbolUse: TSymbolUsage) : TSymbolPosition; overload;
         function FindSymbolUsageOfType(const symName : String; symbolType : TSymbolClass; symbolUse : TSymbolUsage) : TSymbolPosition;
         function FindSymbolByUsageAtLine(const scriptPos : TScriptPos; symbolUse : TSymbolUsage) : TSymbol;

         function GetEnumerator : TdwsSymbolDictionaryEnumerator;
         function Count : Integer; inline;

         procedure WriteToJSON(wr : TdwsJSONWriter);
         function  ToJSON : String;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TSymbolPositionbSubAllocator ------------------
// ------------------

// Initialize
//
procedure TSymbolPositionbSubAllocator.Initialize;
begin
   FStackCount := 0;
   FStackCapacity := 512;
   SetLength(FStack, FStackCapacity);
end;

// Finalize
//
procedure TSymbolPositionbSubAllocator.Finalize;
begin
   CleanupSymbolPositions;
   FStack := nil;
end;

// Finalized
//
function TSymbolPositionbSubAllocator.Finalized : Boolean;
begin
   Result := (FStack = nil);
end;

// AllocateNewSymbolPositions
//
procedure TSymbolPositionbSubAllocator.AllocateNewSymbolPositions;
var
   block : PSymbolPositionBlock;
   n : Integer;
begin
   New(block);
   n := Length(FBlocks);
   SetLength(FBlocks, n+1);
   FBlocks[n] := block;
   for n := 0 to High(block^) do
      ReleaseSymbolPosition(@block^[n]);
end;

// AllocateSymbolPosition
//
function TSymbolPositionbSubAllocator.AllocateSymbolPosition : TSymbolPosition;
begin
   if FStackCount = 0 then
      AllocateNewSymbolPositions;
   Result := FStack[FStackCount-1];
   Dec(FStackCount);
end;

// ReleaseSymbolPosition
//
procedure TSymbolPositionbSubAllocator.ReleaseSymbolPosition(symPos : TSymbolPosition);
begin
   if FStackCount = FStackCapacity then begin
      FStackCapacity := FStackCapacity + (High(TSymbolPositionBlock) shr 1);
      SetLength(FStack, FStackCapacity);
   end;
   FStack[FStackCount] := symPos;
   Inc(FStackCount);
end;

// CleanupSymbolPositions
//
procedure TSymbolPositionbSubAllocator.CleanupSymbolPositions;
var
   i : Integer;
begin
   for i := 0 to High(FBlocks) do
      Dispose(FBlocks[i]);
   SetLength(FBlocks, 0);
   FStackCount := 0;
end;

// ------------------
// ------------------ TSymbolPositionList ------------------
// ------------------

// Create
//
constructor TSymbolPositionList.Create(aDictionary : TdwsSymbolDictionary; aSymbol: TSymbol);
begin
   inherited Create;
   FDictionary := aDictionary;
   FSymbol := ASymbol;
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
   if (scriptPos.Line <= 0) or (scriptPos.SourceFile = nil) then Exit;

   if FCount = 0 then
      FSourceFile := scriptPos.SourceFile
   else if FSourceFile <> scriptPos.SourceFile then
      FSourceFile := nil;

   symPos := FDictionary.FSymPosAllocator.AllocateSymbolPosition;
   symPos.FScriptPos := scriptPos;
   symPos.FSymUsages := useTypes;

   capacity := Length(FPosList);
   if FCount = capacity then
      SetLength(FPosList, capacity + 4 + (capacity shr 2));

   FPosList[FCount] := symPos;
   Inc(FCount);
end;

// Delete
//
procedure TSymbolPositionList.Delete(index : Integer);
var
   n : Integer;
begin
   FDictionary.FSymPosAllocator.ReleaseSymbolPosition(FPosList[index]);

   n := FCount-index-1;
   if n > 0 then begin
      Move(FPosList[index+1], FPosList[index], n*SizeOf(TSymbolPosition));
      FPosList[FCount-1] := Default(TSymbolPosition);
   end;
   Dec(FCount);
end;

// Clear
//
procedure TSymbolPositionList.Clear;
var
   i : Integer;
begin
   if not FDictionary.FSymPosAllocator.Finalized then begin
      for i := 0 to FCount-1 do
         FDictionary.FSymPosAllocator.ReleaseSymbolPosition(FPosList[i]);
   end;
   SetLength(FPosList, 0);
   FCount := 0;
end;

// FindSymbolAtPosition
//
function TSymbolPositionList.FindSymbolAtPosition(aCol, aLine : Integer; const sourceFile : String): TSymbol;
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

// FindAnyUsageInFile
//
function TSymbolPositionList.FindAnyUsageInFile(const symbolUses : TSymbolUsages; const sourceFile : TSourceFile) : TSymbolPosition;
var
   i : Integer;
begin
   if Self<>nil then begin
      for i:=0 to Count-1 do begin
         Result:=FPosList[i];
         if (Result.ScriptPos.SourceFile=sourceFile) and ((symbolUses*Result.SymbolUsages)<>[]) then Exit;
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

// WriteToJSON
//
procedure TSymbolPositionList.WriteToJSON(wr : TdwsJSONWriter);
var
   i : Integer;
   u : TSymbolUsage;
begin
   wr.BeginObject;

   wr.BeginObject('symbol');
      wr.WriteString('name', Symbol.Name);
      wr.WriteString('class', String(Symbol.ClassName));
   wr.EndObject;

   wr.BeginArray('positions');
   for i := 0 to Count-1 do begin
      wr.BeginObject;
         wr.BeginArray('usages');
         for u := Low(TSymbolUsage) to High(TSymbolUsage) do begin
            if u in Items[i].SymbolUsages then
               wr.WriteString(String(GetEnumName(TypeInfo(TSymbolUsage), Ord(u))));
         end;
         wr.EndArray;
         wr.WriteString('position', Items[i].ScriptPos.AsInfo);
      wr.EndObject;
   end;
   wr.EndArray;

   wr.EndObject;
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
// ------------------ TSymbolPositionListHash ------------------
// ------------------

// SameItem
//
function TSymbolPositionListHash.SameItem(const item1, item2 : TSymbolPositionList) : Boolean;
begin
   Result := (item1.FSymbol = item2.FSymbol);
end;

// GetItemHashCode
//
function TSymbolPositionListHash.GetItemHashCode(const item1 : TSymbolPositionList) : Cardinal;
begin
   Result := SimplePointerHash(item1.FSymbol);
end;

// MatchSymbol
//
function TSymbolPositionListHash.MatchSymbol(sym : TSymbol) : TSymbolPositionList;
var
   i : Integer;
begin
   if FCount = 0 then Exit(nil);

   i := (SimplePointerHash(sym) and (FCapacity-1));

   repeat
      if FBuckets[i].HashCode = 0 then
         Exit(nil)
      else begin
         Result := FBuckets[i].Value;
         if Result.FSymbol = sym then Exit;
      end;
      i := (i+1) and (FCapacity-1);
   until False;

   Result := nil;
end;

// ------------------
// ------------------ TdwsSymbolDictionary ------------------
// ------------------

// Create
//
constructor TdwsSymbolDictionary.Create;
begin
   inherited;
   FHash := TSymbolPositionListHash.Create;
   FSymPosAllocator.Initialize;
end;

// Destroy
//
destructor TdwsSymbolDictionary.Destroy;
begin
   FSymPosAllocator.Finalize;
   Clear;
   FHash.Free;
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
   symPosList := FindSymbolPosList(sym);
   if symPosList=nil then begin
      symPosList := TSymbolPositionList.Create(Self, sym);
      FHash.Add(symPosList);
   end;

   // add the instance of the symbol to the position list
   symPosList.Add(scriptPos, useTypes);
end;

// FindSymbolAtPosition
//
function TdwsSymbolDictionary.FindSymbolAtPosition(aCol, aLine : Integer; const sourceFile : String) : TSymbol;
var
   i : Integer;
   symPosList : TSymbolPositionList;
begin
   Result := nil;
   for i := 0 to FHash.Capacity-1 do begin
      if FHash.HashBucketValue(i, symPosList) then begin
         Result := symPosList.FindSymbolAtPosition(aCol, aLine, sourceFile);
         if Assigned(Result) then Break;
      end;
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
   Result.Hash := Self.FHash;
   Result.Index := -1;
   Result.FCurrent := nil;
end;

// TdwsSymbolDictionaryEnumerator.GetCurrent
//
function TdwsSymbolDictionary.TdwsSymbolDictionaryEnumerator.GetCurrent: TSymbolPositionList;
begin
   Hash.HashBucketValue(Index, Result);
end;

// TdwsSymbolDictionaryEnumerator.MoveNext
//
function TdwsSymbolDictionary.TdwsSymbolDictionaryEnumerator.MoveNext: Boolean;
begin
   while Index < Hash.Capacity-1 do begin
      Inc(Index);
      Result := Hash.HashBucketValue(Index, FCurrent);
      if Result then begin
         Assert(FCurrent<>nil);
         Exit;
      end;
   end;
   Result := False;
end;

// Count
//
function TdwsSymbolDictionary.Count: Integer;
begin
  Result := FHash.Count;
end;

// WriteToJSON
//
function SymbolCustomSort(list: TStringList; index1, index2: Integer): Integer;
var
   spl1, spl2 : TSymbolPositionList;
begin
   Result := CompareText(list[index1], list[index2]);
   if Result = 0 then begin
      spl1 := TSymbolPositionList(list.Objects[index1]);
      spl2 := TSymbolPositionList(list.Objects[index2]);
      if spl1.Count = 0 then begin
         if spl2.Count <> 0 then
            Result := -1;
      end else begin
         if spl2.Count = 0 then
            Result := 1
         else if spl1[0].ScriptPos.IsBeforeOrEqual(spl2[0].ScriptPos) then
            if spl1[0].ScriptPos.SamePosAs(spl2[0].ScriptPos) then
               Result := CompareText(spl1.Symbol.ClassName, spl2.Symbol.ClassName)
            else Result := -1
         else Result := 1;
      end;
   end;
end;
procedure TdwsSymbolDictionary.WriteToJSON(wr : TdwsJSONWriter);
var
   i : Integer;
   list : TStringList;
   symPosList : TSymbolPositionList;
begin
   list := TStringList.Create;
   try
      for symPosList in Self do
         if symPosList.Count > 0 then
            list.AddObject(String(symPosList.Symbol.Name), symPosList);
      list.CustomSort(@SymbolCustomSort);

      wr.BeginArray;
      for i := 0 to list.Count-1 do
         TSymbolPositionList(list.Objects[i]).WriteToJSON(wr);
      wr.EndArray;
   finally
      list.Free;
   end;
end;

// ToJSON
//
function TdwsSymbolDictionary.ToJSON : String;
var
   wr : TdwsJSONWriter;
begin
   wr := TdwsJSONWriter.Create;
   try
      WriteToJSON(wr);
      Result := wr.ToString;
   finally
      wr.Free;
   end;
end;

// FindSymbolPosList
//
function TdwsSymbolDictionary.FindSymbolPosList(sym : TSymbol): TSymbolPositionList;
begin
   Result := FHash.MatchSymbol(sym);
end;

// FindSymbolPosList
//
function TdwsSymbolDictionary.FindSymbolPosList(const symName : String) : TSymbolPositionList;
var
   symPosList : TSymbolPositionList;
begin
   for symPosList in Self do begin
      if UnicodeSameText(symPosList.FSymbol.Name, symName) then
         Exit(symPosList);
   end;
   Result:=nil;
end;

type
   TSymbolRemover = class(TList)
      procedure CollectSymbolsToRemove(sym : TSymbol);
      function RemoveCallback(const item : TSymbolPositionList) : TSimpleHashAction;
   end;

// CollectSymbolsToRemove
//
procedure TSymbolRemover.CollectSymbolsToRemove(sym : TSymbol);
var
   i : Integer;
   funcSym : TFuncSymbol;
begin
   Add(sym);
   // TFuncSymbol - remove params
   funcSym:=sym.AsFuncSymbol;
   if funcSym<>nil then begin
      for i := 0 to funcSym.Params.Count - 1 do
         CollectSymbolsToRemove(funcSym.Params[i]);
   // TPropertySymbol - remove array indices
   end else if sym is TPropertySymbol then begin
      for i := 0 to TPropertySymbol(sym).ArrayIndices.Count - 1 do
         CollectSymbolsToRemove(TPropertySymbol(sym).ArrayIndices[i]);
   // TStructuredTypeSymbol - remove members (methods, fields, properties)
   end else if sym is TCompositeTypeSymbol then begin
      for i := 0 to TCompositeTypeSymbol(sym).Members.Count - 1 do
         CollectSymbolsToRemove(TCompositeTypeSymbol(sym).Members[i]);
   end;
end;

// RemoveCallback
//
function TSymbolRemover.RemoveCallback(const item : TSymbolPositionList) : TSimpleHashAction;
begin
   if IndexOf(item.FSymbol) >= 0 then begin
      item.Free;
      Result := shaRemove;
   end else Result := shaNone;
end;

// Remove
//
procedure TdwsSymbolDictionary.Remove(sym: TSymbol);
var
   list : TSymbolRemover;
begin
   if FindSymbolPosList(sym) <> nil then begin
      list := TSymbolRemover.Create;
      try
         list.CollectSymbolsToRemove(sym);
         FHash.Enumerate(list.RemoveCallback);
      finally
         list.Free;
      end;
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

   for i:=0 to FHash.Capacity-1 do begin
      if FHash.HashBucketValue(i, symPosList) then begin
         if symPosList.FSourceFile=startPos.SourceFile then
            symPosList.RemoveInRange(startPos, endPos);
      end;
   end;
end;

// EnumerateInRange
//
procedure TdwsSymbolDictionary.EnumerateInRange(const startPos, endPos : TScriptPos; const callBack : TdwsSymbolDictionaryProc);
var
   i, j : Integer;
   symPosList : TSymbolPositionList;
   symPos : TSymbolPosition;
begin
   if startPos.SourceFile<>endPos.SourceFile then Exit;

   for i:=0 to FHash.Capacity-1 do begin
      if FHash.HashBucketValue(i, symPosList) then begin
         for j:=symPosList.Count-1 downto 0 do begin
            symPos:=symPosList[j];
            if     startPos.IsBeforeOrEqual(symPos.ScriptPos)
               and symPos.ScriptPos.IsBeforeOrEqual(endPos) then begin
               callBack(symPosList.Symbol);
               Break;
            end;
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
   for i:=0 to FHash.Capacity-1 do begin
      if FHash.HashBucketValue(i, symPosList) then begin
         k:=symPosList.IndexOfPosition(scriptPos);
         if k>=0 then begin
            symPos:=symPosList[k];
            symPos.SymbolUsages:=symPos.SymbolUsages+addUsages-removeUsages;
         end;
      end;
   end;
end;

// Clear
//
procedure TdwsSymbolDictionary.Clear;
var
   i : Integer;
   symPosList : TSymbolPositionList;
begin
   for i := 0 to FHash.Capacity-1 do begin
      if FHash.HashBucketValue(i, symPosList) then
         symPosList.Free;
   end;
   FHash.Clear;
end;

// PackCallback
//
function TdwsSymbolDictionary.PackCallback(const item : TSymbolPositionList) : TSimpleHashAction;
begin
   if item.Count = 0 then begin
      item.Free;
      Result := shaRemove;
   end else Result := shaNone;
end;

// Pack
//
procedure TdwsSymbolDictionary.Pack;
begin
   FHash.Enumerate(Self.PackCallback);
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

function TdwsSymbolDictionary.FindSymbolUsage(const SymName: String;
  SymbolUse: TSymbolUsage): TSymbolPosition;
var
  list: TSymbolPositionList;
begin
  Result := nil;
  list := FindSymbolPosList(SymName);
  if Assigned(list) then
    Result := list.FindUsage(SymbolUse);
end;

function TdwsSymbolDictionary.FindSymbolUsageOfType(const SymName: String;
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
   for i := 0 to FHash.Capacity-1 do begin
      if FHash.HashBucketValue(i, list) then begin
         for j:=0 to list.Count-1 do begin
            symPos:=list[j];
            if     (symbolUse in symPos.SymbolUsages)
               and (symPos.ScriptPos.SourceFile=scriptPos.SourceFile)
               and (symPos.ScriptPos.Line=scriptPos.Line) then begin
               Exit(list.Symbol);
            end;
         end;
      end;
   end;
   Result:=nil;
end;

function TdwsSymbolDictionary.FindSymbolPosListOfType(const SymName: String;
  SymbolType: TSymbolClass): TSymbolPositionList;
var
   symPosList : TSymbolPositionList;
begin
   for symPosList in Self do begin
      if (symPosList.Symbol is symbolType) and UnicodeSameText(symPosList.Symbol.Name, symName) then
         Exit(symPosList);
   end;
   Result := nil;
end;

end.
