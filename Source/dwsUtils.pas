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
{$I dws.inc}
unit dwsUtils;

interface

uses Classes, SysUtils, Variants, SyncObjs, dwsXPlatform;

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

         procedure Initialize;
   end;

   // TTightList
   //
   {: Compact list embedded in a record.<p>
      If the list holds only 1 item, no dynamic memory is allocated
      (the list pointer is used).
      Make sure to Clear or Clean in the destructor of the Owner. }
   TTightList = record
      private
         FList : PPointerList;

         procedure RaiseIndexOutOfBounds;
         function GetList : PPointerList; inline;

      public
         FCount : Integer;     // exposed so it can be used for direct property access

         property List : PPointerList read GetList;
         property Count : Integer read FCount;

         procedure Free; // to posture as a regular TList
         procedure Clean;  // clear the list and free the item objects
         procedure Clear;  // clear the list without freeing the items
         function Add(item : Pointer) : Integer;
         procedure Assign(const aList : TTightList);
         function IndexOf(item : Pointer) : Integer;
         function Remove(item : Pointer) : Integer;
         procedure Delete(index : Integer);
         procedure Insert(index : Integer; item : Pointer);
         procedure Move(curIndex, newIndex : Integer);
         procedure Exchange(index1, index2 : Integer);
   end;

   // TTightStack
   //
   {: Embeddable stack functionality }
   TTightStack = record
      private
         FList : PPointerList;
         FCount : Integer;
         FCapacity : Integer;

         procedure Grow;

      public
         procedure Push(item : Pointer); inline;
         function  Peek : Pointer; inline;
         procedure Pop; inline;

         procedure Clear; inline;
         procedure Clean;
         procedure Free;

         property List : PPointerList read FList;
         property Count : Integer read FCount;
   end;


   // TSimpleList<T>
   //
   {: A minimalistic generic list. }
   TSimpleList<T> = class
      private
         FItems : array of T;
         FCount : Integer;
         FCapacity : Integer;
      protected
         procedure Grow;
         function GetItems(const idx : Integer) : T; inline;
         procedure SetItems(const idx : Integer; const value : T); inline;
      public
         procedure Add(const item : T);
         procedure Extract(idx : Integer);
         procedure Clear;
         property Items[const position : Integer] : T read GetItems write SetItems; default;
         property Count : Integer read FCount;
   end;

   // TObjectList
   //
   {: A simple generic object list, owns objects }
   TObjectList<T: class> = class
      private
         FItems : array of T;
         FCount : Integer;
      protected
         function GetItem(index : Integer) : T;
         procedure SetItem(index : Integer; const item : T);
      public
         destructor Destroy; override;
         function Add(const anItem : T) : Integer;
         function IndexOf(const anItem : T) : Integer;
         function Extract(idx : Integer) : T;
         procedure ExtractAll;
         procedure Clear;
         property Items[index : Integer] : T read GetItem write SetItem; default;
         property Count : Integer read FCount;
   end;

   // TSortedList
   //
   {: List that maintains its elements sorted, subclasses must override Compare }
   TSortedList<T: class> = class
      private
         FItems : array of T;
         FCount : Integer;
      protected
         function GetItem(index : Integer) : T;
         function Find(const item : T; var index : Integer) : Boolean;
         function Compare(const item1, item2 : T) : Integer; virtual; abstract;
         procedure InsertItem(index : Integer; const anItem : T);
      public
         function Add(const anItem : T) : Integer;
         function AddOrFind(const anItem : T; var added : Boolean) : Integer;
         function Extract(const anItem : T) : Integer;
         function IndexOf(const anItem : T) : Integer;
         procedure Clear;
         procedure Clean;
         property Items[index : Integer] : T read GetItem; default;
         property Count : Integer read FCount;
   end;

   // TSimpleStack<T>
   //
   {: A minimalistic generic stack.
      Note that internal array items are NOT cleared on Pop, for refcounted types,
      you need to clear yourself manually via Peek. }
   TSimpleStack<T> = class
      private
         FItems : array of T;
         FCount : Integer;
         FCapacity : Integer;
      protected
         procedure Grow;
         function GetPeek : T; inline;
         procedure SetPeek(const item : T);
         function GetItems(const position : Integer) : T;
         procedure SetItems(const position : Integer; const value : T);
      public
         procedure Push(const item : T);
         procedure Pop; inline;
         procedure Clear;
         property Peek : T read GetPeek write SetPeek;
         property Items[const position : Integer] : T read GetItems write SetItems;
         property Count : Integer read FCount;
   end;

   TSimpleHashBucket<T> = record
      HashCode : Integer;
      Value : T;
   end;
   TSimpleHashBucketArray<T> = array of TSimpleHashBucket<T>;
   TSimpleHashProc<T> = reference to procedure (const item : T);

   {: Minimalistic open-addressing hash, subclasses must override SameItem and GetItemHashCode.
      HashCodes *MUST* be non zero }
   TSimpleHash<T> = class
      private
         FBuckets : TSimpleHashBucketArray<T>;
         FCount : Integer;
         FGrowth : Integer;
         FCapacity : Integer;

      protected
         procedure Grow;
         function HashBucket(const hashCode : Integer) : Integer; inline;
         function LinearFind(const item : T; var index : Integer) : Boolean;
         function SameItem(const item1, item2 : T) : Boolean; virtual; abstract;
         // hashCode must be non-null
         function GetItemHashCode(const item1 : T) : Integer; virtual; abstract;

      public
         function Add(const anItem : T) : Boolean; // true if added
         function Extract(const anItem : T) : Boolean; // true if extracted
         function Contains(const anItem : T) : Boolean;
         function Match(var anItem : T) : Boolean;
         procedure Enumerate(const callBack : TSimpleHashProc<T>);
         procedure Clear;

         property Count : Integer read FCount;
   end;

   TSimpleObjectHash<T: Class> = class(TSimpleHash<T>)
      protected
         function SameItem(const item1, item2 : T) : Boolean; override;
         function GetItemHashCode(const item1 : T) : Integer; override;

      public
         procedure Clean;
   end;

const
   cWriteOnlyBlockStreamBlockSize = $2000 - 2*SizeOf(Pointer);

type

   // TWriteOnlyBlockStream
   //
   {: Provides a write-only block-based stream. }
   TWriteOnlyBlockStream = class (TStream)
      private
         FFirstBlock : PPointerArray;
         FCurrentBlock : PPointerArray;
         FBlockRemaining : PInteger;
         FTotalSize : Integer;

      protected
          function GetSize: Int64; override;

          procedure AllocateCurrentBlock;
          procedure FreeBlocks;

      public
         constructor Create;
         destructor Destroy; override;

         function Seek(Offset: Longint; Origin: Word): Longint; override;
         function Read(var Buffer; Count: Longint): Longint; override;
         function Write(const buffer; count: Longint): Longint; override;
         // must be strictly an utf16 string
         procedure WriteString(const utf16String : String);
         procedure WriteSubString(const utf16String : String; startPos : Integer); overload;
         procedure WriteSubString(const utf16String : String; startPos, length : Integer); overload;
         procedure WriteChar(utf16Char : Char);
         // assumes data is an utf16 string
         function ToString : String; override;

         procedure Clear;

         procedure StoreData(var buffer); overload;
         procedure StoreData(destStream : TStream); overload;
   end;

{: Changes the class of an object (by altering the VMT pointer).<p>
   Only checks IntanceSize.
   Use only if you understand fully what the above means. }
procedure ChangeObjectClass(ref : TObject; newClass : TClass);

procedure UnifyAssignString(const fromStr : String; var toStr : String);
procedure TidyStringsUnifier;

function UnicodeCompareText(const s1, s2 : String) : Integer;
function UnicodeSameText(const s1, s2 : String) : Boolean;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// MorphObjectClass
//
procedure ChangeObjectClass(ref : TObject; newClass : TClass);
begin
   Assert(ref.InstanceSize=newClass.InstanceSize);
   PPointer(ref)^:=Pointer(newClass);
end;

// ------------------
// ------------------ String Unifier ------------------
// ------------------

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
   vUnifierLock : TCriticalSection;

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
         vUnifierLock.Enter;
         i:=sl.IndexOf(fromStr);
         if i<0 then
            i:=sl.Add(fromStr);
         toStr:=TStringListCracker(sl).FList[i].FString;
         vUnifierLock.Leave;
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
      vUnifierLock.Enter;
      sl.Clear;
      vUnifierLock.Leave;
   end;
end;

// UnicodeCompareLen
//
function UnicodeCompareLen(p1, p2 : PChar; n : Integer) : Integer;
var
   i : Integer;
   remaining : Integer;
   c1, c2 : Integer;
begin
   for i:=1 to n do begin
      c1:=Ord(p1^);
      c2:=Ord(p2^);
      if (c1<>c2) then begin
         if (c1<=127) and (c2<=127) then begin
            if c1 in [Ord('a')..Ord('z')] then
               c1:=c1+Ord('A')-Ord('a');
            if c2 in [Ord('a')..Ord('z')] then
               c2:=c2+Ord('A')-Ord('a');
            if c1<>c2 then begin
               Result:=c1-c2;
               Exit;
            end;
         end else begin
            remaining:=n-i+1;
            Result:=UnicodeComparePChars(p1, remaining, p2, remaining);
            Exit;
         end;
      end;
      Inc(p1);
      Inc(p2);
   end;
   Result:=0;
end;

// UnicodeCompareText
//
function UnicodeCompareText(const s1, s2 : String) : Integer;
var
   n1, n2, dn : Integer;
begin
   if S1<>'' then begin
      if S2<>'' then begin
         n1:=Length(s1);
         n2:=Length(s2);
         dn:=n1-n2;
         if dn<0 then begin
            Result:=UnicodeCompareLen(PChar(NativeInt(s1)), PChar(NativeInt(s2)), n1);
            if Result=0 then
               Result:=-1;
         end else begin
            Result:=UnicodeCompareLen(PChar(NativeInt(S1)), PChar(NativeInt(s2)), n2);
            if (Result=0) and (dn>0) then
               Result:=1;
         end;
      end else Result:=1;
   end else if S2<>'' then
      Result:=-1
   else Result:=0;
end;

// UnicodeSameText
//
function UnicodeSameText(const s1, s2 : String) : Boolean;
begin
   Result:=(UnicodeCompareText(s1, s2)=0);
end;

// InitializeStringsUnifier
//
procedure InitializeStringsUnifier;
var
   i : Integer;
begin
   vUnifierLock:=TCriticalSection.Create;
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
   FreeAndNil(vUnifierLock);
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
   Initialize;
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
      VInteger:=n;
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
      VInteger:=n;
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
      VInteger:=n;
   end;
end;

// Initialize
//
procedure TVarRecArrayContainer.Initialize;
var
   i : Integer;
   rec : PVarRec;
begin
   for i:=0 to High(VarRecArray) do begin
      rec:=@VarRecArray[i];
      case rec.VType of
         vtInt64 : rec.VInt64:=@FIntegers[rec.VInteger];
         vtExtended : rec.VExtended:=@FFloats[rec.VInteger];
         vtUnicodeString : rec.VString:=Pointer(FStrings[rec.VInteger]);
      end;
   end;
end;

// ------------------
// ------------------ TVarRecArrayContainer ------------------
// ------------------

// Clean
//
procedure TTightList.Clean;
var
   i : Integer;
begin
   case Count of
      0 : Exit;
      1 : TObject(FList).Free;
   else
      for i:=Count-1 downto 0 do
         TObject(FList[i]).Free;
   end;
   Clear;
end;

// Clear
//
procedure TTightList.Clear;
begin
   case Count of
      0 : Exit;
      1 : ;
   else
      FreeMem(FList);
   end;
   FList:=nil;
   FCount:=0;
end;

// Free
//
procedure TTightList.Free;
begin
   Clear;
end;

// GetList
//
function TTightList.GetList : PPointerList;
begin
   if Count=1 then
      Result:=@FList
   else Result:=FList;
end;

// Add
//
function TTightList.Add(item : Pointer) : Integer;
var
   buf : Pointer;
begin
   case Count of
      0 : begin
         FList:=item;
         FCount:=1;
      end;
      1 : begin
         buf:=FList;
         FList:=AllocMem(2*SizeOf(Pointer));
         FList[0]:=buf;
         FList[1]:=item;
         FCount:=2;
      end;
   else
      Inc(FCount);
      ReallocMem(FList, Count*SizeOf(Pointer));
      FList[Count-1]:=item;
   end;
   Result:=FCount-1;
end;

// Assign
//
procedure TTightList.Assign(const aList : TTightList);
begin
   Clear;
   FCount:=aList.FCount;
   case Count of
      0 : Exit;
      1 : FList:=aList.FList;
   else
      ReallocMem(FList, Count*SizeOf(Pointer));
      System.Move(aList.FList^, FList^, Count*SizeOf(Pointer));
   end;
end;

// IndexOf
//
function TTightList.IndexOf(item : Pointer) : Integer;
begin
   case Count of
      0 : Result:=-1;
      1 : if FList=item then Result:=0 else Result:=-1;
   else
      Result:=0;
      while Result<FCount do begin
         if FList[Result]=item then Exit;
         Inc(Result);
      end;
      Result:=-1;
   end;
end;

// Remove
//
function TTightList.Remove(item : Pointer) : Integer;
begin
   Result:=IndexOf(item);
   if Result>=0 then
      Delete(Result);
end;

// Delete
//
procedure TTightList.Delete(index : Integer);
var
   i : Integer;
   buf : Pointer;
begin
   if Cardinal(index)>=Cardinal(Count) then
      RaiseIndexOutOfBounds
   else begin
      case Count of
         1 : begin
            FList:=nil;
            FCount:=0;
         end;
         2 : begin
            buf:=FList;
            if index=0 then
               FList:=FList[1]
            else FList:=FList[0];
            FreeMem(buf);
            FCount:=1;
         end;
      else
         for i:=index+1 to Count-1 do
            FList[i-1]:=FList[i];
         Dec(FCount);
      end;
   end;
end;

// Insert
//
procedure TTightList.Insert(index : Integer; item : Pointer);
var
   i : Integer;
   locList : PPointerList;
begin
   if Cardinal(index)>Cardinal(FCount) then
      RaiseIndexOutOfBounds
   else case Count of
      0 : begin
         FList:=item;
         FCount:=1;
      end;
      1 : begin
         if index=1 then
            Add(item)
         else begin
            Add(FList);
            FList[0]:=item;
         end;
      end;
   else
      ReallocMem(FList, (FCount+1)*SizeOf(Pointer));
      locList:=FList;
      for i:=Count-1 downto index do
         locList[i+1]:=locList[i];
      locList[index]:=item;
      Inc(FCount);
   end;
end;

// Move
//
procedure TTightList.Move(curIndex, newIndex : Integer);
var
   item : Pointer;
begin
   if (Cardinal(curIndex)>=Cardinal(FCount)) or (Cardinal(newIndex)>=Cardinal(FCount)) then
      RaiseIndexOutOfBounds
   else begin
      case curIndex-newIndex of
         0 : ; // ignore
         -1, 1 : Exchange(curIndex, newIndex);
      else
         item:=FList[curIndex];
         Delete(curIndex);
         Insert(newIndex, item);
      end;
   end;
end;

// Exchange
//
procedure TTightList.Exchange(index1, index2 : Integer);
var
   item : Pointer;
begin
   if index1<>index2 then begin
      item:=FList[index1];
      FList[index1]:=FList[index2];
      FList[index2]:=item;
   end;
end;

// RaiseIndexOutOfBounds
//
procedure TTightList.RaiseIndexOutOfBounds;
begin
   raise Exception.Create('List index out of bounds');
end;

// ------------------
// ------------------ TObjectList<T> ------------------
// ------------------

// Destroy
//
destructor TObjectList<T>.Destroy;
begin
   Clear;
   inherited;
end;

// GetItem
//
function TObjectList<T>.GetItem(index : Integer) : T;
begin
   Result:=FItems[index];
end;

// SetItem
//
procedure TObjectList<T>.SetItem(index : Integer; const item : T);
begin
   FItems[index]:=item;
end;

// Add
//
function TObjectList<T>.Add(const anItem : T) : Integer;
begin
   if Count=Length(FItems) then
      SetLength(FItems, Count+8+(Count shr 4));
   FItems[FCount]:=anItem;
   Inc(FCount);
end;

// IndexOf
//
function TObjectList<T>.IndexOf(const anItem : T) : Integer;
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      if FItems[i]=anItem then Exit(i);
   Result:=-1;
end;

// Extract
//
function TObjectList<T>.Extract(idx : Integer) : T;
begin
   Result:=FItems[idx];
   Move(FItems[idx+1], FItems[idx], SizeOf(T)*(Count-1-idx));
   Dec(FCount);
end;

// ExtractAll
//
procedure TObjectList<T>.ExtractAll;
begin
   FCount:=0;
end;

// Clear
//
procedure TObjectList<T>.Clear;
var
   i : Integer;
begin
   for i:=FCount-1 downto 0 do
      FItems[i].Free;
   FCount:=0;
end;

// ------------------
// ------------------ TSortedList<T> ------------------
// ------------------

// GetItem
//
function TSortedList<T>.GetItem(index : Integer) : T;
begin
   Result:=FItems[index];
end;

// Find
//
function TSortedList<T>.Find(const item : T; var index : Integer) : Boolean;
var
   lo, hi, mid, compResult : Integer;
begin
   Result:=False;
   lo:=0;
   hi:=FCount-1;
   while lo<=hi do begin
      mid:=(lo+hi) shr 1;
      compResult:=Compare(FItems[mid], item);
      if compResult<0 then
         lo:=mid+1
      else begin
         hi:=mid- 1;
         if compResult=0 then
            Result:=True;
      end;
   end;
   index:=lo;
end;

// InsertItem
//
procedure TSortedList<T>.InsertItem(index : Integer; const anItem : T);
begin
   if Count=Length(FItems) then
      SetLength(FItems, Count+8+(Count shr 4));
   if index<Count then
      System.Move(FItems[index], FItems[index+1], (Count-index)*SizeOf(Pointer));
   Inc(FCount);
   FItems[index]:=anItem;
end;

// Add
//
function TSortedList<T>.Add(const anItem : T) : Integer;
begin
   Find(anItem, Result);
   InsertItem(Result, anItem);
end;

// AddOrFind
//
function TSortedList<T>.AddOrFind(const anItem : T; var added : Boolean) : Integer;
begin
   added:=not Find(anItem, Result);
   if added then
      InsertItem(Result, anItem);
end;

// Extract
//
function TSortedList<T>.Extract(const anItem : T) : Integer;
var
   i : Integer;
begin
   if Find(anItem, Result) then begin
      Move(FItems[Result+1], FItems[Result], (FCount-Result-1)*SizeOf(T));
      SetLength(FItems, FCount-1);
      Dec(FCount);
   end else Result:=-1;
end;

// IndexOf
//
function TSortedList<T>.IndexOf(const anItem : T) : Integer;
begin
   if not Find(anItem, Result) then
      Result:=-1;
end;

// Clear
//
procedure TSortedList<T>.Clear;
begin
   SetLength(FItems, 0);
   FCount:=0;
end;

// Clean
//
procedure TSortedList<T>.Clean;
var
   i : Integer;
begin
   for i:=0 to FCount-1 do
      FItems[i].Free;
   Clear;
end;

// ------------------
// ------------------ TSimpleStack<T> ------------------
// ------------------

// Grow
//
procedure TSimpleStack<T>.Grow;
begin
   FCapacity:=FCapacity+8+(FCapacity shr 2);
   SetLength(FItems, FCapacity);
end;

// Push
//
procedure TSimpleStack<T>.Push(const item : T);
begin
   if FCount=FCapacity then Grow;
   FItems[FCount]:=item;
   Inc(FCount);
end;

// Pop
//
procedure TSimpleStack<T>.Pop;
begin
   Dec(FCount);
end;

// GetPeek
//
function TSimpleStack<T>.GetPeek : T;
begin
   Result:=FItems[FCount-1];
end;

// SetPeek
//
procedure TSimpleStack<T>.SetPeek(const item : T);
begin
   FItems[FCount-1]:=item;
end;

// GetItems
//
function TSimpleStack<T>.GetItems(const position : Integer) : T;
begin
   Result:=FItems[FCount-1-position];
end;

// SetItems
//
procedure TSimpleStack<T>.SetItems(const position : Integer; const value : T);
begin
   FItems[FCount-1-position]:=value;
end;

// Clear
//
procedure TSimpleStack<T>.Clear;
begin
   SetLength(FItems, 0);
   FCount:=0;
   FCapacity:=0;
end;

// ------------------
// ------------------ TWriteOnlyBlockStream ------------------
// ------------------

// Create
//
constructor TWriteOnlyBlockStream.Create;
begin
   inherited Create;
   AllocateCurrentBlock;
end;

// Destroy
//
destructor TWriteOnlyBlockStream.Destroy;
begin
   inherited;
   FreeBlocks;
end;

// FreeBlocks
//
procedure TWriteOnlyBlockStream.FreeBlocks;
var
   iterator, next : PPointerArray;
begin
   iterator:=FFirstBlock;
   while iterator<>nil do begin
      next:=PPointerArray(iterator[0]);
      FreeMem(iterator);
      iterator:=next;
   end;
   FCurrentBlock:=nil;
   FFirstBlock:=nil;
   FTotalSize:=0;
end;

// AllocateCurrentBlock
//
procedure TWriteOnlyBlockStream.AllocateCurrentBlock;
var
   newBlock : PPointerArray;
begin
   newBlock:=GetMemory(cWriteOnlyBlockStreamBlockSize+2*SizeOf(Pointer));
   newBlock[0]:=nil;
   FBlockRemaining:=@newBlock[1];
   FBlockRemaining^:=0;

   if FCurrentBlock<>nil then
      FCurrentBlock[0]:=newBlock
   else FFirstBlock:=newBlock;
   FCurrentBlock:=newBlock;
end;

// Clear
//
procedure TWriteOnlyBlockStream.Clear;
begin
   FreeBlocks;
   AllocateCurrentBlock;
end;

// StoreData
//
procedure TWriteOnlyBlockStream.StoreData(var buffer);
var
   n : Integer;
   iterator : PPointerArray;
   dest : PByteArray;
begin
   dest:=@buffer;
   iterator:=FFirstBlock;
   while iterator<>nil do begin
      n:=PInteger(@iterator[1])^;
      if n>0 then begin
         Move(iterator[2], dest^, n);
         dest:=@dest[n];
      end;
      iterator:=iterator[0];
   end;
end;

// StoreData
//
procedure TWriteOnlyBlockStream.StoreData(destStream : TStream);
var
   n : Integer;
   iterator : PPointerArray;
begin
   iterator:=FFirstBlock;
   while iterator<>nil do begin
      n:=PInteger(@iterator[1])^;
      destStream.Write(iterator[2], n);
      iterator:=iterator[0];
   end;
end;

// Seek
//
function TWriteOnlyBlockStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
   if (Origin=soFromCurrent) and (Offset=0) then
      Result:=FTotalSize
   else raise EStreamError.Create('not allowed');
end;

// Read
//
function TWriteOnlyBlockStream.Read(var Buffer; Count: Longint): Longint;
begin
   raise EStreamError.Create('not allowed');
end;

// Write
//
function TWriteOnlyBlockStream.Write(const buffer; count: Longint): Longint;
var
   newBlock : PPointerArray;
   source : PByteArray;
   fraction : Integer;
begin
   Result:=count;
   if count<=0 then Exit;

   Inc(FTotalSize, count);
   source:=@Buffer;

   fraction:=cWriteOnlyBlockStreamBlockSize-FBlockRemaining^;
   if count>fraction then begin
      // does not fit in current block
      if FBlockRemaining^>0 then begin
         // current block contains some data, write fraction, allocate new block
         Move(source^, PByteArray(@FCurrentBlock[2])[FBlockRemaining^], fraction);
         Dec(count, fraction);
         source:=@source[fraction];
         FBlockRemaining^:=cWriteOnlyBlockStreamBlockSize;

         AllocateCurrentBlock;
      end;

      if count>cWriteOnlyBlockStreamBlockSize div 2 then begin
         // large amount still to be written, insert specific block
         newBlock:=GetMemory(count+2*SizeOf(Pointer));
         newBlock[0]:=FCurrentBlock;
         PInteger(@newBlock[1])^:=count;
         Move(source^, newBlock[2], count);
         if FFirstBlock=nil then
            FFirstBlock:=newBlock
         else FCurrentBlock[0]:=newBlock;
         FCurrentBlock:=newBlock;
         AllocateCurrentBlock;
         Exit;
      end;
   end;

   // if we reach here, everything fits in current block
   Move(source^, PByteArray(@FCurrentBlock[2])[FBlockRemaining^], count);
   Inc(FBlockRemaining^, count);
end;

// WriteString
//
procedure TWriteOnlyBlockStream.WriteString(const utf16String : String);
var
   stringCracker : NativeInt;
begin
   if utf16String<>'' then begin
      stringCracker:=NativeInt(utf16String);
      Write(Pointer(stringCracker)^, PInteger(stringCracker-SizeOf(Integer))^*SizeOf(Char));
   end;
end;

// WriteChar
//
procedure TWriteOnlyBlockStream.WriteChar(utf16Char : Char);
begin
   Write(utf16Char, SizeOf(Char));
end;

// ToString
//
function TWriteOnlyBlockStream.ToString : String;
begin
   if FTotalSize>0 then begin

      Assert((FTotalSize and 1) = 0);
      SetLength(Result, FTotalSize div 2);
      StoreData(Result[1]);

   end else Result:='';
end;

// GetSize
//
function TWriteOnlyBlockStream.GetSize: Int64;
begin
   Result:=FTotalSize;
end;

// WriteSubString
//
procedure TWriteOnlyBlockStream.WriteSubString(const utf16String : String; startPos : Integer);
begin
   WriteSubString(utf16String, startPos, Length(utf16String)-startPos+1);
end;

// WriteSubString
//
procedure TWriteOnlyBlockStream.WriteSubString(const utf16String : String; startPos, length : Integer);
var
   p, n : Integer;
begin
   Assert(startPos>=1);
   if length<=0 then Exit;
   n:=System.Length(utf16String);
   if startPos>n then Exit;
   p:=startPos+length-1;
   if p>n then p:=n;
   length:=p-startPos+1;
   if length>0 then
      Write(utf16String[startPos], length*SizeOf(Char));
end;

// ------------------
// ------------------ TTightStack ------------------
// ------------------

// Grow
//
procedure TTightStack.Grow;
begin
   FCapacity:=FCapacity+8+FCapacity shr 1;
   ReallocMem(FList, FCapacity*SizeOf(Pointer));
end;

// Push
//
procedure TTightStack.Push(item : Pointer);
begin
   if FCount=FCapacity then Grow;
   FList[FCount]:=item;
   Inc(FCount);
end;

// Peek
//
function TTightStack.Peek : Pointer;
begin
   Result:=FList[FCount-1];
end;

// Pop
//
procedure TTightStack.Pop;
begin
   Dec(FCount);
end;

// Clear
//
procedure TTightStack.Clear;
begin
   FCount:=0;
end;

// Clean
//
procedure TTightStack.Clean;
begin
   while Count>0 do begin
      TObject(Peek).Free;
      Pop;
   end;
end;

// Free
//
procedure TTightStack.Free;
begin
   FCount:=0;
   FCapacity:=0;
   FreeMem(FList);
   FList:=nil;
end;

// ------------------
// ------------------ TSimpleHash<T> ------------------
// ------------------

// Grow
//
procedure TSimpleHash<T>.Grow;
var
   i, j, n : Integer;
   hashCode : Integer;
   oldBuckets : TSimpleHashBucketArray<T>;
begin
   if FCapacity=0 then
      FCapacity:=16
   else FCapacity:=FCapacity*2;
   FGrowth:=(FCapacity*3) div 4;

   oldBuckets:=FBuckets;
   FBuckets:=nil;
   SetLength(FBuckets, FCapacity);

   n:=FCapacity-1;
   for i:=0 to High(oldBuckets) do begin
      if oldBuckets[i].HashCode=0 then continue;
      j:=HashBucket(oldBuckets[i].HashCode);
      while FBuckets[j].HashCode<>0 do
         j:=(j+1) and n;
      FBuckets[j]:=oldBuckets[i];
   end;
end;

// HashBucket
//
function TSimpleHash<T>.HashBucket(const hashCode : Integer) : Integer;
begin
   Result:=hashCode and (FCapacity-1); // capacity is a power of two
end;

// LinearFind
//
function TSimpleHash<T>.LinearFind(const item : T; var index : Integer) : Boolean;
begin
   repeat
      if FBuckets[index].HashCode=0 then
         Exit(False);
      if SameItem(item, FBuckets[index].Value) then
         Exit(True);
      index:=(index+1) and (FCapacity-1);
   until False;
end;

// Add
//
function TSimpleHash<T>.Add(const anItem : T) : Boolean;
var
   i : Integer;
   hashCode : Integer;
begin
   if FCount>=FGrowth then Grow;

   hashCode:=GetItemHashCode(anItem);
   i:=HashBucket(hashCode);
   if LinearFind(anItem, i) then Exit(False);
   FBuckets[i].HashCode:=hashCode;
   FBuckets[i].Value:=anItem;
   Inc(FCount);
end;

// Extract
//
function TSimpleHash<T>.Extract(const anItem : T) : Boolean;
var
   i : Integer;
   hashCode : Integer;
begin
   hashCode:=GetItemHashCode(anItem);
   i:=HashBucket(hashCode);
   Result:=LinearFind(anItem, i);
   if Result then begin
      FBuckets[i].HashCode:=0;
      Dec(FCount);
   end;
end;

// Contains
//
function TSimpleHash<T>.Contains(const anItem : T) : Boolean;
var
   i : Integer;
begin
   if FCount=0 then Exit(False);
   i:=HashBucket(GetItemHashCode(anItem));
   Result:=LinearFind(anItem, i);
end;

// Match
//
function TSimpleHash<T>.Match(var anItem : T) : Boolean;
var
   i : Integer;
begin
   if FCount=0 then Exit(False);
   i:=HashBucket(GetItemHashCode(anItem));
   Result:=LinearFind(anItem, i);
   if Result then
      anItem:=FBuckets[i].Value;
end;

// Enumerate
//
procedure TSimpleHash<T>.Enumerate(const callBack : TSimpleHashProc<T>);
var
   i : Integer;
begin
   for i:=0 to High(FBuckets) do
      if FBuckets[i].HashCode<>0 then
         callBack(FBuckets[i].Value);
end;

// Clear
//
procedure TSimpleHash<T>.Clear;
begin
   FCount:=0;
   FCapacity:=0;
   FGrowth:=0;
   SetLength(FBuckets, 0);
end;

// ------------------
// ------------------ TSimpleObjectHash<T> ------------------
// ------------------

// SameItem
//
function TSimpleObjectHash<T>.SameItem(const item1, item2 : T) : Boolean;
begin
   Result:=(item1=item2);
end;

// GetItemHashCode
//
function TSimpleObjectHash<T>.GetItemHashCode(const item1 : T) : Integer;
var
   p : NativeInt;
begin
   p:=PNativeInt(@item1)^; // workaround compiler issue
   Result:=(p shr 4)+1;
end;

// Clean
//
procedure TSimpleObjectHash<T>.Clean;
var
   i : Integer;
begin
   for i:=0 to FCapacity-1 do
      if FBuckets[i].HashCode<>0 then
         FBuckets[i].Value.Free;
   Clear;
end;

// ------------------
// ------------------ TSimpleList<T> ------------------
// ------------------

// Add
//
procedure TSimpleList<T>.Add(const item : T);
begin
   if FCount=FCapacity then Grow;
   FItems[FCount]:=item;
   Inc(FCount);
end;

// Extract
//
procedure TSimpleList<T>.Extract(idx : Integer);
begin
   Move(FItems[idx+1], FItems[idx], (FCount-idx-1)*SizeOf(T));
   Dec(FCount);
end;

// Clear
//
procedure TSimpleList<T>.Clear;
begin
   SetLength(FItems, 0);
   FCapacity:=0;
   FCount:=0;
end;

// Grow
//
procedure TSimpleList<T>.Grow;
begin
   FCapacity:=FCapacity+8+(FCapacity shr 2);
   SetLength(FItems, FCapacity);
end;

// GetItems
//
function TSimpleList<T>.GetItems(const idx : Integer) : T;
begin
   Result:=FItems[idx];
end;

// SetItems
//
procedure TSimpleList<T>.SetItems(const idx : Integer; const value : T);
begin
   FItems[idx]:=value;
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



