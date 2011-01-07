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
         FList: PPointerList;

         procedure RaiseIndexOutOfBounds;
         function GetList : PPointerList; inline;

      public
         FCount: Integer;     // exposed so it can be used for direct property access

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

   // TSortedList
   //
   {: List that maintains its elements sorted }
   TSortedList<T> = class
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
         function IndexOf(const anItem : T) : Integer;
         procedure Clear;
         property Items[index : Integer] : T read GetItem; default;
         property Count : Integer read FCount;
   end;

   // TSimpleStack<T>
   //
   {: A minimalistic generic stack }
   TSimpleStack<T> = class
      private
         FItems : array of T;
         FCount : Integer;
      protected
         function GetPeek : T;
         procedure SetPeek(const item : T);
         function GetItems(const position : Integer) : T;
         procedure SetItems(const position : Integer; const value : T);
      public
         procedure Push(const item : T);
         function Pop : T;
         procedure Clear;
         property Peek : T read GetPeek write SetPeek;
         property Items[const position : Integer] : T read GetItems write SetItems;
         property Count : Integer read FCount;
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

// ------------------
// ------------------ TSimpleStack<T> ------------------
// ------------------

// Push
//
procedure TSimpleStack<T>.Push(const item : T);
var
   capacity : Integer;
begin
   capacity:=Length(FItems);
   if FCount=capacity then
      SetLength(FItems, capacity+8+(capacity shr 2));
   FItems[FCount]:=item;
   Inc(FCount);
end;

// Pop
//
function TSimpleStack<T>.Pop : T;
begin
   Result:=FItems[FCount-1];
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
         if FFirstBlock=FCurrentBlock then
            FFirstBlock:=newBlock;
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

