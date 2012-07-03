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
unit dwsUtils;

{$I dws.inc}

interface

uses Classes, SysUtils, Variants, SyncObjs, dwsXPlatform;

type

   // see http://delphitools.info/2011/11/30/fixing-tcriticalsection/
   {$HINTS OFF}
   TFixedCriticalSection = class(TCriticalSection)
      private
         FDummy : array [0..95] of Byte;
   end;
   {$HINTS ON}

   // TRefCountedObject
   //
   // Uses Monitor hidden field to store refcount, so not compatible with monitor use
   // (but Monitor is buggy, so no great loss)
   TRefCountedObject = class
      private
         function  GetRefCount : Integer; inline;
         procedure SetRefCount(n : Integer); inline;
      public
         function  IncRefCount : Integer; inline;
         function  DecRefCount : Integer;
         property  RefCount : Integer read GetRefCount write SetRefCount;
         procedure Free;
   end;

   // IGetSelf
   //
   IGetSelf = interface
      function GetSelf : TObject;
   end;

   // TInterfacedSelfObject
   //
   TInterfacedSelfObject = class (TRefCountedObject, IUnknown, IGetSelf)
      protected
         function GetSelf : TObject;
         function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
         function _AddRef: Integer; stdcall;
         function _Release: Integer; stdcall;

      public
         class function NewInstance: TObject; override;
         procedure AfterConstruction; override;
         procedure BeforeDestruction; override;
   end;

   // IAutoStore
   //
   IAutoStore<T: class> = interface
      function GetValue : T;
      property Value : T read GetValue;
   end;

   // TAutoStore
   //
   TAutoStore<T: class> = class(TInterfacedSelfObject, IAutoStore<T>)
      private
         FValue : T;
      protected
         function GetValue : T;
      public
         constructor Create(value : T);
         destructor Destroy; override;
   end;

   IAutoStrings = IAutoStore<TStrings>;

   // TVarRecArrayContainer
   //
   TVarRecArrayContainer = class
      private
         FIntegers : array of Int64;
         FFloats : array of Extended;
         FStrings : array of UnicodeString;

         function AddVarRec : PVarRec;

      public
         VarRecArray : array of TVarRec;

         constructor Create; overload;
         constructor Create(const variantArray : array of Variant); overload;

         procedure Add(const v : Variant);
         procedure AddBoolean(const b : Boolean);
         procedure AddInteger(const i : Int64);
         procedure AddFloat(const f : Double);
         procedure AddString(const s : UnicodeString);

         procedure Initialize;
   end;

   // TTightList
   //
   {: Compact list embedded in a record.<p>
      If the list holds only 1 item, no dynamic memory is allocated
      (the list pointer is used).
      Make sure to Clear or Clean in the destructor of the Owner. }
   TTightListArray = array [0..MaxInt shr 4] of TRefCountedObject;
   PObjectTightList = ^TTightListArray;
   TTightList = record
      private
         FList : PObjectTightList;

         procedure RaiseIndexOutOfBounds;
         function GetList : PObjectTightList; inline;

      public
         FCount : Integer;     // exposed so it can be used for direct property access

         property List : PObjectTightList read GetList;
         property Count : Integer read FCount;

         procedure Free; // to posture as a regular TList
         procedure Clean;  // clear the list and free the item objects
         procedure Clear;  // clear the list without freeing the items
         function Add(item : TRefCountedObject) : Integer;
         procedure Assign(const aList : TTightList);
         function IndexOf(item : TRefCountedObject) : Integer;
         function Remove(item : TRefCountedObject) : Integer;
         procedure Delete(index : Integer);
         procedure Insert(index : Integer; item : TRefCountedObject);
         procedure Move(curIndex, newIndex : Integer);
         procedure Exchange(index1, index2 : Integer);
   end;

   // TTightStack
   //
   {: Embeddable stack functionality }
   TTightStack = record
      private
         FList : PObjectTightList;
         FCount : Integer;
         FCapacity : Integer;

         procedure Grow;

      public
         procedure Push(item : TRefCountedObject); inline;
         function  Peek : TRefCountedObject; inline;
         procedure Pop; inline;

         procedure Clear; inline;
         procedure Clean;
         procedure Free;

         property List : PObjectTightList read FList;
         property Count : Integer read FCount;
   end;

   TSimpleCallbackStatus = (csContinue, csAbort);

   TSimpleCallback<T> = reference to function (var item : T) : TSimpleCallbackStatus;

   // TSimpleQueue
   //
   {: A minimalistic generic FIFO queue. }
   TSimpleQueue<T> = class
      private
         { Private Declarations }
         FItems : array of T;
         FHead, FTail, FCount : Integer;
         procedure SetCapacity(newCapacity : Integer);

		protected
         { Protected Declarations }
         class var vDefault_T : T;

      public
         { Public Declarations }
         procedure EnQueue(const value : T);
         procedure DeQueue;
         function Peek : T; inline;
         procedure Clear;
         procedure Enumerate(const callback : TSimpleCallback<T>);
         property Count : Integer read FCount;
   end;

   // TArrayObjectList<T>
   //
   {: An embeddable wrapped array. }
   TArrayObjectList<T: TRefCountedObject> = record
      private
         FCount : Integer;

         function GetItems(const idx : Integer) : T; inline;
         procedure SetItems(const idx : Integer; const value : T); inline;

      public
         List : TArray<T>;

         procedure Add(const item : T);
         procedure Delete(idx : Integer);
         procedure Clear;
         procedure Clean;
         property Items[const position : Integer] : T read GetItems write SetItems; default;
         property Count : Integer read FCount;
   end;

   // TSimpleList<T>
   //
   {: A minimalistic generic list class. }
   TSimpleList<T> = class
      private
         FItems : array of T;
         FCount : Integer;
         FCapacity : Integer;
      protected
         procedure Grow;
         function GetItems(const idx : Integer) : T;
         procedure SetItems(const idx : Integer; const value : T);
      public
         procedure Add(const item : T);
         procedure Extract(idx : Integer);
         procedure Clear;
         procedure Enumerate(const callback : TSimpleCallback<T>);
         property Items[const position : Integer] : T read GetItems write SetItems; default;
         property Count : Integer read FCount;
   end;

   // TObjectList
   //
   {: A simple generic object list, owns objects }
   TObjectList<T: TRefCountedObject> = class
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
   TSortedList<T: TRefCountedObject> = class
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
         function Extract(const anItem : T) : Integer; overload;
         function Extract(index : Integer) : T; overload;
         function IndexOf(const anItem : T) : Integer;
         procedure Clear;
         procedure Clean;
         procedure Enumerate(const callback : TSimpleCallback<T>);
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
      HashCode : Cardinal;
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
         function LinearFind(const item : T; var index : Integer) : Boolean;
         function SameItem(const item1, item2 : T) : Boolean; virtual; abstract;
         // hashCode must be non-null
         function GetItemHashCode(const item1 : T) : Integer; virtual; abstract;

      public
         function Add(const anItem : T) : Boolean; // true if added
         function Replace(const anItem : T) : Boolean; // true if added
         function Contains(const anItem : T) : Boolean;
         function Match(var anItem : T) : Boolean;
         procedure Enumerate(const callBack : TSimpleHashProc<T>);
         procedure Clear;

         property Count : Integer read FCount;
   end;

   TSimpleObjectHash<T: TRefCountedObject> = class(TSimpleHash<T>)
      protected
         function SameItem(const item1, item2 : T) : Boolean; override;
         function GetItemHashCode(const item1 : T) : Integer; override;

      public
         procedure Clean;
   end;

   TNameObjectHashBucket<T: TRefCountedObject> = record
      Name : String;
      Obj : T;
   end;

   TSimpleNameObjectHash<T: TRefCountedObject> = class(TSimpleHash<TNameObjectHashBucket<T>>)
      protected
         function SameItem(const item1, item2 : TNameObjectHashBucket<T>) : Boolean; override;
         function GetItemHashCode(const item1 : TNameObjectHashBucket<T>) : Integer; override;

         function GetObjects(const name : String) : T;
         procedure SetObjects(const name : String; obj : T);

      public
         function AddObject(const name : String; obj : T; replace : Boolean = False) : Boolean;

         property Objects[const name : String] : T read GetObjects write SetObjects; default;
   end;

   TObjectObjectHashBucket<TKey, TValue: TRefCountedObject> = record
      Key : TKey;
      Value : TValue;
   end;

   TSimpleObjectObjectHash<TKey, TValue: TRefCountedObject> = class(TSimpleHash<TObjectObjectHashBucket<TKey, TValue>>)
      protected
         function SameItem(const item1, item2 : TObjectObjectHashBucket<TKey, TValue>) : Boolean; override;
         function GetItemHashCode(const item1 : TObjectObjectHashBucket<TKey, TValue>) : Integer; override;

      public
         function  GetValue(key : TKey) : TValue;
         procedure SetValue(key : TKey; value : TValue);

         procedure CleanValues;
   end;

   TObjectsLookup = class (TSortedList<TRefCountedObject>)
      protected
         function Compare(const item1, item2 : TRefCountedObject) : Integer; override;
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
         // must be strictly an utf16 UnicodeString
         procedure WriteString(const utf16String : UnicodeString);
         procedure WriteSubString(const utf16String : UnicodeString; startPos : Integer); overload;
         procedure WriteSubString(const utf16String : UnicodeString; startPos, length : Integer); overload;
         procedure WriteChar(utf16Char : WideChar);
         // assumes data is an utf16 UnicodeString
         function ToString : UnicodeString; override;

         procedure Clear;

         procedure StoreData(var buffer); overload;
         procedure StoreData(destStream : TStream); overload;
   end;

   TFastCompareStringList = class (TStringList)
      function CompareStrings(const S1, S2: UnicodeString): Integer; override;
   end;

   TFastCompareTextList = class (TStringList)
      function CompareStrings(const S1, S2: UnicodeString): Integer; override;
   end;

   ETightListOutOfBound = class(Exception);

{: Changes the class of an object (by altering the VMT pointer).<p>
   Only checks IntanceSize.
   Use only if you understand fully what the above means. }
procedure ChangeObjectClass(ref : TObject; newClass : TClass);

procedure UnifyAssignString(const fromStr : UnicodeString; var toStr : UnicodeString);
procedure TidyStringsUnifier;

function UnicodeCompareLen(p1, p2 : PWideChar; n : Integer) : Integer;
function UnicodeCompareText(const s1, s2 : UnicodeString) : Integer;
function UnicodeSameText(const s1, s2 : UnicodeString) : Boolean;

function StrIBeginsWith(const aStr, aBegin : UnicodeString) : Boolean;
function StrBeginsWith(const aStr, aBegin : UnicodeString) : Boolean;

function StrCountChar(const aStr : UnicodeString; c : Char) : Integer;

function Min(a, b : Integer) : Integer; inline;

function SimpleStringHash(const s : String) : Cardinal;

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

// SimpleStringHash
//
function SimpleStringHash(const s : String) : Cardinal;
var
   i : Integer;
begin
   Result:=Length(s);
   for i:=1 to Result do
      Result:=((Result shl 2) or (Result shr 30)) xor Ord(s[i]);
end;

// ------------------
// ------------------ UnicodeString Unifier ------------------
// ------------------

type
   {$IF CompilerVersion > 22}
   TStringListList = TStringItemList;
   {$ELSE}
   TStringListList = PStringItemList;
   {$IFEND}

   TStringListCracker = class (TStrings)
      private
         FList : TStringListList;
   end;

   TUnifierStringList = class (TFastCompareStringList)
      FLock : TFixedCriticalSection;
      constructor Create;
      destructor Destroy; override;
   end;

var
   vCharStrings : array [0..127] of TUnifierStringList;

// CompareStrings
//
function TFastCompareStringList.CompareStrings(const S1, S2: UnicodeString): Integer;
begin
   Result:=CompareStr(S1, S2);
end;

// TUnifierStringList.Create
//
constructor TUnifierStringList.Create;
begin
   inherited;
   FLock:=TFixedCriticalSection.Create;
   Sorted:=True;
   Duplicates:=dupIgnore;
end;

// Destroy
//
destructor TUnifierStringList.Destroy;
begin
   inherited;
   FLock.Destroy;
end;

// InitializeStringsUnifier
//
procedure InitializeStringsUnifier;
var
   i : Integer;
begin
   for i:=Low(vCharStrings) to High(vCharStrings) do
      vCharStrings[i]:=TUnifierStringList.Create;
end;

// FinalizeStringsUnifier
//
procedure FinalizeStringsUnifier;
var
   i : Integer;
begin
   for i:=Low(vCharStrings) to High(vCharStrings) do begin
      vCharStrings[i].Free;
      vCharStrings[i]:=nil;
   end;
end;

// UnifyAssignString
//
procedure UnifyAssignString(const fromStr : UnicodeString; var toStr : UnicodeString);
var
   i : Integer;
   sl : TUnifierStringList;
begin
   if fromStr='' then
      toStr:=''
   else begin
      i:=Ord(fromStr[1]) and High(vCharStrings);
      sl:=vCharStrings[i];
      sl.FLock.Enter;
      i:=sl.AddObject(fromStr, nil);
      toStr:=TStringListCracker(sl).FList[i].FString;
      sl.FLock.Leave;
   end;
end;

// TidyStringsUnifier
//
procedure TidyStringsUnifier;
var
   i : Integer;
   sl : TUnifierStringList;
begin
   for i:=Low(vCharStrings) to High(vCharStrings) do begin
      sl:=vCharStrings[i];
      sl.FLock.Enter;
      sl.Clear;
      sl.FLock.Leave;
   end;
end;

// UnicodeCompareLen
//
function UnicodeCompareLen(p1, p2 : PWideChar; n : Integer) : Integer;
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
function UnicodeCompareText(const s1, s2 : UnicodeString) : Integer;
var
   n1, n2, dn : Integer;
begin
   if S1<>'' then begin
      if S2<>'' then begin
         n1:=Length(s1);
         n2:=Length(s2);
         dn:=n1-n2;
         if dn<0 then begin
            Result:=UnicodeCompareLen(PWideChar(NativeInt(s1)), PWideChar(NativeInt(s2)), n1);
            if Result=0 then
               Result:=-1;
         end else begin
            Result:=UnicodeCompareLen(PWideChar(NativeInt(S1)), PWideChar(NativeInt(s2)), n2);
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
function UnicodeSameText(const s1, s2 : UnicodeString) : Boolean;
begin
   Result:=(Length(s1)=Length(s2)) and (UnicodeCompareText(s1, s2)=0)
end;

// StrIBeginsWith
//
function StrIBeginsWith(const aStr, aBegin : UnicodeString) : Boolean;
var
   n1, n2 : Integer;
begin
   n1:=Length(aStr);
   n2:=Length(aBegin);
   if (n2>n1) or (n2=0) then
      Result:=False
   else Result:=(UnicodeCompareLen(PWideChar(aStr), PWideChar(aBegin), n2)=0);
end;

// StrBeginsWith
//
function StrBeginsWith(const aStr, aBegin : UnicodeString) : Boolean;
var
   n1, n2 : Integer;
begin
   n1:=Length(aStr);
   n2:=Length(aBegin);
   if (n2>n1) or (n2=0) then
      Result:=False
   else Result:=CompareMem(PWideChar(aStr), PWideChar(aBegin), n2);
end;

// StrCountChar
//
function StrCountChar(const aStr : UnicodeString; c : Char) : Integer;
var
   i : Integer;
begin
   Result:=0;
   for i:=1 to Length(aStr) do
      if aStr[i]=c then
         Inc(Result);
end;

// Min
//
function Min(a, b : Integer) : Integer;
begin
   if a<b then
      Result:=a
   else Result:=b;
end;

// ------------------
// ------------------ TFastCompareTextList ------------------
// ------------------

// CompareStrings
//
function TFastCompareTextList.CompareStrings(const S1, S2: UnicodeString): Integer;
begin
   Result:=UnicodeCompareText(s1, s2);
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
procedure TVarRecArrayContainer.AddString(const s : UnicodeString);
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
      1 : TRefCountedObject(FList).Free;
   else
      for i:=Count-1 downto 0 do
         FList[i].Free;
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
function TTightList.GetList : PObjectTightList;
begin
   if Count=1 then
      Result:=@FList
   else Result:=FList;
end;

// Add
//
function TTightList.Add(item : TRefCountedObject) : Integer;
var
   buf : Pointer;
begin
   case Count of
      0 : begin
         FList:=PObjectTightList(item);
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
function TTightList.IndexOf(item : TRefCountedObject) : Integer;
begin
   case Count of
      0 : Result:=-1;
      1 : begin
         if FList=PObjectTightList(item) then
            Result:=0
         else Result:=-1;
      end;
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
function TTightList.Remove(item : TRefCountedObject) : Integer;
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
               FList:=PObjectTightList(FList[1])
            else FList:=PObjectTightList(FList[0]);
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
procedure TTightList.Insert(index : Integer; item : TRefCountedObject);
var
   i : Integer;
   locList : PObjectTightList;
begin
   if Cardinal(index)>Cardinal(FCount) then
      RaiseIndexOutOfBounds
   else case Count of
      0 : begin
         FList:=PObjectTightList(item);
         FCount:=1;
      end;
      1 : begin
         if index=1 then
            Add(item)
         else begin
            Add(TRefCountedObject(FList));
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
   raise ETightListOutOfBound.Create('List index out of bounds');
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
   if Find(anItem, Result) then
      Extract(Result)
   else Result:=-1;
end;

// Extract
//
function TSortedList<T>.Extract(index : Integer) : T;
var
   n : Integer;
begin
   Dec(FCount);
   Result:=FItems[index];
   n:=FCount-index;
   if n>0 then
      Move(FItems[index+1], FItems[index], n*SizeOf(T));
   SetLength(FItems, FCount);
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

// Enumerate
//
procedure TSortedList<T>.Enumerate(const callback : TSimpleCallback<T>);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      if callback(FItems[i])=csAbort then
         Break;
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
         FCurrentBlock[0]:=newBlock;
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
procedure TWriteOnlyBlockStream.WriteString(const utf16String : UnicodeString);
var
   stringCracker : NativeInt;
begin
   if utf16String<>'' then begin
      stringCracker:=NativeInt(utf16String);
      Write(Pointer(stringCracker)^, PInteger(stringCracker-SizeOf(Integer))^*SizeOf(WideChar));
   end;
end;

// WriteChar
//
procedure TWriteOnlyBlockStream.WriteChar(utf16Char : WideChar);
begin
   Write(utf16Char, SizeOf(WideChar));
end;

// ToString
//
function TWriteOnlyBlockStream.ToString : UnicodeString;
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
procedure TWriteOnlyBlockStream.WriteSubString(const utf16String : UnicodeString; startPos : Integer);
begin
   WriteSubString(utf16String, startPos, Length(utf16String)-startPos+1);
end;

// WriteSubString
//
procedure TWriteOnlyBlockStream.WriteSubString(const utf16String : UnicodeString; startPos, length : Integer);
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
      Write(utf16String[startPos], length*SizeOf(WideChar));
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
procedure TTightStack.Push(item : TRefCountedObject);
begin
   if FCount=FCapacity then Grow;
   FList[FCount]:=item;
   Inc(FCount);
end;

// Peek
//
function TTightStack.Peek : TRefCountedObject;
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
      TRefCountedObject(Peek).Free;
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
      FCapacity:=32
   else FCapacity:=FCapacity*2;
   FGrowth:=(FCapacity*3) div 4;

   oldBuckets:=FBuckets;
   FBuckets:=nil;
   SetLength(FBuckets, FCapacity);

   n:=FCapacity-1;
   for i:=0 to High(oldBuckets) do begin
      if oldBuckets[i].HashCode=0 then continue;
      j:=(oldBuckets[i].HashCode and (FCapacity-1));
      while FBuckets[j].HashCode<>0 do
         j:=(j+1) and n;
      FBuckets[j]:=oldBuckets[i];
   end;
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
   i:=(hashCode and (FCapacity-1));
   if LinearFind(anItem, i) then Exit(False);
   FBuckets[i].HashCode:=hashCode;
   FBuckets[i].Value:=anItem;
   Inc(FCount);
   Result:=True;
end;

// Replace
//
function TSimpleHash<T>.Replace(const anItem : T) : Boolean;
var
   i : Integer;
   hashCode : Integer;
begin
   if FCount>=FGrowth then Grow;

   hashCode:=GetItemHashCode(anItem);
   i:=(hashCode and (FCapacity-1));
   if LinearFind(anItem, i) then begin
      FBuckets[i].Value:=anItem
   end else begin
      FBuckets[i].HashCode:=hashCode;
      FBuckets[i].Value:=anItem;
      Inc(FCount);
      Result:=True;
   end;
end;

// Contains
//
function TSimpleHash<T>.Contains(const anItem : T) : Boolean;
var
   i : Integer;
begin
   if FCount=0 then Exit(False);
   i:=(GetItemHashCode(anItem) and (FCapacity-1));
   Result:=LinearFind(anItem, i);
end;

// Match
//
function TSimpleHash<T>.Match(var anItem : T) : Boolean;
var
   i : Integer;
begin
   if FCount=0 then Exit(False);
   i:=(GetItemHashCode(anItem) and (FCapacity-1));
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
// ------------------ TSimpleQueue<T> ------------------
// ------------------

// EnQueue
//
procedure TSimpleQueue<T>.EnQueue(const value : T);
var
   n : Integer;
begin
   n:=Length(FItems);
   if Count=n then begin
      n:=n*2+8;
      SetCapacity(n);
   end;
   FItems[FHead]:=Value;
   FHead:=(FHead+1) mod n;
   Inc(FCount);
end;

// DeQueue
//
procedure TSimpleQueue<T>.DeQueue;
begin
   Assert(FCount>0);
   FItems[FTail]:=vDefault_T;
   FTail:=(FTail+1) mod Length(FItems);
   Dec(FCount);
end;

// Peek
//
function TSimpleQueue<T>.Peek : T;
begin
   Result:=FItems[FTail];
end;

// Clear
//
procedure TSimpleQueue<T>.Clear;
begin
   SetLength(FItems, 0);
   FHead:=0;
   FTail:=0;
   FCount:=0;
end;

// Enumerate
//
procedure TSimpleQueue<T>.Enumerate(const callback : TSimpleCallback<T>);
var
   i, n : Integer;
begin
   n:=Length(FItems);
   for i:=0 to Count-1 do begin
      if callback(FItems[(FTail+i) mod n])=csAbort then
         Break;
   end;
end;

// SetCapacity
//
procedure TSimpleQueue<T>.SetCapacity(newCapacity : Integer);
var
   tailCount, offset, i : Integer;
begin
   offset:=newCapacity-Length(FItems);
   if offset=0 then Exit;

   // If head <= tail, then part of the queue wraps around
   // the end of the array; don't introduce a gap in the queue.
   if (FHead<FTail) or ((FHead=FTail) and (Count>0)) then
      tailCount:=Length(FItems)-FTail
   else tailCount:=0;

   if offset>0 then
      SetLength(FItems, newCapacity);
   if tailCount>0 then begin
      System.Move(FItems[FTail], FItems[FTail+offset], tailCount*SizeOf(T));
      if offset>0 then
         System.FillChar(FItems[FTail], offset*SizeOf(T), 0);
      Inc(FTail, offset);
   end;
   if offset<0 then
      SetLength(FItems, newCapacity);
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
var
   n : Integer;
begin
   n:=FCount-idx-1;
   if n>0 then
      Move(FItems[idx+1], FItems[idx], n*SizeOf(T))
   else FillChar(FItems[idx], SizeOf(T), 0);
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

// Enumerate
//
procedure TSimpleList<T>.Enumerate(const callback : TSimpleCallback<T>);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      if callBack(FItems[i])=csAbort then
         Break;
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

// ------------------
// ------------------ TObjectsLookup ------------------
// ------------------

// Compare
//
function TObjectsLookup.Compare(const item1, item2 : TRefCountedObject) : Integer;
begin
   if NativeUInt(item1)<NativeUInt(item2) then
      Result:=-1
   else if NativeUInt(item1)=NativeUInt(item2) then
      Result:=0
   else Result:=-1;
end;

// ------------------
// ------------------ TInterfacedSelfObject ------------------
// ------------------

// GetSelf
//
function TInterfacedSelfObject.GetSelf : TObject;
begin
   Result:=Self;
end;

// QueryInterface
//
function TInterfacedSelfObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
   if GetInterface(IID, Obj) then
      Result:=0
   else Result:=E_NOINTERFACE;
end;

// _AddRef
//
function TInterfacedSelfObject._AddRef: Integer;
begin
   Result:=IncRefCount;
end;

// _Release
//
function TInterfacedSelfObject._Release: Integer;
begin
   Result:=DecRefCount;
   if Result=0 then Destroy;
end;

// NewInstance
//
class function TInterfacedSelfObject.NewInstance: TObject;
begin
   Result:=inherited NewInstance;
   TRefCountedObject(Result).IncRefCount;
end;

// AfterConstruction
//
procedure TInterfacedSelfObject.AfterConstruction;
begin
   // Release the constructor's implicit refcount
   DecRefCount;
end;

// BeforeDestruction
//
procedure TInterfacedSelfObject.BeforeDestruction;
begin
   Assert(RefCount=0);
end;

// ------------------
// ------------------ TAutoStore<T> ------------------
// ------------------

// GetValue
//
function TAutoStore<T>.GetValue : T;
begin
   Result:=FValue;
end;

// Create
//
constructor TAutoStore<T>.Create(value : T);
begin
   FValue:=value;
end;

// Destroy
//
destructor TAutoStore<T>.Destroy;
begin
   FValue.Free;
end;

// ------------------
// ------------------ TSimpleNameObjectHash<T> ------------------
// ------------------

// SameItem
//
function TSimpleNameObjectHash<T>.SameItem(const item1, item2 : TNameObjectHashBucket<T>) : Boolean;
begin
   Result:=(item1.Name=item2.Name);
end;

// GetItemHashCode
//
function TSimpleNameObjectHash<T>.GetItemHashCode(const item1 : TNameObjectHashBucket<T>) : Integer;
begin
   Result:=SimpleStringHash(item1.Name);
end;

// GetObjects
//
function TSimpleNameObjectHash<T>.GetObjects(const name : String) : T;
var
   h : Cardinal;
   i : Integer;
begin
   if FCount=0 then
      Exit(T(TObject(nil)));  // workaround for D2010 compiler bug

   h:=SimpleStringHash(name);
   i:=(h and (FCapacity-1));

   repeat
      with FBuckets[i] do begin
         if HashCode=0 then
            Exit(T(TObject(nil)));  // workaround for D2010 compiler bug
         if (HashCode=h) and (Value.Name=name) then begin
            Result:=Value.Obj;
            Exit;
         end;
      end;
      i:=(i+1) and (FCapacity-1);
   until False;
end;

// SetObjects
//
procedure TSimpleNameObjectHash<T>.SetObjects(const name : String; obj : T);
begin
   AddObject(name, obj, True);
end;

// AddObject
//
function TSimpleNameObjectHash<T>.AddObject(const name : String; obj : T;
                                            replace : Boolean = False) : Boolean;
var
   i : Integer;
   h : Cardinal;
begin
   if FCount>=FGrowth then Grow;

   h:=SimpleStringHash(name);
   i:=(h and (FCapacity-1));

   repeat
      with FBuckets[i] do begin
         if HashCode=0 then
            Break
         else if (HashCode=h) and (Value.Name=name) then begin
            if replace then
               Value.Obj:=obj;
            Exit(False);
         end;
      end;
      i:=(i+1) and (FCapacity-1);
   until False;

   with FBuckets[i] do begin
      HashCode:=h;
      Value.Name:=name;
      Value.Obj:=obj;
   end;
   Inc(FCount);
   Result:=True;
end;

// ------------------
// ------------------ TArrayObjectList<T> ------------------
// ------------------

// Add
//
procedure TArrayObjectList<T>.Add(const item : T);
var
   n : Integer;
begin
   n:=FCount;
   SetLength(List, n+1);
   List[n]:=item;
   Inc(FCount);
end;

// Delete
//
procedure TArrayObjectList<T>.Delete(idx : Integer);
var
   n : Integer;
begin
   n:=FCount-1;
   if idx<n then
      System.Move(List[idx+1], List[idx], (n-idx)*SizeOf(TObject));
   SetLength(List, n);
   Dec(FCount);
end;

// Clear
//
procedure TArrayObjectList<T>.Clear;
begin
   SetLength(List, 0);
   FCount:=0;
end;

// Clean
//
procedure TArrayObjectList<T>.Clean;
var
   i : Integer;
begin
   for i:=0 to High(List) do
      List[i].Free;
   Clear;
end;

// GetItems
//
function TArrayObjectList<T>.GetItems(const idx : Integer) : T;
begin
   Result:=List[idx];
end;

// SetItems
//
procedure TArrayObjectList<T>.SetItems(const idx : Integer; const value : T);
begin
   List[idx]:=value;
end;

// ------------------
// ------------------ TRefCountedObject ------------------
// ------------------

// Free
//
procedure TRefCountedObject.Free;
begin
   if Self<>nil then
      DecRefCount;
end;

// IncRefCount
//
function TRefCountedObject.IncRefCount : Integer;
var
   p : PInteger;
begin
   p:=PInteger(NativeInt(Self)+InstanceSize-hfFieldSize+hfMonitorOffset);
   Result:=InterlockedIncrement(p^);
end;

// DecRefCount
//
function TRefCountedObject.DecRefCount : Integer;
var
   p : PInteger;
begin
   p:=PInteger(NativeInt(Self)+InstanceSize-hfFieldSize+hfMonitorOffset);
   if p^=0 then begin
      Destroy;
      Result:=0;
   end else Result:=InterlockedDecrement(p^);
end;

// GetRefCount
//
function TRefCountedObject.GetRefCount : Integer;
var
   p : PInteger;
begin
   p:=PInteger(NativeInt(Self)+InstanceSize-hfFieldSize+hfMonitorOffset);
   Result:=p^;
end;

// SetRefCount
//
procedure TRefCountedObject.SetRefCount(n : Integer);
var
   p : PInteger;
begin
   p:=PInteger(NativeInt(Self)+InstanceSize-hfFieldSize+hfMonitorOffset);
   p^:=n;
end;

// ------------------
// ------------------ TSimpleObjectObjectHash<T1, T2> ------------------
// ------------------

// SameItem
//
function TSimpleObjectObjectHash<TKey, TValue>.SameItem(const item1, item2 : TObjectObjectHashBucket<TKey, TValue>) : Boolean;
begin
   Result:=(item1.Key=item2.Key);
end;

// GetItemHashCode
//
function TSimpleObjectObjectHash<TKey, TValue>.GetItemHashCode(const item1 : TObjectObjectHashBucket<TKey, TValue>) : Integer;
begin
   Result:=(PNativeInt(@item1.Key)^ shr 2);
end;

// GetValue
//
function TSimpleObjectObjectHash<TKey, TValue>.GetValue(key : TKey) : TValue;
var
   bucket : TObjectObjectHashBucket<TKey,TValue>;
begin
   bucket.Key:=key;
   if Match(bucket) then
      Result:=bucket.Value
   else Result:=TValue(TObject(nil));  // workaround for D2010 compiler bug
end;

// SetValue
//
procedure TSimpleObjectObjectHash<TKey, TValue>.SetValue(key : TKey; value : TValue);
var
   bucket : TObjectObjectHashBucket<TKey,TValue>;
begin
   bucket.Key:=key;
   bucket.Value:=value;
   Replace(bucket);
end;

// CleanValues
//
procedure TSimpleObjectObjectHash<TKey, TValue>.CleanValues;
var
   i : Integer;
begin
   for i:=0 to FCapacity-1 do begin
      if FBuckets[i].HashCode<>0 then
         FBuckets[i].Value.Value.Free;
   end;
   Clear;
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

