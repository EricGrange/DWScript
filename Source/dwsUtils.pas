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

uses
   Classes, SysUtils, Variants, Types, StrUtils, Masks,
   dwsStrings, dwsXPlatform, Math;

type

   TStringDynArray = array of UnicodeString;

   TInt64Array = array [0..High(MaxInt) shr 4] of Int64;
   PInt64Array = ^TInt64Array;

   // TRefCountedObject
   //
   // Uses Monitor hidden field to store refcount, so not compatible with monitor use
   // (but Monitor is buggy, so no great loss)
   TRefCountedObject = class
      private
         {$ifdef FPC}
         FRefCount : Integer;
         {$endif}
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
      ['{77D8EA0B-311C-422B-B8DE-AA5BDE726E41}']
      function GetSelf : TObject;
      function ToString : String;
   end;

   // TInterfacedSelfObject
   //
   TInterfacedSelfObject = class (TRefCountedObject, IUnknown, IGetSelf)
      protected
         function GetSelf : TObject;
         function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; stdcall;
         function _AddRef : Integer; stdcall;
         function _Release : Integer; stdcall;

      public
         class function NewInstance: TObject; override;
         procedure AfterConstruction; override;
         procedure BeforeDestruction; override;
   end;

   // IAutoStrings
   //
   IAutoStrings = interface
      function GetValue : TStringList;
      property Value : TStringList read GetValue;
      function Clone : IAutoStrings;
   end;

   // TAutoStrings
   //
   TAutoStrings = class(TInterfacedSelfObject, IAutoStrings)
      private
         FValue : TStringList;
      protected
         function GetValue : TStringList;
         function Clone : IAutoStrings;
      public
         constructor Create;
         constructor CreateCapture(value : TStringList);
         constructor CreateClone(value : TStringList);
         destructor Destroy; override;
   end;

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
         procedure MoveItem(curIndex, newIndex : Integer); // Note: D2009 fails if this method is called Move (!) - HV
         procedure Exchange(index1, index2 : Integer);
         function ItemsAllOfClass(aClass : TClass) : Boolean;
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
         function  Peek : TRefCountedObject;  overload; inline;
         function  Peek(n : Integer) : TRefCountedObject; overload;
         procedure Pop; inline;

         procedure Clear; inline;
         procedure Clean;
         procedure Free;

         property List : PObjectTightList read FList;
         property Count : Integer read FCount;
   end;

   TSimpleCallbackStatus = (csContinue, csAbort);

   TSimpleCallback<T> = function (var item : T) : TSimpleCallbackStatus;

   // TSimpleList<T>
   //
   {: A minimalistic generic list class. }
   TSimpleList<T> = class
      private
         type
            ArrayT = array of T;
         var
            FItems : ArrayT;
            FCount : Integer;
            FCapacity : Integer;

      protected
         procedure Grow;
         function GetItems(const idx : Integer) : T; {$IFDEF DELPHI_2010_MINUS}{$ELSE} inline; {$ENDIF}
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
   TObjectList<T{$IFNDEF FPC}: TRefCountedObject{$ENDIF}> = class
      private
         type
            ArrayT = array of T;
         var
            FItems : ArrayT;
            FCount : Integer;

      protected
         function GetItem(index : Integer) : T; {$IFDEF DELPHI_2010_MINUS}{$ELSE} inline; {$ENDIF}
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
   TSortedList<T{$IFNDEF FPC}: TRefCountedObject{$ENDIF}> = class
      private
         type
            ArrayT = array of T;
         var
            FItems : ArrayT;
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
         function ExtractAt(index : Integer) : T;
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
      type ArrayT = array of T;
      var
         FItems : ArrayT;
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

   PSimpleIntegerStackChunk = ^TSimpleIntegerStackChunk;
   TSimpleIntegerStackChunk = record
      public
         const ChunkSize = 16-2;
      public
         Data : array [0..ChunkSize-1] of Integer;
         Prev : PSimpleIntegerStackChunk;
   end;

   // TSimpleIntegerStack
   //
   {: A minimalistic chunked integer stack. }
   TSimpleIntegerStack = class
      private
         FChunk : PSimpleIntegerStackChunk;
         FChunkIndex : Integer;
         FCount : Integer;
         FPooledChunk : PSimpleIntegerStackChunk;
         FBaseChunk : TSimpleIntegerStackChunk;

         class var vTemplate : TSimpleIntegerStack;

      protected
         procedure Grow;
         procedure Shrink;
         function GetPeek : Integer; inline;
         procedure SetPeek(const item : Integer); inline;

      public
         constructor Create;
         destructor Destroy; override;
         class function Allocate : TSimpleIntegerStack; static;

         procedure Push(const item : Integer); inline;
         procedure Pop; inline;
         procedure Clear;
         property Peek : Integer read GetPeek write SetPeek;
         property Count : Integer read FCount;
   end;

   TSimpleHashBucket<T> = record
      HashCode : Cardinal;
      Value : T;
   end;
   TSimpleHashBucketArray<T> = array of TSimpleHashBucket<T>;
   TSimpleHashAction = (shaNone, shaRemove);
   TSimpleHashFunc<T> = function (const item : T) : TSimpleHashAction of object;

   {: Minimalistic open-addressing hash, subclasses must override SameItem and GetItemHashCode.
      HashCodes *MUST* be non zero }
   TSimpleHash<T> = class
      private
         {$IFDEF DELPHI_XE3}
         // workaround for XE3 compiler bug
         FBuckets : array of TSimpleHashBucket<T>;
         {$ELSE}
         FBuckets : TSimpleHashBucketArray<T>;
         {$ENDIF}
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
         function Remove(const anItem : T) : Boolean; // true if removed
         function Contains(const anItem : T) : Boolean;
         function Match(var anItem : T) : Boolean;
         procedure Enumerate(callBack : TSimpleHashFunc<T>);
         procedure Clear;

         property Count : Integer read FCount;
   end;

   TSimpleObjectHash<T{$IFNDEF FPC}: TRefCountedObject{$ENDIF}> = class(TSimpleHash<T>)
      protected
         function SameItem(const item1, item2 : T) : Boolean; override;
         function GetItemHashCode(const item1 : T) : Integer; override;

      public
         procedure Clean;
   end;

   TNameObjectHashBucket = record
      HashCode : Cardinal;
      Name : UnicodeString;
      Obj : TObject;
   end;
   PNameObjectHashBucket = ^TNameObjectHashBucket;
   TNameObjectHashBuckets = array of TNameObjectHashBucket;

   TNameObjectHash = class
      private
         FBuckets : TNameObjectHashBuckets;
         FCount : Integer;
         FGrowth : Integer;
         FHighIndex : Integer;

      protected
         procedure Grow;
         procedure Resize(newSize : Integer);

         function GetHashedIndex(const aName : UnicodeString; aHash : Cardinal) : Integer;
         function GetIndex(const aName : UnicodeString) : Integer;

         function GetHashedObjects(const aName : UnicodeString; aHash : Cardinal) : TObject; inline;
         function GetObjects(const aName : UnicodeString) : TObject; inline;

         procedure SetHashedObjects(const aName : UnicodeString; aHash : Cardinal; obj : TObject); inline;
         procedure SetObjects(const aName : UnicodeString; obj : TObject); inline;

         function GetBucket(index : Integer) : PNameObjectHashBucket; inline;

         function GetBucketObject(index : Integer) : TObject; inline;
         procedure SetBucketObject(index : Integer; obj : TObject); inline;
         function GetBucketName(index : Integer) : String; inline;

      public
         constructor Create(initialCapacity : Integer = 0);

         class function HashName(const aName : UnicodeString) : Cardinal; static; inline;
         function AddObject(const aName : UnicodeString; aObj : TObject; replace : Boolean = False) : Boolean;
         function AddHashedObject(const aName : UnicodeString; aHash : Cardinal; aObj : TObject; replace : Boolean = False) : Boolean;

         procedure Clean;
         procedure Clear;
         procedure Pack;

         property Objects[const aName : UnicodeString] : TObject read GetObjects write SetObjects; default;
         property HashedObjects[const aName : UnicodeString; aHash : Cardinal] : TObject read GetHashedObjects write SetHashedObjects;

         property Bucket[index : Integer] : PNameObjectHashBucket read GetBucket;
         property BucketObject[index : Integer] : TObject read GetBucketObject write SetBucketObject;
         property BucketName[index : Integer] : String read GetBucketName;
         property BucketIndex[const aName : UnicodeString] : Integer read GetIndex;

         property Count : Integer read FCount;
         property HighIndex : Integer read FHighIndex;
   end;

   TSimpleRefCountedObjectHash = class (TSimpleObjectHash<TRefCountedObject>);

   TSimpleNameObjectHash<T{$IFNDEF FPC}: class{$ENDIF}> = class
      private
         FHash : TNameObjectHash;

      protected
         function GetIndex(const aName : UnicodeString) : Integer; inline;
         function GetObjects(const aName : UnicodeString) : T; inline;
         procedure SetObjects(const aName : UnicodeString; obj : T); inline;
         function GetBucketObject(index : Integer) : T; inline;
         procedure SetBucketObject(index : Integer; obj : T); inline;
         function GetBucketName(index : Integer) : String; inline;

      public
         constructor Create(initialCapacity : Integer = 0);
         destructor Destroy; override;

         function AddObject(const aName : UnicodeString; aObj : T; replace : Boolean = False) : Boolean; inline;

         procedure Clean; inline;
         procedure Clear; inline;
         procedure Pack; inline;

         property Objects[const aName : UnicodeString] : T read GetObjects write SetObjects; default;

         property BucketObject[index : Integer] : T read GetBucketObject write SetBucketObject;
         property BucketName[index : Integer] : String read GetBucketName;
         property BucketIndex[const aName : UnicodeString] : Integer read GetIndex;

         function Count : Integer; inline;
         function HighIndex : Integer; inline;
   end;

   TObjectObjectHashBucket<TKey, TValue{$IFNDEF FPC}: TRefCountedObject{$ENDIF}> = record
      Key : TKey;
      Value : TValue;
   end;

   TSimpleObjectObjectHash<TKey, TValue{$IFNDEF FPC}: TRefCountedObject{$ENDIF}> = class
      type
         TObjectObjectHashBucket = record
            HashCode : Cardinal;
            Name : UnicodeString;
            Key : TKey;
            Value : TValue;
         end;
         TObjectObjectHashBuckets = array of TObjectObjectHashBucket;

      private
         FBuckets : TObjectObjectHashBuckets;
         FCount : Integer;
         FGrowth : Integer;
         FCapacity : Integer;

      protected
         procedure Grow;
         function GetItemHashCode(const item1 : TObjectObjectHashBucket) : Integer;
         function LinearFind(const item : TObjectObjectHashBucket; var index : Integer) : Boolean;
         function Match(var anItem : TObjectObjectHashBucket) : Boolean;
         function Replace(const anItem : TObjectObjectHashBucket) : Boolean; // true if added

      public
         function  GetValue(aKey : TKey) : TValue;
         procedure SetValue(aKey : TKey; aValue : TValue);
         procedure Clear;

         procedure CleanValues;
   end;

   TNameValueHashBucket<T> = record
      Name : String;
      Value : T;
   end;

   TCaseInsensitiveNameValueHash<T> = class (TSimpleHash<TNameValueHashBucket<T>>)
      protected
         function SameItem(const item1, item2 : TNameValueHashBucket<T>) : Boolean; override;
         function GetItemHashCode(const item1 : TNameValueHashBucket<T>) : Integer; override;
   end;

   TObjectsLookup = class (TSortedList<TRefCountedObject>)
      protected
         function Compare(const item1, item2 : TRefCountedObject) : Integer; override;
   end;

   TStringUnifierBucket = record
      Hash : Cardinal;
      Str : UnicodeString;
   end;
   PStringUnifierBucket = ^TStringUnifierBucket;
   TStringUnifierBuckets = array of TStringUnifierBucket;

   TStringUnifier = class
      private
         FLock : TMultiReadSingleWrite;
         FBuckets : TStringUnifierBuckets;
         FCount : Integer;
         FGrowth : Integer;
         FCapacity : Integer;

      protected
         procedure Grow;

      public
         constructor Create;
         destructor Destroy; override;

         // aString must NOT be empty
         procedure UnifyAssign(const aString : UnicodeString; h : Cardinal; var unifiedString : UnicodeString);

         procedure Lock; inline;
         procedure UnLock; inline;

         property Count : Integer read FCount;
         procedure Clear;
   end;

   TThreadCached<T> = class
      private
         FLock : TMultiReadSingleWrite;
         FExpiresAt : Int64;
         FMaxAge : Int64;
         FOnNeedValue : TSimpleCallback<T>;
         FValue : T;

      protected
         function GetValue : T;
         procedure SetValue(const v : T);

      public
         constructor Create(const aNeedValue : TSimpleCallback<T>; maxAgeMSec : Integer);
         destructor Destroy; override;

         procedure Invalidate;

         property Value : T read GetValue write SetValue;
         property MaxAge : Int64 read FMaxAge;
   end;

   // TSimpleQueue<T>
   //
   {: A minimalistic generic queue.
      Based on a linked list with an items pool, supports both FIFO & LIFO. }
   TSimpleQueue<T> = class
      private
      type
         PItemT = ^ItemT;
         ItemT = record
            Prev, Next : PItemT;
            Value : T;
         end;
      var
         FFirst, FLast : PItemT;
         FCount : Integer;
         FPool : PItemT;
         FPoolLeft : Integer;

      protected
         function Alloc : PItemT;
         procedure Release(i : PItemT);

      public
         constructor Create(poolSize : Integer = 8);
         destructor Destroy; override;

         // Adds to the end of the queue
         procedure Push(const v : T);
         // Removes from the end of the queue
         function  Pop(var v : T) : Boolean; overload;
         function  Pop : T; overload;
         // Adds to the beginning of the queue
         procedure Insert(const v : T);
         // Removes from the beginning of the queue
         function  Pull(var v : T) : Boolean; overload;
         function  Pull : T; overload;

         procedure Clear;

         property Count : Integer read FCount;
   end;

const
   cWriteOnlyBlockStreamBlockSize = $2000 - 2*SizeOf(Pointer);
   cNameObjectHashMinSize = 32;

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
         const cCRLF : array [0..1] of WideChar = (#13, #10);
         const cAsciiCRLF : array [0..1] of AnsiChar = (#13, #10);

         function GetSize: Int64; override;

         procedure AllocateCurrentBlock;
         procedure FreeBlocks;

         procedure WriteSpanning(source : PByteArray; count : Integer);
         procedure WriteLarge(source : PByteArray; count : Integer);
         procedure WriteBuf(source : PByteArray; count : Integer);

      public
         constructor Create;
         destructor Destroy; override;

         class function AllocFromPool : TWriteOnlyBlockStream; static;
         procedure ReturnToPool;

         function Seek(Offset: Longint; Origin: Word): Longint; override;
         function Read(var Buffer; Count: Longint): Longint; override;
         function Write(const buffer; count: Longint): Longint; override;

         procedure WriteByte(b : Byte); inline;
         procedure WriteBytes(const b : array of Byte);
         procedure WriteInt32(const i : Integer); inline;
         procedure WriteDWord(const dw : DWORD); inline;

         // must be strictly an utf16 UnicodeString
         procedure WriteString(const utf16String : UnicodeString); overload; inline;
         procedure WriteString(const i : Int32); overload; inline;
         procedure WriteString(const i : Int64); overload; inline;
         procedure WriteSubString(const utf16String : UnicodeString; startPos : Integer); overload;
         procedure WriteSubString(const utf16String : UnicodeString; startPos, aLength : Integer); overload;
         procedure WriteUTF8String(const utf8String : RawByteString); overload; inline;
         procedure WriteCRLF; inline;
         procedure WriteAsciiCRLF; inline;
         procedure WriteChar(utf16Char : WideChar); inline;
         procedure WriteDigits(value : Int64; digits : Integer);

         // assumes data is an utf16 UnicodeString, spits out utf8 in FPC, utf16 in Delphi
         function ToString : String; override;
         function ToUTF8String : RawByteString;
         function ToBytes : TBytes;
         function ToRawBytes : RawByteString;

         procedure Clear;

         procedure StoreData(var buffer); overload;
         procedure StoreData(destStream : TStream); overload;
         procedure StoreUTF8Data(destStream : TStream); overload;
   end;

   IWriteOnlyBlockStream = interface
      function Stream : TWriteOnlyBlockStream;

      procedure WriteString(const utf16String : UnicodeString); overload;
      procedure WriteChar(utf16Char : WideChar);
      procedure WriteCRLF;

      function ToString : String;
   end;

   TAutoWriteOnlyBlockStream = class (TInterfacedSelfObject, IWriteOnlyBlockStream)
      private
         FStream : TWriteOnlyBlockStream;

      protected
         function Stream : TWriteOnlyBlockStream;

         procedure WriteString(const utf16String : UnicodeString); overload;
         procedure WriteChar(utf16Char : WideChar);
         procedure WriteCRLF;

      public
         constructor Create;
         destructor Destroy; override;

         function ToString : String; override;
   end;

   TSimpleInt64List = class(TSimpleList<Int64>)
      protected
         procedure DoExchange(index1, index2 : Integer); inline;
         procedure QuickSort(minIndex, maxIndex : Integer);

      public
         procedure Sort;
   end;

   TSimpleDoubleList = class(TSimpleList<Double>)
      protected
         procedure DoExchange(index1, index2 : Integer); inline;
         procedure QuickSort(minIndex, maxIndex : Integer);

      public
         procedure Sort;
         // Kahan summation
         function Sum : Double;
   end;

   TSimpleStringHash = class(TSimpleHash<String>)
      protected
         function SameItem(const item1, item2 : String) : Boolean; override;
         function GetItemHashCode(const item1 : String) : Integer; override;
   end;

   TFastCompareStringList = class (TStringList)
      {$ifdef FPC}
      function DoCompareText(const s1,s2 : string) : PtrInt; override;
      {$else}
      function CompareStrings(const S1, S2: UnicodeString): Integer; override;
      function IndexOfName(const name : UnicodeString): Integer; override;
      {$endif}
   end;

   TFastCompareTextList = class (TStringList)
      {$ifdef FPC}
      function DoCompareText(const s1,s2 : string) : PtrInt; override;
      {$else}
      function CompareStrings(const S1, S2: UnicodeString): Integer; override;
      function FindName(const name : UnicodeString; var index : Integer) : Boolean;
      function IndexOfName(const name : UnicodeString): Integer; override;
      {$endif}
   end;

   TClassCloneConstructor<T: TRefCountedObject> = record
      private
         FTemplate : T;
         FSize : Integer;
      public
         procedure Initialize(aTemplate : T);
         procedure Finalize;
         function Create : T; inline;
   end;

   ETightListOutOfBound = class(Exception)
   end;

   TQuickSort = record
      public
         CompareMethod : function (index1, index2 : Integer) : Integer of object;
         SwapMethod : procedure (index1, index2 : Integer) of object;

         procedure Sort(minIndex, maxIndex : Integer);
   end;

   TStringIterator = class
      private
         FStr : String;
         FPStr : PChar;
         FPosition : Integer;
         FLength : Integer;

      public
         constructor Create(const s : String);

         function Current : Char; inline;
         function EOF : Boolean; inline;
         procedure Next; inline;
         procedure SkipWhiteSpace;

         function CollectQuotedString : String;
         function CollectAlphaNumeric : String;
         function CollectInteger : Int64;

         property Str : String read FStr;
         property Length : Integer read FLength write FLength;
   end;

   EStringIterator = class (Exception) end;

   EdwsVariantTypeCastError = class(EVariantTypeCastError)
      public
         constructor Create(const v : Variant; const desiredType : UnicodeString;
                            originalException : Exception);
   end;

   PFormatSettings = ^TFormatSettings;

   TPooledObject = class
      private
         FNext : TPooledObject;

      public
         constructor Create; virtual;
   end;
   TPooledObjectClass = class of TPooledObject;

   TPool = record
      private
         FRoot : TPooledObject;
         FPoolClass : TPooledObjectClass;
         FLock : TMultiReadSingleWrite;
         FCount : Integer;
         FCapacity : Integer;

      public
         procedure Initialize(aClass : TPooledObjectClass);
         procedure Finalize;

         procedure Clean(nb : Integer);

         function Acquire : TPooledObject;
         procedure Release(obj : TPooledObject);

         property Count : Integer read FCount;
         property Capacity : Integer read FCapacity write FCapacity;
   end;

const
   cMSecToDateTime : Double = 1/(24*3600*1000);

procedure UnifyAssignString(const fromStr : UnicodeString; var toStr : UnicodeString);
function  UnifiedString(const fromStr : UnicodeString) : UnicodeString; inline;
procedure TidyStringsUnifier;
function  StringUnifierHistogram : TIntegerDynArray;

function UnicodeCompareLen(p1, p2 : PWideChar; n : Integer) : Integer;
function UnicodeCompareText(const s1, s2 : UnicodeString) : Integer;
function UnicodeSameText(const s1, s2 : UnicodeString) : Boolean;
function AsciiCompareLen(p1, p2 : PAnsiChar; n : Integer) : Integer; overload;
function AsciiCompareText(p : PAnsiChar; const s : RawByteString) : Integer;
function AsciiSameText(p : PAnsiChar; const s : RawByteString) : Boolean;

function PosA(const sub, main : RawByteString) : Integer; inline;

function StrIsASCII(const s : String) : Boolean;

function StrNonNilLength(const aString : UnicodeString) : Integer; inline;

function StrIBeginsWith(const aStr, aBegin : UnicodeString) : Boolean;
function StrBeginsWith(const aStr, aBegin : UnicodeString) : Boolean;
function StrBeginsWithA(const aStr, aBegin : RawByteString) : Boolean;
function StrBeginsWithBytes(const aStr : RawByteString; const bytes : array of Byte) : Boolean;
function StrIEndsWith(const aStr, aEnd : UnicodeString) : Boolean;
function StrIEndsWithA(const aStr, aEnd : RawByteString) : Boolean;
function StrEndsWith(const aStr, aEnd : UnicodeString) : Boolean;
function StrEndsWithA(const aStr, aEnd : RawByteString) : Boolean;
function StrContains(const aStr, aSubStr : UnicodeString) : Boolean; overload;
function StrContains(const aStr : UnicodeString; aChar : WideChar) : Boolean; overload;
function LowerCaseA(const aStr : RawByteString) : RawByteString;

function StrMatches(const aStr, aMask : UnicodeString) : Boolean;

function StrDeleteLeft(const aStr : UnicodeString; n : Integer) : UnicodeString;
function StrDeleteRight(const aStr : UnicodeString; n : Integer) : UnicodeString;

function StrAfterChar(const aStr : UnicodeString; aChar : WideChar) : UnicodeString;
function StrBeforeChar(const aStr : UnicodeString; aChar : WideChar) : UnicodeString;

function StrReplaceChar(const aStr : UnicodeString; oldChar, newChar : WideChar) : UnicodeString;

function StrCountChar(const aStr : UnicodeString; c : WideChar) : Integer;

function Min(a, b : Integer) : Integer; inline;

function WhichPowerOfTwo(const v : Int64) : Integer;

function SimpleStringHash(const s : UnicodeString) : Cardinal; inline;
function SimpleLowerCaseStringHash(const s : UnicodeString) : Cardinal;
function SimpleByteHash(p : PByte; n : Integer) : Cardinal;

function SimpleIntegerHash(x : Cardinal) : Cardinal;
function SimpleInt64Hash(x : Int64) : Cardinal;

function RawByteStringToScriptString(const s : RawByteString) : UnicodeString; overload; inline;
procedure RawByteStringToScriptString(const s : RawByteString; var result : UnicodeString); inline; overload;
procedure BytesToScriptString(const p : PByte; n : Integer; var result : UnicodeString);
function ScriptStringToRawByteString(const s : UnicodeString) : RawByteString; overload; inline;
procedure ScriptStringToRawByteString(const s : UnicodeString; var result : RawByteString); overload;

function BinToHex(const data; n : Integer) : String; overload;
function BinToHex(const data : RawByteString) : String; overload; inline;

function HexToBin(const data : String) : RawByteString;

type
   TInt64StringBuffer = array [0..21] of WideChar;
   TInt32StringBuffer = array [0..11] of WideChar;

function FastInt32ToBuffer(const val : Int32; var buf : TInt32StringBuffer) : Integer;
function FastInt64ToBuffer(const val : Int64; var buf : TInt64StringBuffer) : Integer;
procedure FastInt64ToStr(const val : Int64; var s : UnicodeString);
procedure FastInt64ToHex(val : Int64; digits : Integer; var s : UnicodeString);
function Int64ToHex(val : Int64; digits : Integer) : UnicodeString; inline;

function DivMod100(var dividend : Cardinal) : Cardinal;

procedure FastStringReplace(var str : UnicodeString; const sub, newSub : UnicodeString);

procedure VariantToString(const v : Variant; var s : UnicodeString);
procedure VariantToInt64(const v : Variant; var r : Int64);

procedure VarClearSafe(var v : Variant);
procedure VarCopySafe(var dest : Variant; const src : Variant); overload;
procedure VarCopySafe(var dest : Variant; const src : IUnknown); overload;
procedure VarCopySafe(var dest : Variant; const src : IDispatch); overload;
procedure VarCopySafe(var dest : Variant; const src : Int64); overload;
procedure VarCopySafe(var dest : Variant; const src : UnicodeString); overload;
procedure VarCopySafe(var dest : Variant; const src : Double); overload;
procedure VarCopySafe(var dest : Variant; const src : Boolean); overload;

procedure WriteVariant(writer: TWriter; const value: Variant);
function ReadVariant(reader: TReader): Variant;

type
   EISO8601Exception = class (Exception);

function TryISO8601ToDateTime(const v : String; var aResult : TDateTime) : Boolean;
function ISO8601ToDateTime(const v : String) : TDateTime;
function DateTimeToISO8601(dt : TDateTime; extendedFormat : Boolean) : String;

procedure SuppressH2077ValueAssignedToVariableNeverUsed(const X); inline;

procedure dwsFreeAndNil(var O); // transitional function, do not use

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// SuppressH2077ValueAssignedToVariableNeverUsed
//
procedure SuppressH2077ValueAssignedToVariableNeverUsed(const X); inline;
begin
end;

// dwsFreeAndNil
//
procedure dwsFreeAndNil(var o);
var
   obj : TObject;
begin
   obj:=TObject(o);
   Pointer(o):=nil;
   if obj is TRefCountedObject then
      TRefCountedObject(obj).Free
   else obj.Free;
end;

// SimpleStringHash
//
function SimpleStringHash(const s : UnicodeString) : Cardinal; inline;
var
   i : Integer;
begin
   // modified FNV-1a using length as seed
   Result:=Length(s);
   for i:=1 to Result do
      Result:=(Result xor Ord(s[i]))*16777619;
end;

// SimpleLowerCaseStringHash
//
function SimpleLowerCaseStringHash(const s : UnicodeString) : Cardinal;

   function Fallback(const s : UnicodeString) : Cardinal;
   begin
      Result:=SimpleStringHash(UnicodeLowerCase(s));
   end;

var
   i : Integer;
   c : Word;
begin
   // modified FNV-1a using length as seed
   Result:=Length(s);
   for i:=1 to Result do begin
      c:=Ord(s[i]);
      if c>127 then
         Exit(Fallback(s))
      else if c in [Ord('A')..Ord('Z')] then
         c:=c+(Ord('a')-Ord('A'));
      Result:=(Result xor c)*16777619;
   end;
end;

// SimpleByteHash
//
function SimpleByteHash(p : PByte; n : Integer) : Cardinal;
begin
   Result:=2166136261;
   for n:=n downto 1 do begin
      Result:=(Result xor p^)*16777619;
      Inc(p);
   end;
end;

// SimpleIntegerHash
//
function SimpleIntegerHash(x : Cardinal) : Cardinal;
begin
   // simplified MurmurHash 3
   Result := x * $cc9e2d51;
   Result := (Result shl 15) or (Result shr 17);
   Result := Result * $1b873593 + $e6546b64;
end;

// SimpleInt64Hash
//
function SimpleInt64Hash(x : Int64) : Cardinal;
var
   k : Cardinal;
begin
   // simplified MurmurHash 3
   Result := Cardinal(x) * $cc9e2d51;
   Result := (Result shl 15) or (Result shr 17);
   Result := Result * $1b873593 + $e6546b64;

   k := (x shr 32) * $cc9e2d51;
   k := (k shl 15) or (k shr 17);
   Result := k * $1b873593 xor Result;
end;

// ScriptStringToRawByteString
//
function ScriptStringToRawByteString(const s : UnicodeString) : RawByteString;
begin
   ScriptStringToRawByteString(s, Result);
end;

// ScriptStringToRawByteString
//
procedure ScriptStringToRawByteString(const s : UnicodeString; var result : RawByteString); overload;
type
  PByteArray = ^TByteArray;
  TByteArray = array[0..maxInt shr 1] of Byte;
var
   i, n : Integer;
   pSrc : PWideChar;
   pDest : PByteArray;
begin
   n:=Length(s);
   SetLength(Result, n);
   if n=0 then Exit;
   pSrc:=PWideChar(Pointer(s));
   pDest:=PByteArray(NativeUInt(Result));
   for i:=0 to n-1 do
      pDest[i]:=PByte(@pSrc[i])^;
end;

// BinToHex
//
function BinToHex(const data; n : Integer) : String;
const
   cHexDigits : array [0..15] of Char = (
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
      'a', 'b', 'c', 'd', 'e', 'f'
   );
var
   i : Integer;
   pDest : PLongWord;
   p : PByte;
begin
   if n=0 then Exit;

   SetLength(Result, n*2);

   pDest:=Pointer(Result);
   p:=@data;
   for i:=1 to n do begin
      pDest^ := Ord(cHexDigits[p^ shr 4]) + (Ord(cHexDigits[p^ and 15]) shl 16);
      Inc(pDest);
      Inc(p);
   end;
end;

// BinToHex
//
function BinToHex(const data : RawByteString) : String; overload;
begin
   Result:=BinToHex(Pointer(data)^, Length(data));
end;

// HexToBin
//
function HexToBin(const data : String) : RawByteString;
var
   i, n, b : Integer;
   c : Char;
   pSrc : PChar;
   pDest : PByte;
begin
   n:=Length(data);
   if (n and 1)<>0 then
      raise Exception.Create('Even hexadecimal character count expected');

   n:=n shr 1;
   SetLength(Result, n);
   pSrc:=PChar(Pointer(data));
   pDest:=PByte(Result);
   for i:=1 to n do begin
      c:=pSrc[0];
      case c of
         '0'..'9' : b := (Ord(c) shl 4)-(Ord('0') shl 4);
         'A'..'F' : b := (Ord(c) shl 4)+(160-(Ord('A') shl 4));
         'a'..'f' : b := (Ord(c) shl 4)+(160-(Ord('a') shl 4));
      else
         raise Exception.Create('Invalid characters in hexadecimal');
      end;
      c:=pSrc[1];
      case c of
         '0'..'9' : b := b + Ord(c) - Ord('0');
         'A'..'F' : b := b + Ord(c) + (10-Ord('A'));
         'a'..'f' : b := b + Ord(c) + (10-Ord('a'));
      else
         raise Exception.Create('Invalid characters in hexadecimal');
      end;
      pDest^ := b;
      Inc(pDest);
      pSrc := @pSrc[2];
   end;
end;

// DivMod100
//
function DivMod100(var dividend : Cardinal) : Cardinal;
const
   c100 : Cardinal = 100;
{$ifndef WIN32_ASM}
var
   divided : Cardinal;
begin
   divided:=dividend div 100;
   Result:=dividend-divided*100;
   dividend:=divided;
{$else}
asm
   mov   ecx, eax

   mov   eax, [eax]
   xor   edx, edx
   div   c100

   mov   [ecx], eax
   mov   eax, edx
{$endif}
end;

type
   TTwoChars = packed array [0..1] of WideChar;
   PTwoChars = ^TTwoChars;
const
   cTwoDigits : packed array [0..99] of TTwoChars = (
      ('0','0'), ('0','1'), ('0','2'), ('0','3'), ('0','4'), ('0','5'), ('0','6'), ('0','7'), ('0','8'), ('0','9'),
      ('1','0'), ('1','1'), ('1','2'), ('1','3'), ('1','4'), ('1','5'), ('1','6'), ('1','7'), ('1','8'), ('1','9'),
      ('2','0'), ('2','1'), ('2','2'), ('2','3'), ('2','4'), ('2','5'), ('2','6'), ('2','7'), ('2','8'), ('2','9'),
      ('3','0'), ('3','1'), ('3','2'), ('3','3'), ('3','4'), ('3','5'), ('3','6'), ('3','7'), ('3','8'), ('3','9'),
      ('4','0'), ('4','1'), ('4','2'), ('4','3'), ('4','4'), ('4','5'), ('4','6'), ('4','7'), ('4','8'), ('4','9'),
      ('5','0'), ('5','1'), ('5','2'), ('5','3'), ('5','4'), ('5','5'), ('5','6'), ('5','7'), ('5','8'), ('5','9'),
      ('6','0'), ('6','1'), ('6','2'), ('6','3'), ('6','4'), ('6','5'), ('6','6'), ('6','7'), ('6','8'), ('6','9'),
      ('7','0'), ('7','1'), ('7','2'), ('7','3'), ('7','4'), ('7','5'), ('7','6'), ('7','7'), ('7','8'), ('7','9'),
      ('8','0'), ('8','1'), ('8','2'), ('8','3'), ('8','4'), ('8','5'), ('8','6'), ('8','7'), ('8','8'), ('8','9'),
      ('9','0'), ('9','1'), ('9','2'), ('9','3'), ('9','4'), ('9','5'), ('9','6'), ('9','7'), ('9','8'), ('9','9')
      );
function EightDigits(i : Cardinal; p : PWideChar) : Integer;
var
   r : Integer;
begin
   Result:=0;
   Dec(p);
   repeat
      if i<100 then begin
         r:=i;
         i:=0;
      end else begin
         r:=DivMod100(i);
      end;
      if r>=10 then begin
         PTwoChars(p)^:=cTwoDigits[r];
         Dec(p, 2);
         Inc(Result, 2);
      end else begin
         p[1]:=WideChar(Ord('0')+r);
         if i>0 then begin
            p[0]:='0';
            Dec(p, 2);
            Inc(Result, 2);
         end else begin
            Inc(Result);
            Break;
         end;
      end;
   until i=0;
end;

// FastInt32ToBuffer
//
{$IFOPT R+}
  {$DEFINE RANGEON}
  {$R-}
{$ELSE}
  {$UNDEF RANGEON}
{$ENDIF}
function FastInt32ToBuffer(const val : Int32; var buf : TInt32StringBuffer) : Integer;
var
   n, nd : Integer;
   neg : Boolean;
   i, next : UInt32;
begin
   if val<0 then begin
      neg:=True;
//range checking is off here because the code causes range check errors
//code here...
      i:=-val;
   end else begin
      if val=0 then begin
         Result:=High(buf);
         buf[Result]:='0';
         Exit;
      end else i:=val;
      neg:=False;
   end;
   nd:=High(buf);
   n:=nd;
   while True do begin
      if i>100000000 then begin
         next:=i div 100000000;
         n:=n-EightDigits(i-next*100000000, @buf[n]);
         i:=next;
      end else begin
         n:=n-EightDigits(i, @buf[n]);
         Break;
      end;
      Dec(nd, 8);
      while n>nd do begin
         buf[n]:='0';
         Dec(n);
      end;
   end;
   if neg then
      buf[n]:='-'
   else Inc(n);
   Result:=n;
end;
{$IFDEF RANGEON}
  {$R+}
{$ENDIF}

// FastInt64ToBuffer
//
{$IFOPT R+}
  {$DEFINE RANGEON}
  {$R-}
{$ELSE}
  {$UNDEF RANGEON}
{$ENDIF}
function FastInt64ToBuffer(const val : Int64; var buf : TInt64StringBuffer) : Integer;
var
   n, nd : Integer;
   neg : Boolean;
   i, next : UInt64;
begin
   if val<0 then begin
      neg:=True;
//range checking is off here because the code causes range check errors
//code here...
      i:=-val;
   end else begin
      if val=0 then begin
         Result:=High(buf);
         buf[Result]:='0';
         Exit;
      end else i:=val;
      neg:=False;
   end;
   nd:=High(buf);
   n:=nd;
   while True do begin
      if i>100000000 then begin
         next:=i div 100000000;
         n:=n-EightDigits(i-next*100000000, @buf[n]);
         i:=next;
      end else begin
         n:=n-EightDigits(i, @buf[n]);
         Break;
      end;
      Dec(nd, 8);
      while n>nd do begin
         buf[n]:='0';
         Dec(n);
      end;
   end;
   if neg then
      buf[n]:='-'
   else Inc(n);
   Result:=n;
end;
{$IFDEF RANGEON}
  {$R+}
{$ENDIF}

// InitializeSmallIntegers
//
var
   vSmallIntegers : array [0..39] of String;
procedure InitializeSmallIntegers;
var
   i : Integer;
begin
   // we can't use a constant array here, as obtaining a string from a constant
   // array implies a memory allocations, which would defeat the whole purpose
   for i := 0 to High(vSmallIntegers) do
      vSmallIntegers[i] := IntToStr(i);
end;

// FastInt64ToStr
//
procedure FastInt64ToStr(const val : Int64; var s : UnicodeString);
var
   buf : TInt64StringBuffer;
   n : Integer;
begin
   if (Int64Rec(val).Hi=0) and (Int64Rec(val).Lo<=High(vSmallIntegers)) then
      s:=vSmallIntegers[Int64Rec(val).Lo]
   else begin
      n:=FastInt64ToBuffer(val, buf);
      SetString(s, PWideChar(@buf[n]), (High(buf)+1)-n);
   end;
end;

// FastInt64ToHex
//
procedure FastInt64ToHex(val : Int64; digits : Integer; var s : UnicodeString);
const
   cIntToHex : array [0..15] of WideChar = (
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
   );
var
   buf : array [0..15] of WideChar;
   p : PWideChar;
   d, i : Integer;
begin
   if Cardinal(digits)>16 then begin
      if digits<=0 then
         digits:=1
      else digits:=16;
   end;
   p:=@buf[15];
   if PIntegerArray(@val)[1]=0 then begin
      i:=PIntegerArray(@val)[0];
      repeat
         d:=i and 15;
         i:=i shr 4;
         p^:=cIntToHex[d];
         Dec(p);
         Dec(digits);
      until i=0;
   end else begin
      repeat
         d:=val and 15;
         val:=val shr 4;
         p^:=cIntToHex[d];
         Dec(p);
         Dec(digits);
      until val=0;
   end;
   for i:=1 to digits do begin
      p^:='0';
      Dec(p);
   end;
   SetString(s, PWideChar(@p[1]), (NativeUInt(@buf[15])-NativeUInt(p)) div SizeOf(WideChar));
end;

// Int64ToHex
//
function Int64ToHex(val : Int64; digits : Integer) : UnicodeString;
begin
   FastInt64ToHex(val, digits, Result);
end;

// FastStringReplace
//
procedure FastStringReplace(var str : UnicodeString; const sub, newSub : UnicodeString);

   procedure FallBack;
   begin
      str:=SysUtils.StringReplace(str, sub, newSub, [rfReplaceAll]);
   end;

   procedure ReplaceChars(pStr : PWideChar; oldChar, newChar : WideChar; n : Integer);
   begin
      pStr^:=newChar;
      for n:=1 to n do begin
         if pStr[n]=oldChar then
            pStr[n]:=newChar;
      end;
   end;

var
   p, dp, np : Integer;
   subLen, newSubLen : Integer;
   pStr, pNewSub : PWideChar;
begin
   if (str='') or (sub='') then Exit;

   p:=Pos(sub, str);
   if p<=0 then Exit;

   subLen:=Length(sub);
   newSubLen:=Length(newSub);

   pNewSub:=PWideChar(newSub);

   if subLen=newSubLen then begin

      // same length, replace in-place
      UniqueString(str);
      pStr:=PWideChar(Pointer(str));

      if subLen=1 then begin

         // special case of character replacement
         ReplaceChars(@pStr[p-1], sub[1], pNewSub^, Length(str)-p);

      end else begin

         repeat
            System.Move(pNewSub^, pStr[p-1], subLen*SizeOf(WideChar));
            p:=PosEx(sub, str, p+subLen);
         until p<=0;

      end;

   end else if newSubLen<subLen then begin

      // shorter replacement, replace & pack in-place
      UniqueString(str);
      pStr:=PWideChar(Pointer(str));

      dp:=p-1;
      while True do begin
         if newSubLen>0 then begin
            System.Move(pNewSub^, pStr[dp], newSubLen*SizeOf(WideChar));
            dp:=dp+newSubLen;
         end;
         p:=p+subLen;
         np:=PosEx(sub, str, p);
         if np>0 then begin
            if np>p then begin
               System.Move(pStr[p-1], pStr[dp], (np-p)*SizeOf(WideChar));
               dp:=dp+np-p;
            end;
            p:=np;
         end else begin
            np:=Length(str)+1-p;
            if np>0 then
               System.Move(pStr[p-1], pStr[dp], np*SizeOf(WideChar));
            SetLength(str, dp+np);
            Break;
         end;
      end;

   end else begin

      // growth required (not optimized yet, todo)
      FallBack;

   end;
end;

// VariantToString
//
procedure VariantToString(const v : Variant; var s : UnicodeString);

   procedure DispatchAsString(const disp : Pointer; var Result : UnicodeString);
   begin
      Result:=UnicodeFormat('IDispatch (%p)', [disp]);
   end;

   procedure UnknownAsString(const unknown : IUnknown; var Result : UnicodeString);
   var
      intf : IGetSelf;
   begin
      if unknown=nil then
         Result:='nil'
      else if unknown.QueryInterface(IGetSelf, intf)=0 then
         Result:=intf.ToString
      else Result:='[IUnknown]';
   end;

   procedure FloatAsString(var v : Double; var Result : UnicodeString);
   begin
      Result:=FloatToStr(v);
   end;

var
   varData : PVarData;
begin
   varData:=PVarData(@v);
   case varData^.VType of
      {$ifdef FPC}
      varString :
         s:=UnicodeString(varData^.VString);
      {$else}
      varString :
         s:=RawByteStringToScriptString(RawByteString(varData^.VString));
      varUString :
         s:=UnicodeString(varData^.VUString);
      {$endif}
      varInt64 :
         FastInt64ToStr(varData^.VInt64, s);
      varDouble :
         FloatAsString(varData^.VDouble, s);
      varBoolean :
         if varData^.VBoolean then
            s:='True'
         else s:='False';
      varNull :
         s:='Null';
      varUnknown :
         UnknownAsString(IUnknown(varData^.VUnknown), s);
   else
      s:=v;
   end;
end;

// VariantToInt64
//
procedure VariantToInt64(const v : Variant; var r : Int64);

   procedure DefaultCast;
   begin
      try
         r:=v;
      except
         // workaround for RTL bug that will sometimes report a failed cast to Int64
         // as being a failed cast to Boolean
         on E : EVariantError do begin
            raise EdwsVariantTypeCastError.Create(v, 'Integer', E);
         end else raise;
      end;
   end;

begin
   case TVarData(v).VType of
      varInt64 :
         r:=TVarData(v).VInt64;
      varBoolean :
         r:=Ord(TVarData(v).VBoolean);
      varUnknown :
         if TVarData(v).VUnknown=nil then
            r:=0
         else DefaultCast;
   else
      DefaultCast;
   end;
end;

// VarClearSafe
//
procedure VarClearSafe(var v : Variant);
// This procedure exists because of a bug in Variants / Windows where if you clear
// a varUnknown, the _Release method will be called before the variable is niled
// So if _Release's code assigns a nil to the same variable, it will result in
// an invalid refcount. _IntfClear does not suffer from that issue
begin
   case TVarData(v).VType of
      varEmpty : begin
         TVarData(v).VUInt64:=0;
      end;
      varBoolean, varInt64, varDouble : begin
         TVarData(v).VType:=varEmpty;
         TVarData(v).VUInt64:=0;
      end;
      varUnknown : begin
         TVarData(v).VType:=varEmpty;
         IUnknown(TVarData(v).VUnknown):=nil;
      end;
      varDispatch : begin
         TVarData(v).VType:=varEmpty;
         IDispatch(TVarData(v).VDispatch):=nil;
      end;
      varUString : begin
         TVarData(v).VType:=varEmpty;
         UnicodeString(TVarData(v).VString):='';
      end;
   else
      VarClear(v);
      TVarData(v).VUInt64:=0;
   end;
end;

// VarCopySafe (variant)
//
procedure VarCopySafe(var dest : Variant; const src : Variant);
begin
   if @dest=@src then Exit;

   VarClearSafe(dest);

   case TVarData(src).VType of
      varEmpty : ;
      varNull : TVarData(dest).VType:=varNull;
      varBoolean : begin
         TVarData(dest).VType:=varBoolean;
         TVarData(dest).VBoolean:=TVarData(src).VBoolean;
      end;
      varInt64 : begin
         TVarData(dest).VType:=varInt64;
         TVarData(dest).VInt64:=TVarData(src).VInt64;
      end;
      varDouble : begin
         TVarData(dest).VType:=varDouble;
         TVarData(dest).VDouble:=TVarData(src).VDouble;
      end;
      varUnknown : begin
         {$ifdef DEBUG} Assert(TVarData(dest).VUnknown=nil); {$endif}
         TVarData(dest).VType:=varUnknown;
         IUnknown(TVarData(dest).VUnknown):=IUnknown(TVarData(src).VUnknown);
      end;
      varDispatch : begin
         {$ifdef DEBUG} Assert(TVarData(dest).VDispatch=nil); {$endif}
         TVarData(dest).VType:=varDispatch;
         IDispatch(TVarData(dest).VDispatch):=IDispatch(TVarData(src).VDispatch);
      end;
      varUString : begin
         {$ifdef DEBUG} Assert(TVarData(dest).VUString=nil); {$endif}
         TVarData(dest).VType:=varUString;
         UnicodeString(TVarData(dest).VUString):=String(TVarData(src).VUString);
      end;
      varSmallint..varSingle, varCurrency..varDate, varError, varShortInt..varLongWord, varUInt64 : begin
         TVarData(dest).RawData[0]:=TVarData(src).RawData[0];
         TVarData(dest).RawData[1]:=TVarData(src).RawData[1];
         TVarData(dest).RawData[2]:=TVarData(src).RawData[2];
         TVarData(dest).RawData[3]:=TVarData(src).RawData[3];
      end;
   else
      dest:=src;
   end;
end;

// VarCopySafe (iunknown)
//
procedure VarCopySafe(var dest : Variant; const src : IUnknown);
begin
   VarClearSafe(dest);

   TVarData(dest).VType:=varUnknown;
   IUnknown(TVarData(dest).VUnknown):=src;
end;

// VarCopySafe (idispatch)
//
procedure VarCopySafe(var dest : Variant; const src : IDispatch);
begin
   VarClearSafe(dest);

   TVarData(dest).VType:=varDispatch;
   IDispatch(TVarData(dest).VDispatch):=src;
end;

// VarCopySafe (int64)
//
procedure VarCopySafe(var dest : Variant; const src : Int64);
begin
   VarClearSafe(dest);

   TVarData(dest).VType:=varInt64;
   TVarData(dest).VInt64:=src;
end;

// VarCopySafe (string)
//
procedure VarCopySafe(var dest : Variant; const src : UnicodeString);
begin
   VarClearSafe(dest);

   TVarData(dest).VType:=varUString;
   UnicodeString(TVarData(dest).VString):=src;
end;

// VarCopySafe (double)
//
procedure VarCopySafe(var dest : Variant; const src : Double);
begin
   VarClearSafe(dest);

   TVarData(dest).VType:=varDouble;
   TVarData(dest).VDouble:=src;
end;

// VarCopySafe (bool)
//
procedure VarCopySafe(var dest : Variant; const src : Boolean);
begin
   VarClearSafe(dest);

   TVarData(dest).VType:=varBoolean;
   TVarData(dest).VBoolean:=src;
end;

// WriteVariant
//
procedure WriteVariant(writer: TWriter; const value: Variant);

   procedure WriteValue(const value: TValueType);
   begin
      writer.Write(value, SizeOf(value));
   end;

begin
   case VarType(Value) of
      varInt64 :
         writer.WriteInteger(PVarData(@value).VInt64);
      varUString :
         {$ifdef FPC}
         writer.WriteString(UnicodeString(PVarData(@value).VString));
         {$else}
         writer.WriteString(UnicodeString(PVarData(@value).VUString));
         {$endif}
      varDouble :
         writer.WriteFloat(PVarData(@value).VDouble);
      varBoolean :
         writer.WriteBoolean(PVarData(@value).VBoolean);
      varEmpty :
         WriteValue(vaNil);
      varNull :
         WriteValue(vaNull);
      varByte, varSmallInt, varInteger :
         writer.WriteInteger(value);
      varString, varOleStr :
         writer.WriteString(value);
      varSingle :
         writer.WriteSingle(value);
      varCurrency :
         writer.WriteCurrency(value);
      varDate :
         writer.WriteDate(value);
   else
      try
         writer.WriteString(Value);
      except
         raise EWriteError.Create('Streaming not supported');
      end;
   end;
end;

// ReadVariant
//
function ReadVariant(reader: TReader): Variant;

  function ReadValue: TValueType;
  begin
    reader.Read(Result, SizeOf(Result));
  end;

const
   {$ifdef FPC}
   cValTtoVarT: array[TValueType] of Integer = (
      varNull, varError, varByte, varSmallInt, varInteger, varDouble,
      varString, varError, varBoolean, varBoolean, varError, varError, varString,
      varEmpty, varError, varSingle, varCurrency, varDate, varOleStr,
      varUInt64, varString, varDouble{$ifdef FPC}, varQWord{$endif}
    );
   {$else}
   cValTtoVarT: array[TValueType] of Integer = (
      varNull, varError, varByte, varSmallInt, varInteger, varDouble,
      varUString, varError, varBoolean, varBoolean, varError, varError, varUString,
      varEmpty, varError, varSingle, varCurrency, varDate, varOleStr,
      varUInt64, varUString, varDouble{$ifdef FPC}, varQWord{$endif}
    );
   {$endif}

var
  valType: TValueType;
begin
  valType := reader.NextValue;
  case valType of
    vaNil, vaNull:
      begin
        if ReadValue = vaNil then
          VarClearSafe(Result)
        else
          Result := NULL;
      end;
    vaInt8: TVarData(Result).VByte := Byte(reader.ReadInteger);
    vaInt16: TVarData(Result).VSmallint := Smallint(reader.ReadInteger);
    vaInt32: TVarData(Result).VInteger := reader.ReadInteger;
    vaInt64: TVarData(Result).VInt64 := reader.ReadInt64;
    vaExtended: TVarData(Result).VDouble := reader.ReadFloat;
    vaSingle: TVarData(Result).VSingle := reader.ReadSingle;
    vaCurrency: TVarData(Result).VCurrency := reader.ReadCurrency;
    vaDate: TVarData(Result).VDate := reader.ReadDate;
    vaString, vaLString, vaUTF8String:
       Result := UnicodeString(reader.ReadString);
    vaWString: Result := reader.ReadString;
    vaFalse, vaTrue:
       TVarData(Result).VBoolean := (reader.ReadValue = vaTrue);
  else
    raise EReadError.Create('Invalid variant stream');
  end;
  TVarData(Result).VType := cValTtoVarT[ValType];
end;

// DateTimeToISO8601
//
function DateTimeToISO8601(dt : TDateTime; extendedFormat : Boolean) : String;
var
   buf : array [0..31] of Char;
   p : PChar;

   procedure WriteChar(c : Char);
   begin
      p^:=c;
      Inc(p);
   end;

   procedure Write2Digits(v : Integer);
   begin
      PTwoChars(p)^:=cTwoDigits[v];
      Inc(p, 2);
   end;

var
   y, m, d, h, n, s, z : Word;
begin
   p:=@buf;
   DecodeDate(dt, y, m, d);
   Write2Digits((y div 100) mod 100);
   Write2Digits(y mod 100);
   if extendedFormat then
      WriteChar('-');
   Write2Digits(m);
   if extendedFormat then
      WriteChar('-');
   Write2Digits(d);
   DecodeTime(dt, h, n, s, z);

   WriteChar('T');
   Write2Digits(h);
   if extendedFormat then
      WriteChar(':');
   Write2Digits(n);
   if s<>0 then begin
      if extendedFormat then
         WriteChar(':');
      Write2Digits(s);
   end;
   WriteChar('Z');

   p^:=#0;
   Result:=buf;
end;

// ISO8601ToDateTime
//
function ISO8601ToDateTime(const v : String) : TDateTime;
var
   p : PChar;

   function ReadDigit : Integer;
   var
      c : Char;
   begin
      c := p^;
      case c of
         '0'..'9' : Result:=Ord(c)-Ord('0');
      else
         raise EISO8601Exception.CreateFmt('Unexpected character (%d) instead of digit', [Ord(c)]);
      end;
      Inc(p);
   end;

   function Read2Digits : Integer; inline;
   begin
      Result:=ReadDigit*10;
      Result:=Result+ReadDigit;
   end;

   function Read4Digits : Integer; inline;
   begin
      Result:=Read2Digits*100;
      Result:=Result+Read2Digits;
   end;

var
   y, m, d, h, n, s : Integer;
   separator : Boolean;
begin
   if v='' then begin
      Result:=0;
      Exit;
   end;
   p:=Pointer(v);

   // parsing currently limited to basic Z variations with limited validation

   y:=Read4Digits;
   separator:=(p^='-');
   if separator then
      Inc(p);
   m:=Read2Digits;
   if separator then begin
      if p^<>'-' then
         raise EISO8601Exception.Create('"-" expected after month');
      Inc(p);
   end;
   d:=Read2Digits;
   try
      Result:=EncodeDate(y, m, d);
   except
      on E: Exception do
         raise EISO8601Exception.Create(E.Message);
   end;

   if p^=#0 then Exit;
   case p^ of
      'T', ' ' : Inc(p);
   else
      raise EISO8601Exception.Create('"T" expected after date');
   end;

   h:=Read2Digits;
   separator:=(p^=':');
   if separator then
      Inc(p);
   n:=Read2Digits;
   case p^ of
      ':', '0'..'9' : begin
         if (p^=':')<>separator then begin
            if separator then
               raise EISO8601Exception.Create('":" expected after minutes')
            else raise EISO8601Exception.Create('Unexpected ":" after minutes');
         end;
         Inc(p);
         s:=Read2Digits;
      end;
   else
      s:=0;
   end;

   Result:=Result+EncodeTime(h, n, s, 0);

   case p^ of
      #0 : exit;
      'Z' : Inc(p);
   else
      raise EISO8601Exception.Create('Unsupported ISO8601 time zone');
   end;

   if p^<>#0 then
      raise EISO8601Exception.Create('Unsupported or invalid ISO8601 format');
end;

// TryISO8601ToDateTime
//
function TryISO8601ToDateTime(const v : String; var aResult : TDateTime) : Boolean;
begin
   try
      aResult:=ISO8601ToDateTime(v);
      Result:=True;
   except
      on E : EISO8601Exception do
         Result:=False;
      else raise
   end;
end;

// FastCompareFloat
//
function FastCompareFloat(d1, d2 : PDouble) : Integer;
{$ifdef WIN32_ASM}
asm
   fld      qword ptr [edx]
   fld      qword ptr [eax]
   xor      eax, eax
   fcomip   st, st(1)
   setnbe   cl
   setb     al
   and      ecx, 1
   neg      eax
   or       eax, ecx
   fstp     st(0)
{$else}
begin
   if d1^<d2^ then
      Result:=-1
   else Result:=Ord(d1^>d2^);
{$endif}
end;

// RawByteStringToScriptString
//
function RawByteStringToScriptString(const s : RawByteString) : UnicodeString;
begin
   RawByteStringToScriptString(s, Result);
end;

// RawByteStringToScriptString
//
procedure RawByteStringToScriptString(const s : RawByteString; var result : UnicodeString); overload;
begin
   if s='' then begin
      result:='';
      exit;
   end;
   BytesToScriptString(Pointer(s), Length(s), result)
end;

// BytesToScriptString
//
procedure BytesToScriptString(const p : PByte; n : Integer; var result : UnicodeString); overload;
var
   i : Integer;
   pSrc : PByteArray;
   pDest : PWordArray;
begin
   SetLength(result, n);
   pSrc:=PByteArray(p);
   pDest:=PWordArray(Pointer(result));
   for i:=0 to n-1 do
      pDest[i]:=Word(PByte(@pSrc[i])^);
end;

// ------------------
// ------------------ TStringUnifier ------------------
// ------------------

// Create
//
constructor TStringUnifier.Create;
begin
   inherited;
   FLock:=TMultiReadSingleWrite.Create;
end;

// Destroy
//
destructor TStringUnifier.Destroy;
begin
   inherited;
   FLock.Free;
end;

// UnifyAssign
//
procedure TStringUnifier.UnifyAssign(const aString : UnicodeString; h : Cardinal; var unifiedString : UnicodeString);
var
   i : Integer;
   bucket : PStringUnifierBucket;
begin
   if FGrowth=0 then Grow;

   i:=(h and (FCapacity-1));

   repeat
      bucket:=@FBuckets[i];
      if (bucket^.Hash=h) and (bucket^.Str=aString) then begin
         unifiedString:=bucket^.Str;
         Exit;
      end else if bucket^.Hash=0 then begin
         bucket^.Hash:=h;
         bucket^.Str:=aString;
         unifiedString:=aString;
         Inc(FCount);
         Dec(FGrowth);
         Exit;
      end;
      i:=(i+1) and (FCapacity-1);
   until False;
end;

// Lock
//
procedure TStringUnifier.Lock;
begin
   FLock.BeginWrite;
end;

// UnLock
//
procedure TStringUnifier.UnLock;
begin
   FLock.EndWrite;
end;

// Clear
//
procedure TStringUnifier.Clear;
begin
   SetLength(FBuckets, 0);
   FCount:=0;
   FGrowth:=0;
   FCapacity:=0;
end;

// Grow
//
procedure TStringUnifier.Grow;
var
   i, j, n : Integer;
   oldBuckets : TStringUnifierBuckets;
begin
   if FCapacity=0 then
      FCapacity:=32
   else FCapacity:=FCapacity*2;
   FGrowth:=(FCapacity*3) div 4-FCount;

   oldBuckets:=FBuckets;
   FBuckets:=nil;
   SetLength(FBuckets, FCapacity);

   n:=FCapacity-1;
   for i:=0 to High(oldBuckets) do begin
      if oldBuckets[i].Hash=0 then continue;
      j:=(oldBuckets[i].Hash and (FCapacity-1));
      while FBuckets[j].Hash<>0 do
         j:=(j+1) and n;
      FBuckets[j].Hash:=oldBuckets[i].Hash;
      FBuckets[j].Str:=oldBuckets[i].Str;
   end;
end;

// ------------------
// ------------------ UnicodeString Unifier ------------------
// ------------------

type
   {$IFNDEF FPC}
   {$IF CompilerVersion > 22}
   TStringListList = TStringItemList;
   {$ELSE}
   TStringListList = PStringItemList;
   {$IFEND}
   {$ELSE}
   TStringListList = PStringItemList;
   {$ENDIF}

   {$ifndef FPC}
   TStringListCracker = class (TStrings)
      private
         FList : TStringListList;
   end;
   {$endif}

var
   vUnifiedStrings : array [0..63] of TStringUnifier;

// CompareStrings
//
{$ifdef FPC}
function TFastCompareStringList.DoCompareText(const S1, S2: String): Integer;
begin
   Result:=CompareStr(S1, S2);
end;
{$else}
function TFastCompareStringList.CompareStrings(const S1, S2: UnicodeString): Integer;
begin
   Result:=CompareStr(S1, S2);
end;

// IndexOfName
//
function TFastCompareStringList.IndexOfName(const name : UnicodeString): Integer;
var
   n, nc : Integer;
   nvs : WideChar;
   list : TStringListList;
begin
   nvs:=NameValueSeparator;
   n:=Length(name);
   list:=TStringListCracker(Self).FList;
   for Result:=0 to Count-1 do begin
      nc:=Length(list[Result].FString);
      if     (nc>n) and (list[Result].FString[n+1]=nvs)
         and CompareMem(PWideChar(Pointer(name)),
                        PWideChar(Pointer(list[Result].FString)), n) then Exit;
   end;
   Result:=-1;
end;
{$endif}

// InitializeStringsUnifier
//
procedure InitializeStringsUnifier;
var
   i : Integer;
begin
   for i:=Low(vUnifiedStrings) to High(vUnifiedStrings) do
      vUnifiedStrings[i]:=TStringUnifier.Create;
end;

// FinalizeStringsUnifier
//
procedure FinalizeStringsUnifier;
var
   i : Integer;
begin
   for i:=Low(vUnifiedStrings) to High(vUnifiedStrings) do begin
      vUnifiedStrings[i].Free;
      vUnifiedStrings[i]:=nil;
   end;
end;

// UnifyAssignString
//
procedure UnifyAssignString(const fromStr : UnicodeString; var toStr : UnicodeString);
var
   i : Integer;
   su : TStringUnifier;
   h : Cardinal;
begin
   if fromStr='' then
      toStr:=''
   else begin
      h:=SimpleStringHash(fromStr);
      i:=h and High(vUnifiedStrings);
      su:=vUnifiedStrings[i];
      su.Lock;
      su.UnifyAssign(fromStr, h shr 6, toStr);
      su.UnLock;
   end;
end;

// UnifiedString
//
function UnifiedString(const fromStr : UnicodeString) : UnicodeString;
begin
   UnifyAssignString(fromStr, Result);
end;

// TidyStringsUnifier
//
procedure TidyStringsUnifier;
var
   i : Integer;
   su : TStringUnifier;
begin
   for i:=Low(vUnifiedStrings) to High(vUnifiedStrings) do begin
      su:=vUnifiedStrings[i];
      su.Lock;
      su.Clear;
      su.UnLock;
   end;
end;

// StringUnifierHistogram
//
function StringUnifierHistogram : TIntegerDynArray;
var
   i : Integer;
begin
   SetLength(Result, Length(vUnifiedStrings));
   for i:=Low(vUnifiedStrings) to High(vUnifiedStrings) do
      Result[i-Low(vUnifiedStrings)]:=vUnifiedStrings[i].Count;
end;

// UnicodeCompareLen
//
function UnicodeCompareLen(p1, p2 : PWideChar; n : Integer) : Integer;
var
   c1, c2 : Integer;
begin
   for n:=n downto 1 do begin
      c1:=Ord(p1^);
      c2:=Ord(p2^);
      if (c1<>c2) then begin
         if (c1<=127) and (c2<=127) then begin
            if c1 in [Ord('a')..Ord('z')] then
               c1:=c1+(Ord('A')-Ord('a'));
            if c2 in [Ord('a')..Ord('z')] then
               c2:=c2+(Ord('A')-Ord('a'));
            if c1<>c2 then begin
               Result:=c1-c2;
               Exit;
            end;
         end else begin
            Result:=UnicodeComparePChars(p1, p2, n);
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
{$R-}
function UnicodeCompareText(const s1, s2 : UnicodeString) : Integer;
var
   n1, n2 : Integer;
   ps1, ps2 : PWideChar;
begin
   ps1:=PWideChar(NativeInt(s1));
   ps2:=PWideChar(NativeInt(s2));
   if ps1<>nil then begin
      if ps2<>nil then begin
         n1:=PInteger(NativeUInt(ps1)-4)^;
         n2:=PInteger(NativeUInt(ps2)-4)^;
         if n1<n2 then begin
            Result:=UnicodeCompareLen(ps1, ps2, n1);
            if Result=0 then
               Result:=-1;
         end else begin
            Result:=UnicodeCompareLen(ps1, ps2, n2);
            if (Result=0) and (n1>n2) then
               Result:=1;
         end;
      end else Result:=1;
   end else if ps2<>nil then
      Result:=-1
   else Result:=0;
end;

// UnicodeSameText
//
function UnicodeSameText(const s1, s2 : UnicodeString) : Boolean;
begin
   Result:=(Length(s1)=Length(s2)) and (UnicodeCompareText(s1, s2)=0)
end;

// AsciiCompareLen
//
function AsciiCompareLen(p1, p2 : PAnsiChar; n : Integer) : Integer;
var
   c1, c2 : Integer;
begin
   for n:=n downto 1 do begin
      c1:=Ord(p1^);
      c2:=Ord(p2^);
      if (c1<>c2) then begin
         if c1 in [Ord('a')..Ord('z')] then
            c1:=c1+(Ord('A')-Ord('a'));
         if c2 in [Ord('a')..Ord('z')] then
            c2:=c2+(Ord('A')-Ord('a'));
         if c1<>c2 then begin
            Result:=c1-c2;
            Exit;
         end;
      end;
      Inc(p1);
      Inc(p2);
   end;
   Result:=0;
end;

// AsciiCompareText
//
function AsciiCompareText(p : PAnsiChar; const s : RawByteString) : Integer;
var
   n : Integer;
begin
   n:=Length(s);
   if n>0 then
      Result:=AsciiCompareLen(p, Pointer(s), n)
   else Result:=0;
   if Result=0 then
      if p[n]<>#0 then
         Result:=1;
end;

// AsciiSameText
//
function AsciiSameText(p : PAnsiChar; const s : RawByteString) : Boolean;
begin
   Result:=(AsciiCompareText(p, s)=0);
end;

// PosA
//
function PosA(const sub, main : RawByteString) : Integer; inline;
begin
   Result:=Pos(sub, main);
end;

// StrIsASCII
//
function StrIsASCII(const s : String) : Boolean;
var
   i : Integer;
begin
   for i:=1 to Length(s)-1 do begin
      case s[i] of
         #0..#127 :;
      else
         Exit(False);
      end;
   end;
   Result:=True;
end;

// StrNonNilLength
//
function StrNonNilLength(const aString : UnicodeString) : Integer;
begin
   Result:=PInteger(NativeUInt(Pointer(aString))-4)^;
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
   else Result:=CompareMem(Pointer(aStr), Pointer(aBegin), n2*SizeOf(WideChar));
end;

// StrBeginsWithA
//
function StrBeginsWithA(const aStr, aBegin : RawByteString) : Boolean;
var
   n1, n2 : Integer;
begin
   n1:=Length(aStr);
   n2:=Length(aBegin);
   if (n2>n1) or (n2=0) then
      Result:=False
   else Result:=CompareMem(Pointer(aStr), Pointer(aBegin), n2);
end;

// StrBeginsWithBytes
//
function StrBeginsWithBytes(const aStr : RawByteString; const bytes : array of Byte) : Boolean;
var
   i, n : Integer;
begin
   n:=Length(bytes);
   if Length(aStr)<n then
      Result:=False
   else begin
      for i:=0 to n-1 do
         if Ord(PAnsiChar(Pointer(aStr))[i])<>bytes[i] then Exit(False);
      Result:=True;
   end;
end;

// StrIEndsWith
//
function StrIEndsWith(const aStr, aEnd : UnicodeString) : Boolean;
var
   n1, n2 : Integer;
begin
   n1:=Length(aStr);
   n2:=Length(aEnd);
   if (n2>n1) or (n2=0) then
      Result:=False
   else Result:=(UnicodeCompareLen(@aStr[n1-n2+1], Pointer(aEnd), n2)=0);
end;

// StrIEndsWithA
//
function StrIEndsWithA(const aStr, aEnd : RawByteString) : Boolean;
var
   n1, n2 : Integer;
begin
   n1:=Length(aStr);
   n2:=Length(aEnd);
   if (n2>n1) or (n2=0) then
      Result:=False
   else Result:=(AsciiCompareLen(@aStr[n1-n2+1], Pointer(aEnd), n2)=0);
end;

// StrEndsWith
//
function StrEndsWith(const aStr, aEnd : UnicodeString) : Boolean;
var
   n1, n2 : Integer;
begin
   n1:=Length(aStr);
   n2:=Length(aEnd);
   if (n2>n1) or (n2=0) then
      Result:=False
   else Result:=CompareMem(@aStr[n1-n2+1], Pointer(aEnd), n2*SizeOf(WideChar));
end;

// StrEndsWithA
//
function StrEndsWithA(const aStr, aEnd : RawByteString) : Boolean;
var
   n1, n2 : Integer;
begin
   n1:=Length(aStr);
   n2:=Length(aEnd);
   if (n2>n1) or (n2=0) then
      Result:=False
   else Result:=CompareMem(@aStr[n1-n2+1], Pointer(aEnd), n2);
end;

// StrContains (sub string)
//
function StrContains(const aStr, aSubStr : UnicodeString) : Boolean;
begin
   if aSubStr='' then
      Result:=True
   else if StrNonNilLength(aSubStr)=1 then
      Result:=StrContains(aStr, aSubStr[1])
   else Result:=(Pos(aSubStr, aStr)>0);
end;

// LowerCaseA
//
function LowerCaseA(const aStr : RawByteString) : RawByteString;
var
   i, n : Integer;
   dst, src : PByte;
   c : Byte;
begin
   n:=Length(aStr);
   SetLength(Result, n);
   if n<=0 then Exit;

   dst:=Pointer(Result);
   src:=Pointer(aStr);
   for i:=1 to n do begin
      c:=src^;
      if c in [Ord('A')..Ord('Z')] then
         c:=c or $20;
      dst^:=c;
      Inc(src);
      Inc(dst);
   end;
end;

// StrMatches
//
function StrMatches(const aStr, aMask : UnicodeString) : Boolean;
var
   mask : TMask;
begin
   mask:=TMask.Create(aMask);
   try
      Result:=mask.Matches(aStr);
   finally
      mask.Free;
   end;
end;

// StrContains (sub char)
//
function StrContains(const aStr : UnicodeString; aChar : WideChar) : Boolean;
var
   i : Integer;
begin
   for i:=0 to Length(aStr)-1 do
      if aStr[i+1]=aChar then Exit(True);
   Result:=False;
end;

// StrDeleteLeft
//
function StrDeleteLeft(const aStr : UnicodeString; n : Integer) : UnicodeString;
begin
   Result:=Copy(aStr, n+1);
end;

// StrDeleteRight
//
function StrDeleteRight(const aStr : UnicodeString; n : Integer) : UnicodeString;
begin
   Result:=Copy(aStr, 1, Length(aStr)-n);
end;

// StrAfterChar
//
function StrAfterChar(const aStr : UnicodeString; aChar : WideChar) : UnicodeString;
var
   p : Integer;
begin
   p:=Pos(aChar, aStr);
   if p>0 then
      Result:=Copy(aStr, p+1)
   else Result:='';
end;

// StrBeforeChar
//
function StrBeforeChar(const aStr : UnicodeString; aChar : WideChar) : UnicodeString;
var
   p : Integer;
begin
   p:=Pos(aChar, aStr);
   if p>0 then
      Result:=Copy(aStr, 1, p-1)
   else Result:=aStr;
end;

// StrReplaceChar
//
function StrReplaceChar(const aStr : UnicodeString; oldChar, newChar : WideChar) : UnicodeString;
var
   i : Integer;
begin
   Result:=aStr;
   for i:=1 to Length(Result) do
      if Result[i]=oldChar then
         Result[i]:=newChar;
end;

// StrCountChar
//
function StrCountChar(const aStr : UnicodeString; c : WideChar) : Integer;
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

// WhichPowerOfTwo
//
function WhichPowerOfTwo(const v : Int64) : Integer;
var
   n : Int64;
begin
   if (v>0) and ((v and (v-1))=0) then begin
      for Result:=0 to 63 do begin
         n:=Int64(1) shl Result;
         if n>v then Break;
         if n=v then Exit;
      end;
   end;
   Result:=-1;
end;

// ------------------
// ------------------ TFastCompareTextList ------------------
// ------------------

// CompareStrings
//
{$ifdef FPC}
function TFastCompareTextList.DoCompareText(const S1, S2: String): Integer;
begin
   Result:=UnicodeCompareText(s1, s2);
end;
{$else}
function TFastCompareTextList.CompareStrings(const S1, S2: UnicodeString): Integer;
begin
   Result:=UnicodeCompareText(s1, s2);
end;

// FindName
//
function TFastCompareTextList.FindName(const name : UnicodeString; var index : Integer) : Boolean;
var
   lo, hi, mid, cmp, n, nc : Integer;
   initial : UnicodeString;
   list : TStringListList;
begin
   Result:=False;
   list:=TStringListCracker(Self).FList;
   initial:=Name+NameValueSeparator;
   n:=Length(initial);
   lo:=0;
   hi:=Count-1;
   while lo<=hi do begin
      mid:=(lo+hi) shr 1;
      nc:=Length(list[mid].FString);
      if nc>=n then begin
         cmp:=UnicodeCompareLen(PWideChar(Pointer(list[mid].FString)), PWideChar(Pointer(initial)), n);
      end else begin
         cmp:=UnicodeCompareLen(PWideChar(Pointer(list[mid].FString)), PWideChar(Pointer(initial)), nc);
         if cmp=0 then
            cmp:=-1;
      end;
      if cmp<0 then
         lo:=mid+1
      else begin
         hi:=mid-1;
         if cmp=0 then begin
            Result:=True;
            if Duplicates<>dupAccept then
               lo:=mid;
         end;
      end;
   end;
   index:=lo;
end;

// IndexOfName
//
function TFastCompareTextList.IndexOfName(const name : UnicodeString): Integer;
var
   n, nc : Integer;
   nvs : WideChar;
   list : TStringListList;
begin
   if not Sorted then begin
      nvs:=NameValueSeparator;
      n:=Length(name);
      list:=TStringListCracker(Self).FList;
      for Result:=0 to Count-1 do begin
         nc:=Length(list[Result].FString);
         if     (nc>n) and (list[Result].FString[n+1]=nvs)
            and (UnicodeCompareLen(PWideChar(Pointer(name)),
                                   PWideChar(Pointer(list[Result].FString)), n)=0) then Exit;
      end;
      Result:=-1;
   end else begin
      if not FindName(name, Result) then
         Result:=-1;
   end;
end;
{$endif}

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
procedure TTightList.MoveItem(curIndex, newIndex : Integer);
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

// ItemsAllOfClass
//
function TTightList.ItemsAllOfClass(aClass : TClass) : Boolean;
var
   i : Integer;
begin
   for i:=0 to FCount-1 do
      if List[i].ClassType<>aClass then Exit(False);
   Result:=True;
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
   Assert(Cardinal(index)<Cardinal(FCount), 'Index out of range');
   Result:=FItems[index];
end;

// SetItem
//
procedure TObjectList<T>.SetItem(index : Integer; const item : T);
begin
   Assert(Cardinal(index)<Cardinal(FCount), 'Index out of range');
   FItems[index]:=item;
end;

// Add
//
function TObjectList<T>.Add(const anItem : T) : Integer;
begin
   if Count=Length(FItems) then
      SetLength(FItems, Count+8+(Count shr 4));
   FItems[FCount]:=anItem;
   Result:=FCount;
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
var
   n : Integer;
begin
   Assert(Cardinal(idx)<Cardinal(FCount), 'Index out of range');
   n:=Count-1-idx;
   Dec(FCount);
   if n>0 then
      System.Move(FItems[idx+1], FItems[idx], SizeOf(T)*n);
   Result:=FItems[idx];
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
begin
   if Find(anItem, Result) then
      ExtractAt(Result)
   else Result:=-1;
end;

// ExtractAt
//
function TSortedList<T>.ExtractAt(index : Integer) : T;
var
   n : Integer;
begin
   Dec(FCount);
   Result:=FItems[index];
   n:=FCount-index;
   if n>0 then
      System.Move(FItems[index+1], FItems[index], n*SizeOf(T));
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

var
   vWOBSPool : Pointer;

// AllocFromPool
//
class function TWriteOnlyBlockStream.AllocFromPool : TWriteOnlyBlockStream;
begin
   Result:=InterlockedExchangePointer(vWOBSPool, nil);
   if Result=nil then
      Result:=TWriteOnlyBlockStream.Create;
end;

// ReturnToPool
//
procedure TWriteOnlyBlockStream.ReturnToPool;
var
   wobs : TWriteOnlyBlockStream;
begin
   if Self=nil then Exit;
   Clear;
   if vWOBSPool=nil then begin
      wobs:=InterlockedExchangePointer(vWOBSPool, Self);
      if wobs<>nil then
         wobs.Destroy;
   end else Destroy;
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
   GetMem(newBlock, cWriteOnlyBlockStreamBlockSize+2*SizeOf(Pointer));
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
         System.Move(iterator[2], dest^, n);
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

// StoreUTF8Data
//
procedure TWriteOnlyBlockStream.StoreUTF8Data(destStream : TStream);
var
   buf : UTF8String;
begin
   buf:=UTF8Encode(ToString);
   if buf<>'' then
      destStream.Write(buf[1], Length(buf));
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

// WriteSpanning
//
procedure TWriteOnlyBlockStream.WriteSpanning(source : PByteArray; count : Integer);
begin
   // current block contains some data, write fraction, allocate new block
   System.Move(source^, PByteArray(@FCurrentBlock[2])[FBlockRemaining^], count);
   FBlockRemaining^:=cWriteOnlyBlockStreamBlockSize;

   AllocateCurrentBlock;
end;

// WriteLarge
//
procedure TWriteOnlyBlockStream.WriteLarge(source : PByteArray; count : Integer);
var
   newBlock : PPointerArray;
begin
   // large amount still to be written, insert specific block
   newBlock:=GetMemory(count+2*SizeOf(Pointer));
   newBlock[0]:=FCurrentBlock;
   PInteger(@newBlock[1])^:=count;
   System.Move(source^, newBlock[2], count);
   FCurrentBlock[0]:=newBlock;
   FCurrentBlock:=newBlock;
   AllocateCurrentBlock;
end;

// WriteBuf
//
procedure TWriteOnlyBlockStream.WriteBuf(source : PByteArray; count : Integer);
type
   TThreeBytes = packed array [1..3] of Byte;
   PThreeBytes = ^TThreeBytes;
   TFiveBytes = packed array [1..5] of Byte;
   PFiveBytes = ^TFiveBytes;
   TSixBytes = packed array [1..6] of Byte;
   PSixBytes = ^TSixBytes;
   TSevenBytes = packed array [1..7] of Byte;
   PSevenBytes = ^TSevenBytes;
var
   dest : PByteArray;
   fraction : Integer;
begin
   Inc(FTotalSize, count);

   fraction:=cWriteOnlyBlockStreamBlockSize-FBlockRemaining^;
   if count>fraction then begin
      // does not fit in current block
      // was current block started?
      if FBlockRemaining^>0 then begin
         WriteSpanning(source, fraction);
         Dec(count, fraction);
         source:=@source[fraction];
      end;
      if count>cWriteOnlyBlockStreamBlockSize div 2 then begin
         WriteLarge(source, count);
         Exit;
      end;
   end;

   // if we reach here, everything fits in current block
   dest:=@PByteArray(@FCurrentBlock[2])[FBlockRemaining^];
   Inc(FBlockRemaining^, count);
   case Cardinal(count) of
      0 : ;
      1 : dest[0]:=source[0];
      2 : PWord(dest)^:=PWord(source)^;
      3 : PThreeBytes(dest)^:=PThreeBytes(source)^;
      4 : PCardinal(dest)^:=PCardinal(source)^;
      5 : PFiveBytes(dest)^:=PFiveBytes(source)^;
      6 : PSixBytes(dest)^:=PSixBytes(source)^;
      7 : PSevenBytes(dest)^:=PSevenBytes(source)^;
      8 : PInt64(dest)^:=PInt64(source)^;
   else
      System.Move(source^, dest^, count);
   end;
end;

// Write
//
function TWriteOnlyBlockStream.Write(const buffer; count: Longint): Longint;
begin
   WriteBuf(@buffer, count);
   Result:=count;
end;

// WriteByte
//
procedure TWriteOnlyBlockStream.WriteByte(b : Byte);
begin
   WriteBuf(@b, 1);
end;

// WriteBytes
//
procedure TWriteOnlyBlockStream.WriteBytes(const b : array of Byte);
var
   n : Integer;
begin
   n:=Length(b);
   if n>0 then
      WriteBuf(@b[0], Length(b));
end;

// WriteInt32
//
procedure TWriteOnlyBlockStream.WriteInt32(const i : Integer);
begin
   WriteBuf(@i, 4);
end;

// WriteDWord
//
procedure TWriteOnlyBlockStream.WriteDWord(const dw : DWORD);
begin
   WriteBuf(@dw, 4);
end;

// WriteString
//
procedure TWriteOnlyBlockStream.WriteString(const utf16String : UnicodeString);
var
   stringCracker : NativeUInt;
begin
   {$ifdef FPC}
   if utf16String<>'' then
      WriteBuf(utf16String[1], Length(utf16String)*SizeOf(WideChar));
   {$else}
   stringCracker:=NativeUInt(utf16String);
   if stringCracker<>0 then
      WriteBuf(Pointer(stringCracker), PInteger(stringCracker-SizeOf(Integer))^*SizeOf(WideChar));
   {$endif}
end;

// WriteString (Int32)
//
procedure TWriteOnlyBlockStream.WriteString(const i : Int32);
var
   buf : TInt32StringBuffer;
   n : Integer;
begin
   n:=FastInt32ToBuffer(i, buf);
   WriteBuf(@buf[n], (High(buf)+1-n)*SizeOf(WideChar));
end;

// WriteString (Int64)
//
procedure TWriteOnlyBlockStream.WriteString(const i : Int64);
var
   buf : TInt64StringBuffer;
   n : Integer;
begin
   n:=FastInt64ToBuffer(i, buf);
   WriteBuf(@buf[n], (High(buf)+1-n)*SizeOf(WideChar));
end;

// WriteChar
//
procedure TWriteOnlyBlockStream.WriteChar(utf16Char : WideChar);
begin
   WriteBuf(@utf16Char, SizeOf(WideChar));
end;

// WriteDigits
//
procedure TWriteOnlyBlockStream.WriteDigits(value : Int64; digits : Integer);
var
   buf : array [0..19] of WideChar;
   n : Integer;
begin
   if digits<=0 then Exit;

   Assert(digits<Length(buf));
   n:=Length(buf);
   while digits>0 do begin
      Dec(n);
      if value<>0 then begin
         buf[n]:=WideChar(Ord('0')+(value mod 10));
         value:=value div 10;
      end else buf[n]:='0';
      Dec(digits);
   end;

   WriteBuf(@buf[n], (Length(buf)-n)*SizeOf(WideChar));
end;

// ToString
//
function TWriteOnlyBlockStream.ToString : String;
{$ifdef FPC}
var
   uniBuf : UnicodeString;
begin
   if FTotalSize>0 then begin

      Assert((FTotalSize and 1) = 0);
      SetLength(uniBuf, FTotalSize div SizeOf(WideChar));
      StoreData(uniBuf[1]);
      Result:=UTF8Encode(uniBuf);

   end else Result:='';
{$else}
begin
   if FTotalSize>0 then begin

      Assert((FTotalSize and 1) = 0);
      SetLength(Result, FTotalSize div SizeOf(WideChar));
      StoreData(Result[1]);

   end else Result:='';
   {$endif}
end;

// ToUTF8String
//
function TWriteOnlyBlockStream.ToUTF8String : RawByteString;
begin
   Result:=UTF8Encode(ToString);
end;

// ToBytes
//
function TWriteOnlyBlockStream.ToBytes : TBytes;
var
   s : Int64;
begin
   s:=Size;
   SetLength(Result, s);
   if s>0 then
      StoreData(Result[0]);
end;

// ToRawBytes
//
function TWriteOnlyBlockStream.ToRawBytes : RawByteString;
var
   s : Int64;
begin
   s:=Size;
   SetLength(Result, s);
   if s>0 then
      StoreData(Result[1]);
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
procedure TWriteOnlyBlockStream.WriteSubString(const utf16String : UnicodeString;
                                               startPos, aLength : Integer);
var
   p, n : Integer;
begin
   Assert(startPos>=1);

   if aLength<=0 then Exit;
   p:=startPos+aLength-1;

   n:=System.Length(utf16String);
   if startPos>n then Exit;

   if p>n then
      n:=n-startPos+1
   else n:=p-startPos+1;

   if n>0 then
      WriteBuf(@utf16String[startPos], n*SizeOf(WideChar));
end;

// WriteUTF8String
//
procedure TWriteOnlyBlockStream.WriteUTF8String(const utf8String : RawByteString);
begin
   WriteBuf(Pointer(utf8String), Length(utf8String));
end;

// WriteCRLF
//
procedure TWriteOnlyBlockStream.WriteCRLF;
begin
   WriteBuf(@cCRLF[0], 2*SizeOf(WideChar));
end;

// WriteAsciiCRLF
//
procedure TWriteOnlyBlockStream.WriteAsciiCRLF;
begin
   WriteBuf(@cAsciiCRLF[0], 2*SizeOf(AnsiChar));
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
   if FCount>0 then
      Result:=FList[FCount-1]
   else Result:=nil;
end;

// Peek
//
function TTightStack.Peek(n : Integer) : TRefCountedObject;
begin
   if n>=0 then
      n:=FCount-n-1;
   if n<0 then
      Result:=nil
   else Result:=FList[n];
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
   {$IFDEF DELPHI_XE3}
   oldBuckets : array of TSimpleHashBucket<T>;
   {$ELSE}
   oldBuckets : TSimpleHashBucketArray<T>;
   {$ENDIF}
begin
   if FCapacity=0 then
      FCapacity:=32
   else FCapacity:=FCapacity*2;
   FGrowth:=(FCapacity*11) div 16;

   {$IFDEF DELPHI_XE3}
   SetLength(oldBuckets, Length(FBuckets));
   for i := 0 to Length(FBuckets) - 1 do
     oldBuckets[i] := FBuckets[i];
   {$ELSE}
   oldBuckets:=FBuckets;
   {$ENDIF}

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
         Exit(False)
      else if SameItem(item, FBuckets[index].Value) then
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

// Remove
//
function TSimpleHash<T>.Remove(const anItem : T) : Boolean;
var
   i : Integer;
   hashCode : Integer;
begin
   if FCount>=FGrowth then Grow;

   hashCode:=GetItemHashCode(anItem);
   i:=(hashCode and (FCapacity-1));
   if LinearFind(anItem, i) then begin
      FBuckets[i].HashCode:=0;
      FBuckets[i].Value:=Default(T);
      Dec(FCount);
      Result:=True;
   end else begin
      Result:=False;
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
procedure TSimpleHash<T>.Enumerate(callBack : TSimpleHashFunc<T>);
var
   i : Integer;
begin
   if FCount=0 then Exit;
   for i:=0 to High(FBuckets) do begin
      if FBuckets[i].HashCode<>0 then begin
         if callBack(FBuckets[i].Value)=shaRemove then begin
            FBuckets[i].HashCode:=0;
            FBuckets[i].Value:=Default(T);
            Dec(FCount);
         end;
      end;
   end;
end;

// Clear
//
procedure TSimpleHash<T>.Clear;
begin
   FCount:=0;
   FCapacity:=0;
   FGrowth:=0;
   FBuckets:=nil;
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
var
   n : Integer;
begin
   FItems[idx]:=Default(T);
   n:=FCount-idx-1;
   if n>0 then begin
      Move(FItems[idx+1], FItems[idx], n*SizeOf(T));
      FillChar(FItems[FCount-1], SizeOf(T), 0);
   end;
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
function TInterfacedSelfObject.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult;
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
// ------------------ TAutoStrings ------------------
// ------------------

// Create
//
constructor TAutoStrings.Create;
begin
   FValue:=TStringList.Create;
end;

// Destroy
//
destructor TAutoStrings.Destroy;
begin
   FValue.Free;
end;

// CreateCapture
//
constructor TAutoStrings.CreateCapture(value : TStringList);
begin
   FValue:=value;
end;

// CreateClone
//
constructor TAutoStrings.CreateClone(value : TStringList);
var
   sl : TStringList;
begin
   sl:=TStringList.Create;
   FValue:=sl;
   sl.Assign(value);
   sl.CaseSensitive:=value.CaseSensitive;
   sl.Sorted:=value.Sorted;
end;

// GetValue
//
function TAutoStrings.GetValue : TStringList;
begin
   Result:=FValue;
end;

// Clone
//
function TAutoStrings.Clone : IAutoStrings;
begin
   Result:=TAutoStrings.CreateClone(FValue);
end;

// ------------------
// ------------------ TNameObjectHash<T> ------------------
// ------------------

// Create
//
constructor TNameObjectHash.Create(initialCapacity : Integer = 0);
var
   n : Integer;
begin
   if initialCapacity>0 then begin
      // find nearest power of two >= initialCapacity
      n:=cNameObjectHashMinSize;
      while n<initialCapacity do
         n:=n shl 1;
      initialCapacity:=n;
      SetLength(FBuckets, n);
   end;
   FHighIndex:=initialCapacity-1;
end;

// HashName
//
class function TNameObjectHash.HashName(const aName : UnicodeString) : Cardinal;
begin
   Result:=SimpleStringHash(aName);
   if Result=0 then
      Result:=1;
end;

// Grow
//
procedure TNameObjectHash.Grow;
begin
   if FHighIndex<cNameObjectHashMinSize-1 then
      Resize(cNameObjectHashMinSize)
   else Resize(FHighIndex*2+2);
end;

// Pack
//
procedure TNameObjectHash.Pack;
var
   i, m : Integer;
begin
   case FCount of
      0 : Clear;
      1..cNameObjectHashMinSize shr 1 : Resize(cNameObjectHashMinSize);
   else
      i:=FHighIndex+1;
      m:=FCount*3; // = 2 * FCount*(3/2)
      while i>=m do
         i:=i shr 1;
      Resize(i);
   end;
end;

// Resize
//
procedure TNameObjectHash.Resize(newSize : Integer);

   procedure MoveBucket(var src, dest : TNameObjectHashBucket); inline;
   begin
      dest.HashCode:=src.HashCode;
      Pointer(dest.Name):=Pointer(src.Name);
      Pointer(src.Name):=nil;
      dest.Obj:=src.Obj;
   end;

var
   i, j, n : Integer;
   mask : Integer;
   oldBuckets : TNameObjectHashBuckets;
   oldBucket : PNameObjectHashBucket;
begin
   Assert(newSize>FCount);  // protect from infinite loop

   oldBuckets:=FBuckets;
   FBuckets:=nil;
   SetLength(FBuckets, newSize);
   FHighIndex:=newSize-1;
   FGrowth:=(FHighIndex*3) div 4;

   if FCount=0 then Exit;

   n:=0;
   mask:=newSize-1;
   oldBucket:=@oldBuckets[0];
   for i:=0 to High(oldBuckets) do begin
      if oldBucket.HashCode<>0 then begin
         Inc(n);
         j:=(oldBucket.HashCode and mask);
         while FBuckets[j].HashCode<>0 do
            j:=(j+1) and mask;
         MoveBucket(oldBucket^, FBuckets[j]);
      end;
      Inc(oldBucket);
   end;
   FCount:=n;
end;

// GetHashedIndex
//
function TNameObjectHash.GetHashedIndex(const aName : UnicodeString; aHash : Cardinal) : Integer;
var
   i : Integer;
begin
   if FCount=0 then Exit(-1);

   i:=aHash and FHighIndex;

   repeat
      with FBuckets[i] do begin
         if HashCode=0 then
            Exit(-1)
         else if (HashCode=aHash) and (Name=aName) then
            Exit(i);
      end;
      i:=(i+1) and FHighIndex;
   until False;
end;

// GetIndex
//
function TNameObjectHash.GetIndex(const aName : UnicodeString) : Integer;
begin
   Result:=GetHashedIndex(aName, HashName(aName));
end;

// GetHashedObjects
//
function TNameObjectHash.GetHashedObjects(const aName : UnicodeString; aHash : Cardinal) : TObject;
var
   i : Integer;
begin
   i:=GetHashedIndex(aName, aHash);
   if i<0 then
      Result:=nil
   else Result:=FBuckets[i].Obj
end;

// GetObjects
//
function TNameObjectHash.GetObjects(const aName : UnicodeString) : TObject;
begin
   Result:=GetHashedObjects(aName, HashName(aName));
end;

// SetHashedObjects
//
procedure TNameObjectHash.SetHashedObjects(const aName : UnicodeString; aHash : Cardinal; obj : TObject);
begin
   AddHashedObject(aName, HashName(aName), obj, True);
end;

// SetObjects
//
procedure TNameObjectHash.SetObjects(const aName : UnicodeString; obj : TObject);
begin
   AddObject(aName, obj, True);
end;

// AddObject
//
function TNameObjectHash.AddObject(const aName : UnicodeString; aObj : TObject;
                                            replace : Boolean = False) : Boolean;
begin
   Result:=AddHashedObject(aName, HashName(aName), aObj, replace);
end;

// AddHashedObject
//
function TNameObjectHash.AddHashedObject(const aName : UnicodeString; aHash : Cardinal; aObj : TObject; replace : Boolean = False) : Boolean;
var
   i : Integer;
begin
   if FCount>=FGrowth then Grow;

   i:=aHash and FHighIndex;

   repeat
      with FBuckets[i] do begin
         if HashCode=0 then
            Break
         else if (HashCode=aHash) and (Name=aName) then begin
            if replace then
               Obj:=aObj;
            Exit(False);
         end;
      end;
      i:=(i+1) and FHighIndex;
   until False;

   with FBuckets[i] do begin
      HashCode:=aHash;
      Name:=aName;
      Obj:=aObj;
   end;
   Inc(FCount);
   Result:=True;
end;

// Clean
//
procedure TNameObjectHash.Clean;
var
   i : Integer;
begin
   for i:=0 to FHighIndex do begin
      if FBuckets[i].HashCode<>0 then
         dwsFreeAndNil(FBuckets[i].Obj);
   end;
   Clear;
end;

// Clear
//
procedure TNameObjectHash.Clear;
begin
   SetLength(FBuckets, 0);
   FGrowth:=0;
   FCount:=0;
   FHighIndex:=-1;
end;

// GetBucket
//
function TNameObjectHash.GetBucket(index : Integer) : PNameObjectHashBucket;
begin
   Result:=@FBuckets[index];
end;

// GetBucketName
//
function TNameObjectHash.GetBucketName(index : Integer) : String;
begin
   Result:=FBuckets[index].Name;
end;

// GetBucketObject
//
function TNameObjectHash.GetBucketObject(index : Integer) : TObject;
begin
   Result:=FBuckets[index].Obj;
end;

// SetBucketObject
//
procedure TNameObjectHash.SetBucketObject(index : Integer; obj : TObject);
begin
   FBuckets[index].Obj:=obj;
end;

// ------------------
// ------------------ TSimpleNameObjectHash<T> ------------------
// ------------------

// Create
//
constructor TSimpleNameObjectHash<T>.Create(initialCapacity : Integer = 0);
begin
   FHash:=TNameObjectHash.Create(initialCapacity);
end;

// Destroy
//
destructor TSimpleNameObjectHash<T>.Destroy;
begin
   FHash.Free;
end;

// Pack
//
procedure TSimpleNameObjectHash<T>.Pack;
begin
   FHash.Pack;
end;

// GetIndex
//
function TSimpleNameObjectHash<T>.GetIndex(const aName : UnicodeString) : Integer;
begin
   Result:=FHash.GetIndex(aName);
end;

// GetObjects
//
function TSimpleNameObjectHash<T>.GetObjects(const aName : UnicodeString) : T;
begin
   Result:=T(FHash.GetObjects(aName));
end;

// SetObjects
//
procedure TSimpleNameObjectHash<T>.SetObjects(const aName : UnicodeString; obj : T);
begin
   FHash.SetObjects(aName, obj);
end;

// AddObject
//
function TSimpleNameObjectHash<T>.AddObject(const aName : UnicodeString; aObj : T;
                                            replace : Boolean = False) : Boolean;
begin
   Result:=FHash.AddObject(aName, aObj, replace);
end;

// Clean
//
procedure TSimpleNameObjectHash<T>.Clean;
begin
   FHash.Clean;
end;

// Clear
//
procedure TSimpleNameObjectHash<T>.Clear;
begin
   FHash.Clear;
end;

// GetBucketName
//
function TSimpleNameObjectHash<T>.GetBucketName(index : Integer) : String;
begin
   Result:=FHash.GetBucketName(index);
end;

// GetBucketObject
//
function TSimpleNameObjectHash<T>.GetBucketObject(index : Integer) : T;
begin
   Result:=T(FHash.GetBucketObject(index));
end;

// SetBucketObject
//
procedure TSimpleNameObjectHash<T>.SetBucketObject(index : Integer; obj : T);
begin
   FHash.SetBucketObject(index, obj);
end;

// Count
//
function TSimpleNameObjectHash<T>.Count : Integer;
begin
   Result:=FHash.Count;
end;

// HighIndex
//
function TSimpleNameObjectHash<T>.HighIndex : Integer;
begin
   Result:=FHash.HighIndex;
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
   {$ifdef FPC}
   p:=@FRefCount;
   {$else}
   p:=PInteger(NativeInt(Self)+InstanceSize-hfFieldSize+hfMonitorOffset);
   {$endif}
   Result:=InterlockedIncrement(p^);
end;

// DecRefCount
//
function TRefCountedObject.DecRefCount : Integer;
var
   p : PInteger;
begin
   {$ifdef FPC}
   p:=@FRefCount;
   {$else}
   p:=PInteger(NativeInt(Self)+InstanceSize-hfFieldSize+hfMonitorOffset);
   {$endif}
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
   {$ifdef FPC}
   p:=@FRefCount;
   {$else}
   p:=PInteger(NativeInt(Self)+InstanceSize-hfFieldSize+hfMonitorOffset);
   {$endif}
   Result:=p^;
end;

// SetRefCount
//
procedure TRefCountedObject.SetRefCount(n : Integer);
var
   p : PInteger;
begin
   {$ifdef FPC}
   p:=@FRefCount;
   {$else}
   p:=PInteger(NativeInt(Self)+InstanceSize-hfFieldSize+hfMonitorOffset);
   {$endif}
   p^:=n;
end;

// ------------------
// ------------------ TSimpleObjectObjectHash<T1, T2> ------------------
// ------------------

// Grow
//
procedure TSimpleObjectObjectHash<TKey, TValue>.Grow;
var
   i, j, n : Integer;
   hashCode : Integer;
   oldBuckets : TObjectObjectHashBuckets;
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

// GetItemHashCode
//
function TSimpleObjectObjectHash<TKey, TValue>.GetItemHashCode(const item1 : TObjectObjectHashBucket) : Integer;
begin
   Result:=(PNativeInt(@item1.Key)^ shr 2);
end;

// GetValue
//
function TSimpleObjectObjectHash<TKey, TValue>.GetValue(aKey : TKey) : TValue;
var
   bucket : TObjectObjectHashBucket;
begin
   bucket.Key:=aKey;
   if Match(bucket) then
      Result:=bucket.Value
   {$ifdef VER200}
   else Result:=default(TValue); // D2009 support
   {$else}
   else Result:=TValue(TObject(nil));  // workaround for D2010 compiler bug
   {$endif}
end;

// SetValue
//
procedure TSimpleObjectObjectHash<TKey, TValue>.SetValue(aKey : TKey; aValue : TValue);
var
   bucket : TObjectObjectHashBucket;
begin
   bucket.Key:=aKey;
   bucket.Value:=aValue;
   Replace(bucket);
end;

// LinearFind
//
function TSimpleObjectObjectHash<TKey, TValue>.LinearFind(const item : TObjectObjectHashBucket; var index : Integer) : Boolean;
begin
   repeat
      if FBuckets[index].HashCode=0 then
         Exit(False)
      else if item.Key=FBuckets[index].Key then
         Exit(True);
      index:=(index+1) and (FCapacity-1);
   until False;
end;

// Match
//
function TSimpleObjectObjectHash<TKey, TValue>.Match(var anItem : TObjectObjectHashBucket) : Boolean;
var
   i : Integer;
begin
   if FCount=0 then Exit(False);
   i:=(GetItemHashCode(anItem) and (FCapacity-1));
   Result:=LinearFind(anItem, i);
   if Result then
      anItem:=FBuckets[i];
end;

// Replace
//
function TSimpleObjectObjectHash<TKey, TValue>.Replace(const anItem : TObjectObjectHashBucket) : Boolean;
var
   i : Integer;
   hashCode : Integer;
begin
   if FCount>=FGrowth then Grow;

   hashCode:=GetItemHashCode(anItem);
   i:=(hashCode and (FCapacity-1));
   if LinearFind(anItem, i) then begin
      FBuckets[i]:=anItem
   end else begin
      FBuckets[i]:=anItem;
      FBuckets[i].HashCode:=hashCode;
      Inc(FCount);
      Result:=True;
   end;
end;

// CleanValues
//
procedure TSimpleObjectObjectHash<TKey, TValue>.CleanValues;
var
   i : Integer;
begin
   for i:=0 to FCapacity-1 do begin
      if FBuckets[i].HashCode<>0 then
         FBuckets[i].Value.Free;
   end;
   Clear;
end;

// Clear
//
procedure TSimpleObjectObjectHash<TKey, TValue>.Clear;
begin
   FCount:=0;
   FCapacity:=0;
   FGrowth:=0;
   SetLength(FBuckets, 0);
end;

// ------------------
// ------------------ TThreadCached<T> ------------------
// ------------------

// Create
//
constructor TThreadCached<T>.Create(const aNeedValue : TSimpleCallback<T>; maxAgeMSec : Integer);
begin
   FLock:=TMultiReadSingleWrite.Create;
   FOnNeedValue:=aNeedValue;
   FMaxAge:=maxAgeMSec;
end;

// Destroy
//
destructor TThreadCached<T>.Destroy;
begin
   FLock.Free;
end;

// Invalidate
//
procedure TThreadCached<T>.Invalidate;
begin
   FExpiresAt:=0;
end;

// GetValue
//
function TThreadCached<T>.GetValue : T;
var
   ts : Int64;
begin
   ts:=GetSystemMilliseconds;
   if ts>=FExpiresAt then begin
      FLock.BeginWrite;
      try
         if FOnNeedValue(FValue)=csContinue then
            FExpiresAt:=ts+FMaxAge;
         Result:=FValue;
      finally
         FLock.EndWrite;
      end;
   end else begin
      FLock.BeginRead;
      try
         Result:=FValue;
      finally
         FLock.EndRead;
      end;
   end;
end;

// SetValue
//
procedure TThreadCached<T>.SetValue(const v : T);
begin
   FLock.BeginWrite;
   try
      FExpiresAt:=GetSystemMilliseconds+FMaxAge;
      FValue:=v;
   finally
      FLock.EndWrite;
   end;
end;

// ------------------
// ------------------ TSimpleIntegerStack ------------------
// ------------------

// Create
//
constructor TSimpleIntegerStack.Create;
begin
   FChunk:=@FBaseChunk;
   FChunkIndex:=-1;
end;

// Destroy
//
destructor TSimpleIntegerStack.Destroy;
begin
   Clear;
end;

// Allocate
//
class function TSimpleIntegerStack.Allocate : TSimpleIntegerStack;
var
   p : Pointer;
   n : Integer;
begin
   n:=InstanceSize;
   GetMem(p, n);
   Move(Pointer(vTemplate)^, p^, n);
   Result:=TSimpleIntegerStack(p);
   Result.FChunk:=@Result.FBaseChunk;
end;

// Push
//
procedure TSimpleIntegerStack.Push(const item : Integer);
begin
   if FChunkIndex<TSimpleIntegerStackChunk.ChunkSize-1 then
      Inc(FChunkIndex)
   else Grow;
   FChunk.Data[FChunkIndex]:=item;
   Inc(FCount);
end;

// Pop
//
procedure TSimpleIntegerStack.Pop;
begin
   if FChunkIndex>0 then
      Dec(FChunkIndex)
   else Shrink;
   Dec(FCount);
end;

// Clear
//
procedure TSimpleIntegerStack.Clear;
var
   p : PSimpleIntegerStackChunk;
begin
   if FPooledChunk<>nil then
      FreeMem(FPooledChunk);
   FPooledChunk:=nil;
   while FChunk<>@FBaseChunk do begin
      p:=FChunk;
      FChunk:=p.Prev;
      FreeMem(p);
   end;
end;

// Grow
//
procedure TSimpleIntegerStack.Grow;
var
   p : PSimpleIntegerStackChunk;
begin
   if FPooledChunk<>nil then begin
      p:=FPooledChunk;
      FPooledChunk:=nil;
   end else GetMem(p, SizeOf(TSimpleIntegerStackChunk));
   p.Prev:=FChunk;
   FChunk:=p;
   FChunkIndex:=0;
end;

// Shrink
//
procedure TSimpleIntegerStack.Shrink;
begin
   if FChunk.Prev=nil then
      Dec(FChunkIndex)
   else begin
      FreeMem(FPooledChunk);
      FPooledChunk:=FChunk;
      FChunk:=FChunk.Prev;
      FChunkIndex:=TSimpleIntegerStackChunk.ChunkSize-1;
   end;
end;

// GetPeek
//
function TSimpleIntegerStack.GetPeek : Integer;
begin
   Result:=FChunk.Data[FChunkIndex];
end;

// SetPeek
//
procedure TSimpleIntegerStack.SetPeek(const item : Integer);
begin
   FChunk.Data[FChunkIndex]:=item;
end;

// ------------------
// ------------------ TClassCloneConstructor<T> ------------------
// ------------------

// Initialize
//
procedure TClassCloneConstructor<T>.Initialize(aTemplate : T);
begin
   FTemplate:=aTemplate;
   FSize:= FTemplate.InstanceSize;
end;

// Finalize
//
procedure TClassCloneConstructor<T>.Finalize;
begin
   FTemplate.Free;
   TObject(FTemplate):=nil; // D2010 bug workaround
end;

// Create
//
function TClassCloneConstructor<T>.Create : T;
begin
  GetMemForT(Result, FSize);
  Move(TtoPointer(FTemplate)^, TtoPointer(Result)^, FSize);
end;

// ------------------
// ------------------ TQuickSort ------------------
// ------------------

// Sort
//
procedure TQuickSort.Sort(minIndex, maxIndex : Integer);
var
   i, j, p, n : Integer;
begin
   n:=maxIndex-minIndex;
   case n of
      1 : begin
         if CompareMethod(minIndex, maxIndex)>0 then
            SwapMethod(minIndex, maxIndex);
      end;
      2 : begin
         i:=minIndex+1;
         if CompareMethod(minIndex,i)>0 then
            SwapMethod(minIndex, i);
         if CompareMethod(i, maxIndex)>0 then begin
            SwapMethod(i, maxIndex);
            if CompareMethod(minIndex, i)>0 then
               SwapMethod(minIndex, i);
         end;
      end;
   else
      if n<=0 then Exit;
      repeat
         i:=minIndex;
         j:=maxIndex;
         p:=((i+j) shr 1);
         repeat
            while CompareMethod(i, p)<0 do Inc(i);
            while CompareMethod(j, p)>0 do Dec(j);
            if i<=j then begin
               SwapMethod(i, j);
               if p=i then
                  p:=j
               else if p=j then
                  p:=i;
               Inc(i);
               Dec(j);
            end;
         until i>j;
         if minIndex<j then
            Sort(minIndex, j);
         minIndex:=i;
      until i>=maxIndex;
   end;
end;

// ------------------
// ------------------ TSimpleQueue<T> ------------------
// ------------------

// Create
//
constructor TSimpleQueue<T>.Create(poolSize: Integer);
begin
   FPoolLeft:=poolSize;
end;

// Destroy
//
destructor TSimpleQueue<T>.Destroy;
var
   next : PItemT;
begin
   Clear;
   while FPool<>nil do begin
      next:=FPool.Next;
      FreeMem(FPool);
      FPool:=next;
   end;
   inherited;
end;

// Alloc
//
function TSimpleQueue<T>.Alloc: PItemT;
begin
   if FPool=nil then
      Result:=AllocMem(SizeOf(ItemT))
   else begin
      Result:=FPool;
      FPool:=Result.Next;
      Result.Next:=nil;
      Inc(FPoolLeft);
   end;
   Inc(FCount);
end;

// Release
//
procedure TSimpleQueue<T>.Release(i: PItemT);
begin
   i.Value:=Default(T);
   if FPoolLeft>0 then begin
      Dec(FPoolLeft);
      i.Prev:=nil;
      i.Next:=FPool;
      FPool:=i;
   end else FreeMem(i);
   Dec(FCount);
end;

// Push
//
procedure TSimpleQueue<T>.Push(const v: T);
var
   p : PItemT;
begin
   p:=Alloc;
   p.Value:=v;
   if FLast<>nil then begin
      p.Prev:=FLast;
      FLast.Next:=p;
   end else FFirst:=p;
   FLast:=p;
end;

// Pop
//
function TSimpleQueue<T>.Pop(var v: T) : Boolean;
var
   p : PItemT;
begin
   if FCount=0 then Exit(False);

   p:=FLast;
   FLast:=p.Prev;
   v:=p.Value;
   Release(p);
   if FLast<>nil then
      FLast.Next:=nil
   else FFirst:=FLast;
   Result:=True;
end;

// Pop
//
function TSimpleQueue<T>.Pop : T;
begin
   Assert(Count>0);
   Pop(Result);
end;

// Insert
//
procedure TSimpleQueue<T>.Insert(const v: T);
var
   p : PItemT;
begin
   p:=Alloc;
   p.Value:=v;
   if FFirst<>nil then begin
      p.Next:=FFirst;
      FFirst.Prev:=p;
   end else FLast:=p;
   FFirst:=p;
end;

// Pull
//
function TSimpleQueue<T>.Pull(var v: T) : Boolean;
var
   p : PItemT;
begin
   if FCount=0 then Exit(False);

   p:=FFirst;
   FFirst:=p.Next;
   v:=p.Value;
   Release(p);
   if FFirst<>nil then
      FFirst.Prev:=nil
   else FLast:=FFirst;
   Result:=True;
end;

// Pull
//
function TSimpleQueue<T>.Pull : T;
begin
   Assert(Count>0);
   Pull(Result);
end;

// Clear
//
procedure TSimpleQueue<T>.Clear;
var
   p, pNext : PItemT;
begin
   p:=FFirst;
   while p<>nil do begin
      pNext:=p.Next;
      Release(p);
      p:=pNext;
   end;
   FFirst:=nil;
   FLast:=nil;
end;

// ------------------
// ------------------ EdwsVariantTypeCastError ------------------
// ------------------

// Create
//
constructor EdwsVariantTypeCastError.Create(const v : Variant;
      const desiredType : UnicodeString; originalException : Exception);
begin
   inherited CreateFmt(RTE_VariantCastFailed,
                       [VarTypeAsText(VarType(v)), desiredType, originalException.ClassName])

end;

// ------------------
// ------------------ TAutoWriteOnlyBlockStream ------------------
// ------------------

// Create
//
constructor TAutoWriteOnlyBlockStream.Create;
begin
   FStream:=TWriteOnlyBlockStream.AllocFromPool;
end;

// Destroy
//
destructor TAutoWriteOnlyBlockStream.Destroy;
begin
   FStream.ReturnToPool;
end;

// Stream
//
function TAutoWriteOnlyBlockStream.Stream : TWriteOnlyBlockStream;
begin
   Result:=FStream;
end;

// WriteString
//
procedure TAutoWriteOnlyBlockStream.WriteString(const utf16String : UnicodeString);
begin
   FStream.WriteString(utf16String);
end;

// WriteChar
//
procedure TAutoWriteOnlyBlockStream.WriteChar(utf16Char : WideChar);
begin
   FStream.WriteChar(utf16Char);
end;

// WriteCRLF
//
procedure TAutoWriteOnlyBlockStream.WriteCRLF;
begin
   FStream.WriteCRLF;
end;

// ToString
//
function TAutoWriteOnlyBlockStream.ToString : String;
begin
   Result:=FStream.ToString;
end;

// ------------------
// ------------------ TStringIterator ------------------
// ------------------

// Create
//
constructor TStringIterator.Create(const s : String);
begin
   FStr:=s;
   FPStr:=PChar(Pointer(s));
   FLength:=System.Length(s);
   FPosition:=0;
end;

// Current
//
function TStringIterator.Current : Char;
begin
   if Cardinal(FPosition)<Cardinal(FLength) then
      Result:=FPstr[FPosition]
   else Result:=#0;
end;

// EOF
//
function TStringIterator.EOF : Boolean;
begin
   Result:=(FPosition>=FLength);
end;

// Next
//
procedure TStringIterator.Next;
begin
   Inc(FPosition);
end;

// SkipWhiteSpace
//
procedure TStringIterator.SkipWhiteSpace;
begin
   while (FPosition<FLength) and (Ord(FPStr[FPosition])<=Ord(' ')) do
      Inc(FPosition);
end;

// CollectQuotedString
//
function TStringIterator.CollectQuotedString : String;
var
   quoteChar : Char;
begin
   quoteChar:=Current;
   Inc(FPosition);
   while not EOF do begin
      if FPstr[FPosition]=quoteChar then begin
         Inc(FPosition);
         if EOF or (FPstr[FPosition]<>quoteChar) then Exit;
         Result:=Result+quoteChar;
      end else begin
         Result:=Result+FPstr[FPosition];
         Inc(FPosition);
      end;
   end;
   raise EStringIterator.Create('Unfinished quoted string');
end;

// CollectAlphaNumeric
//
function TStringIterator.CollectAlphaNumeric : String;
var
   start : Integer;
begin
   start:=FPosition;
   while FPosition<FLength do begin
      case FPstr[FPosition] of
         '0'..'9', 'a'..'z', 'A'..'Z' : Inc(FPosition);
      else
         break;
      end;
   end;
   Result:=Copy(FStr, start+1, FPosition-start);
end;

// CollectInteger
//
function TStringIterator.CollectInteger : Int64;
var
   neg : Boolean;
begin
   if (FPosition<FLength) and (FPStr[FPosition]='-') then begin
      neg:=True;
      Inc(FPosition);
   end else neg:=False;
   if FPosition>=FLength then
      EStringIterator.Create('Unfinished integer');
   Result:=0;
   while FPosition<FLength do begin
      case FPstr[FPosition] of
         '0'..'9' : begin
            Result:=Result*10+Ord(FPstr[FPosition])-Ord('0');
            Inc(FPosition);
         end;
      else
         break;
      end;
   end;
   if neg then
      Result:=-Result;
end;

// ------------------
// ------------------ TCaseInsensitiveNameValueHash<T> ------------------
// ------------------

// SameItem
//
function TCaseInsensitiveNameValueHash<T>.SameItem(const item1, item2 : TNameValueHashBucket<T>) : Boolean;
begin
   Result:=UnicodeSameText(item1.Name, item2.Name);
end;

// GetItemHashCode
//
function TCaseInsensitiveNameValueHash<T>.GetItemHashCode(const item1 : TNameValueHashBucket<T>) : Integer;
begin
   Result:=SimpleLowerCaseStringHash(item1.Name);
end;

// ------------------
// ------------------ TSimpleStringHash ------------------
// ------------------

// SameItem
//
function TSimpleStringHash.SameItem(const item1, item2 : String) : Boolean;
begin
   Result:=UnicodeSameText(item1, item2);
end;

// GetItemHashCode
//
function TSimpleStringHash.GetItemHashCode(const item1 : String) : Integer;
begin
   Result:=SimpleLowerCaseStringHash(item1);
end;

// ------------------
// ------------------ TSimpleInt64List ------------------
// ------------------

// DoExchange
//
procedure TSimpleInt64List.DoExchange(index1, index2 : Integer);
var
   t : Int64;
begin
   t:=FItems[index1];
   FItems[index1]:=FItems[index2];
   FItems[index2]:=t;
end;

// QuickSort
//
procedure TSimpleInt64List.QuickSort(minIndex, maxIndex : Integer);
var
   i, j, p, n : Integer;
begin
   n:=maxIndex-minIndex;
   case n of
      1 : begin
         if FItems[minIndex]>FItems[maxIndex] then
            DoExchange(minIndex, maxIndex);
      end;
      2 : begin
         i:=minIndex+1;
         if FItems[minIndex]>FItems[i] then
            DoExchange(minIndex, i);
         if FItems[i]>FItems[maxIndex] then begin
            DoExchange(i, maxIndex);
            if FItems[minIndex]>FItems[i] then
               DoExchange(minIndex, i);
         end;
      end;
   else
      if n<=0 then Exit;
      repeat
         i:=minIndex;
         j:=maxIndex;
         p:=((i+j) shr 1);
         repeat
            while FItems[i]<FItems[p] do Inc(i);
            while Fitems[j]>FItems[p] do Dec(j);
            if i<=j then begin
               DoExchange(i, j);
               if p=i then
                  p:=j
               else if p=j then
                  p:=i;
               Inc(i);
               Dec(j);
            end;
         until i>j;
         if minIndex<j then
            QuickSort(minIndex, j);
         minIndex:=i;
      until i>=maxIndex;
   end;
end;

// Sort
//
procedure TSimpleInt64List.Sort;
begin
   QuickSort(0, Count-1);
end;

// ------------------
// ------------------ TSimpleDoubleList ------------------
// ------------------

// Sort
//
procedure TSimpleDoubleList.Sort;
begin
   QuickSort(0, Count-1);
end;

// Sum
//
function TSimpleDoubleList.Sum : Double;
var
   c, y, t : Double;
   i : Integer;
begin
   if Count=0 then Exit(0);
   Result:=FItems[0];
   c:=0;
   for i:=1 to Count-1 do begin
      y:=FItems[i]-c;
      t:=Result+y;
      c:=(t-Result)-y;
      Result:=t;
   end;
end;

// DoExchange
//
procedure TSimpleDoubleList.DoExchange(index1, index2 : Integer);
var
   buf : Double;
begin
   buf:=FItems[index1];
   FItems[index1]:=FItems[index2];
   FItems[index2]:=buf;
end;

// QuickSort
//
procedure TSimpleDoubleList.QuickSort(minIndex, maxIndex : Integer);
var
   i, j, p, n : Integer;
begin
   n:=maxIndex-minIndex;
   case n of
      1 : begin
         if FItems[minIndex]>FItems[maxIndex] then
            DoExchange(minIndex, maxIndex);
      end;
      2 : begin
         i:=minIndex+1;
         if FItems[minIndex]>FItems[i] then
            DoExchange(minIndex, i);
         if FItems[i]>FItems[maxIndex] then begin
            DoExchange(i, maxIndex);
            if FItems[minIndex]>FItems[i] then
               DoExchange(minIndex, i);
         end;
      end;
   else
      if n<=0 then Exit;
      repeat
         i:=minIndex;
         j:=maxIndex;
         p:=((i+j) shr 1);
         repeat
            while FItems[i]<FItems[p] do Inc(i);
            while Fitems[j]>FItems[p] do Dec(j);
            if i<=j then begin
               DoExchange(i, j);
               if p=i then
                  p:=j
               else if p=j then
                  p:=i;
               Inc(i);
               Dec(j);
            end;
         until i>j;
         if minIndex<j then
            QuickSort(minIndex, j);
         minIndex:=i;
      until i>=maxIndex;
   end;
end;

// ------------------
// ------------------ TPooledObject ------------------
// ------------------

// Create
//
constructor TPooledObject.Create;
begin
   // nothing, just to introduce virtual construction
end;

// ------------------
// ------------------ TPool ------------------
// ------------------

// Initialize
//
procedure TPool.Initialize(aClass : TPooledObjectClass);
begin
   FRoot:=nil;
   FPoolClass:=aClass;
   FLock:=TMultiReadSingleWrite.Create;
   FCount:=0;
   FCapacity:=16*1024;
end;

// Finalize
//
procedure TPool.Finalize;
begin
   Clean(0);
   FLock.Free;
   FLock:=nil;
   FPoolClass:=nil;
   FCount:=0;
   FCapacity:=0;
end;

// Clean
//
procedure TPool.Clean(nb : Integer);
var
   obj : TPooledObject;
begin
   if FCount=0 then Exit;

   FLock.BeginWrite;
   try
      if nb=0 then nb:=FCount;
      while (FRoot<>nil) and (nb>0) do begin
         obj:=FRoot;
         FRoot:=obj.FNext;
         obj.Destroy;
         Dec(nb);
         Dec(FCount);
      end;
   finally
      FLock.EndWrite;
   end;
end;

// Acquire
//
function TPool.Acquire : TPooledObject;
begin
   Result:=nil;
   if FRoot<>nil then begin
      FLock.BeginWrite;
      try
         if FRoot<>nil then begin
            Result:=FRoot;
            FRoot:=Result.FNext;
            Dec(FCount);
         end;
      finally
         FLock.EndWrite;
      end;
   end;
   if Result=nil then
      Result:=FPoolClass.Create
   else Result.FNext:=nil;
end;

// Release
//
procedure TPool.Release(obj : TPooledObject);
begin
   FLock.BeginWrite;
   try
      if FCount<FCapacity then begin
         obj.FNext:=FRoot;
         FRoot:=obj;
         obj:=nil;
         Inc(FCount);
      end;
   finally
      FLock.EndWrite;
   end;
   if obj<>nil then
      obj.Destroy;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   InitializeSmallIntegers;
   InitializeStringsUnifier;
   TSimpleIntegerStack.vTemplate:=TSimpleIntegerStack.Create;

finalization

   FinalizeStringsUnifier;
   TSimpleIntegerStack.vTemplate.Free;
   TObject(vWOBSPool).Free;

end.

