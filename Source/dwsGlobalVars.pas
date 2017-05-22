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
unit dwsGlobalVars;

interface

uses
   Classes, SysUtils, Variants, Masks,
   dwsUtils, dwsXPlatform, dwsXXHash;

const
   cGlobalVarHashmaps = 15;

type

   TGlobalVar = class(TPooledObject)
      public
         Value : Variant;
         Expire : UInt64;

         procedure Release;

         procedure WriteToFiler(writer: TWriter; const Name : String);
         procedure ReadFromFiler(reader: TReader; var Name : String);
   end;

   TNamesEnumerationCallback = procedure (const name : String) of object;

   TGlobalVarsGarbageCollector = class;

   TGlobalVarsHashMap = record
      Lock : TMultiReadSingleWrite;
      Hash : TNameObjectHash;

      procedure Initialize;
      procedure Finalize;

      function GetObjects(hashCode : Cardinal; const aName : String) : TGlobalVar; inline;
      procedure SetObjects(hashCode : Cardinal; const aName : String; obj : TGlobalVar); inline;
      property Objects[hashCode : Cardinal; const aName : String] : TGlobalVar read GetObjects write SetObjects; default;

      procedure Clean;
      procedure Cleanup(mask : TMask);
      function  Collect : Integer;

      procedure EnumerateNames(mask : TMask; callback : TNamesEnumerationCallback);
   end;
   PGlobalVarsHashMap = ^TGlobalVarsHashMap;

   TGlobalVars = record
      private
         Maps : array [0..cGlobalVarHashmaps] of TGlobalVarsHashMap;
         GC : TGlobalVarsGarbageCollector;
         GCTimer : ITimer;

      public
         procedure Initialize;
         procedure Finalize;
         function  Initialized : Boolean; inline;

         function Write(const aName : String; const aValue : Variant; expirationSeconds : Double) : Boolean;
         function TryRead(const aName : String; var value : Variant) : Boolean;

         function  Delete(const aName : String) : Boolean;
         procedure Cleanup(const filter : String = '*');

         procedure EnumerateNames(const filter : String; callback : TNamesEnumerationCallback);
         function  NamesCommaText : String;

         procedure SaveToFiler(writer : TWriter);
         procedure LoadFromFiler(reader : TReader);

         function Increment(const aName : String; const delta : Int64) : Int64;
         function CompareExchange(const aName : String; const value, comparand : Variant) : Variant;

         procedure Collect;
         procedure IncrementalCollect;
         function  Count : Integer;
   end;
   PGlobalVars = ^TGlobalVars;

   TGlobalVarsGarbageCollector = class
      private
         FOffset : Integer;
         FVars : PGlobalVars;

      public
         constructor Create(v : PGlobalVars);

         procedure IncrementalCollect;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cGlobalVarsLarge = 512;
   cGlobalVarsPoolSize = 64;
   cGarbageCollectionIntervalMilliseconds = 5*1000;
   cNoExpire = UInt64(-1);

var
   vGVPoolUsers : Integer;
   vGVPool : TPool;

// ------------------
// ------------------ TGlobalVar ------------------
// ------------------

// WriteToFiler
//
procedure TGlobalVar.WriteToFiler(writer: TWriter; const Name : String);
begin
   writer.WriteString(Name);
   WriteVariant(writer, Value);
end;

// ReadFromFiler
//
procedure TGlobalVar.ReadFromFiler(reader: TReader; var Name : String);
begin
   Name:=reader.ReadString;
   Value:=ReadVariant(reader);
end;

// Release
//
procedure TGlobalVar.Release;
begin
   VarClearSafe(Value);
   vGVPool.Release(Self);
end;

// ------------------
// ------------------ TGlobalVarsHashMap ------------------
// ------------------

// Initialize
//
procedure TGlobalVarsHashMap.Initialize;
begin
   Lock:=TMultiReadSingleWrite.Create;
   Hash:=TNameObjectHash.Create;
end;

// Finalize
//
procedure TGlobalVarsHashMap.Finalize;
begin
   FreeAndNil(Lock);
   if Hash <> nil then begin
      Hash.Clean;
      Hash.Free;
      Hash := nil;
   end;
end;

// GetObjects
//
function TGlobalVarsHashMap.GetObjects(hashCode : Cardinal; const aName : String) : TGlobalVar;
begin
   Result:=TGlobalVar(Hash.HashedObjects[aName, hashCode]);
end;

// SetObjects
//
procedure TGlobalVarsHashMap.SetObjects(hashCode : Cardinal; const aName : String; obj : TGlobalVar);
begin
   Hash.HashedObjects[aName, hashCode]:=obj;
end;

// Clean
//
procedure TGlobalVarsHashMap.Clean;
begin
   Lock.BeginWrite;
   try
      Hash.Clean;
   finally
      Lock.EndWrite;
   end;
end;

// Cleanup
//
procedure TGlobalVarsHashMap.Cleanup(mask : TMask);
var
   i, n : Integer;
   gv : TGlobalVar;
   t : UInt64;
   shouldCollect : Boolean;
   bucket : PNameObjectHashBucket;
begin
   t:=GetSystemMilliseconds;

   Lock.BeginWrite;
   try
      n:=0;
      bucket:=Hash.Bucket[0];
      for i:=0 to Hash.HighIndex do begin
         gv:=TGlobalVar(bucket.Obj);
         if gv=nil then
            Inc(n)
         else if    (gv.Expire<t)
                 or ((mask<>nil) and mask.Matches(bucket.Name)) then begin
            gv.Release;
            bucket.Obj:=nil;
            Inc(n);
         end;
         Inc(bucket);
      end;
      // if hash is large and 80% or more of the hash slots are nil, then rehash
      shouldCollect:=(Hash.HighIndex>cGlobalVarsLarge) and (5*n>4*Hash.HighIndex);
   finally
      Lock.EndWrite;
   end;
   if shouldCollect then
      Collect;
end;

// Collect
//
function TGlobalVarsHashMap.Collect : Integer;
var
   i : Integer;
   gv : TGlobalVar;
   t : UInt64;
   bucket : PNameObjectHashBucket;
begin
   Result:=0;
   if Hash.HighIndex<=0 then Exit;

   t:=GetSystemMilliseconds;

   Lock.BeginWrite;
   try
      bucket:=Hash.Bucket[0];
      for i:=0 to Hash.HighIndex do begin
         gv:=TGlobalVar(bucket.Obj);
         if gv<>nil then begin
            if gv.Expire<t then begin
               gv.Release;
               bucket.HashCode:=0;
               Inc(Result);
            end;
         end else if bucket.Name<>'' then begin
            bucket.HashCode:=0;
            Inc(Result);
         end;
         Inc(bucket);
      end;
      if Result > 0 then
         Hash.Pack
      else if Hash.Count <= ((Hash.HighIndex+1) shr 2) then begin
         Result := Hash.HighIndex shr 1;
         Hash.Pack;
      end;
   finally
      Lock.EndWrite;
   end;
end;
// EnumerateNames
//
procedure TGlobalVarsHashMap.EnumerateNames(mask : TMask; callback : TNamesEnumerationCallback);
var
   i : Integer;
   t : UInt64;
   bucket : PNameObjectHashBucket;
   gv : TGlobalVar;
begin
   if Hash.Count=0 then Exit;
   t:=GetSystemMilliseconds;
   Lock.BeginRead;
   try
      if Hash.HighIndex>0 then begin
         bucket:=Hash.Bucket[0];
         for i:=0 to Hash.HighIndex do begin
            gv:=TGlobalVar(bucket.Obj);
            if gv<>nil then begin
               if gv.Expire>=t then begin
                  if (mask=nil) or mask.Matches(bucket.Name) then
                     callback(bucket.Name);
               end else begin
                  gv.Release;
                  bucket.Obj:=nil;
               end;
            end;
            Inc(bucket);
         end;
      end;
   finally
      Lock.EndRead;
   end;
end;

// ------------------
// ------------------ TGlobalVars ------------------
// ------------------

// Initialize
//
procedure TGlobalVars.Initialize;
var
   i : Integer;
begin
   for i:=0 to High(Maps) do
      Maps[i].Initialize;
   GC:=TGlobalVarsGarbageCollector.Create(@Self);
   GCTimer:=TTimerTimeout.Create(cGarbageCollectionIntervalMilliseconds, GC.IncrementalCollect);

   Inc(vGVPoolUsers);
   if vGVPoolUsers=1 then begin
      vGVPool.Initialize(TGlobalVar);
      vGVPool.Capacity:=cGlobalVarsPoolSize;
   end;
end;

// Finalize
//
procedure TGlobalVars.Finalize;
var
   i : Integer;
begin
   GCTimer:=nil;
   FreeAndNil(GC);

   for i:=0 to High(Maps) do
      Maps[i].Finalize;

   Dec(vGVPoolUsers);
   if vGVPoolUsers=0 then begin
      vGVPool.Finalize;
   end;
end;

// Initialized
//
function TGlobalVars.Initialized : Boolean;
begin
   Result := (GC<>nil);
end;

// Write
//
function TGlobalVars.Write(const aName : String; const aValue : Variant; expirationSeconds : Double) : Boolean;
var
   gv : TGlobalVar;
   expire : UInt64;
   map : PGlobalVarsHashMap;
   h : Cardinal;
begin
   if expirationSeconds>0 then
      expire:=GetSystemMilliseconds+Round(expirationSeconds*1000)
   else expire:=cNoExpire;

   h:=TNameObjectHash.HashName(aName);
   map:=@Maps[h and High(Maps)];

   map.Lock.BeginWrite;
   try
      gv:=map.Objects[h, aName];
      if gv=nil then begin
         gv:=TGlobalVar(vGVPool.Acquire);
         map.Objects[h, aName]:=gv;
         Result:=True;
      end else Result:=False;
      gv.Value:=aValue;
      gv.Expire:=expire;
   finally
      map.Lock.EndWrite;
   end;
end;

// TryRead
//
function TGlobalVars.TryRead(const aName : String; var value : Variant) : Boolean;
var
   gv : TGlobalVar;
   t : UInt64;
   map : PGlobalVarsHashMap;
   h : Cardinal;
begin
   Result:=False;
   t:=GetSystemMilliseconds;

   h:=TNameObjectHash.HashName(aName);
   map:=@Maps[h and High(Maps)];

   map.Lock.BeginRead;
   try
      gv:=map.Objects[h, aName];
      if (gv<>nil) and (gv.Expire>=t) then begin
         value:=gv.Value;
         Result:=True;
      end;
   finally
      map.Lock.EndRead;
   end;
end;

// Delete
//
function TGlobalVars.Delete(const aName : String) : Boolean;
var
   gv : TGlobalVar;
   map : PGlobalVarsHashMap;
   h : Cardinal;
begin
   Result:=False;

   h:=TNameObjectHash.HashName(aName);
   map:=@Maps[h and High(Maps)];

   map.Lock.BeginWrite;
   try
      gv:=map.Objects[h, aName];
      if gv<>nil then begin
         gv.Expire:=0;
         VarClearSafe(gv.Value);
         Result:=True;
      end;
   finally
      map.Lock.EndWrite;
   end;
end;

// Cleanup
//
procedure TGlobalVars.Cleanup(const filter : String = '*');
var
   i : Integer;
   mask : TMask;
begin
   if filter='*' then begin

      for i:=0 to High(Maps) do
         Maps[i].Clean;

   end else begin

      if filter='' then
         mask:=nil
      else mask:=TMask.Create(filter);
      try
         for i:=0 to High(Maps) do
            Maps[i].Cleanup(mask);
      finally
         mask.Free;
      end;

   end;
end;

// EnumerateNames
//
procedure TGlobalVars.EnumerateNames(const filter : String; callback : TNamesEnumerationCallback);
var
   i : Integer;
   mask : TMask;
begin
   if filter<>'*' then
      mask:=TMask.Create(filter)
   else mask:=nil;
   try
      for i:=0 to High(Maps) do
         Maps[i].EnumerateNames(mask, callback);
   finally
      mask.Free;
   end;
end;

// NamesCommaText
//
type
   TStringsAdder = class(TStrings)
      procedure Add(const name : String); reintroduce;
   end;
procedure TStringsAdder.Add(const name : String);
begin
   inherited Add(name);
end;
function TGlobalVars.NamesCommaText : String;
var
   list : TStringList;
begin
   list:=TStringList.Create;
   try
      EnumerateNames('*', TStringsAdder(list).Add);
      if list.Count > 0 then
         Result := list.CommaText
      else Result := '';
   finally
      list.Free;
   end;
end;

// SaveToFiler
//
procedure TGlobalVars.SaveToFiler(writer : TWriter);
var
   i, j : Integer;
   gv : TGlobalVar;
   hash : TNameObjectHash;
begin
   writer.WriteListBegin;

   for i:=0 to High(Maps) do begin
      Maps[i].Lock.BeginRead;
      try
         hash:=Maps[i].Hash;
         for j:=0 to hash.HighIndex do begin
            gv:=TGlobalVar(hash.BucketObject[j]);
            if (gv<>nil) and (gv.Expire=cNoExpire) then
               gv.WriteToFiler(writer, hash.BucketName[j]);
         end;
      finally
         Maps[i].Lock.EndRead;
      end;
   end;

   writer.WriteListEnd;
end;

// LoadFromFiler
//
procedure TGlobalVars.LoadFromFiler(reader : TReader);
var
   i : Integer;
   h : Cardinal;
   name : String;
   gv : TGlobalVar;
   map : PGlobalVarsHashMap;
begin
   reader.ReadListBegin;

   for i:=0 to High(Maps) do
      Maps[i].Lock.BeginWrite;
   try
      for i:=0 to High(Maps) do
         Maps[i].Hash.Clean;

      while not reader.EndOfList do begin
         gv:=TGlobalVar(vGVPool.Acquire);
         gv.ReadFromFiler(reader, name);
         gv.Expire:=cNoExpire;
         h:=TNameObjectHash.HashName(name);
         map:=@Maps[h and High(Maps)];
         map.Objects[h, name]:=gv;
      end;
   finally
      for i:=0 to High(Maps) do
         Maps[i].Lock.EndWrite;
   end;

   reader.ReadListEnd;
end;


// Increment
//
function TGlobalVars.Increment(const aName : String; const delta : Int64) : Int64;
var
   gv : TGlobalVar;
   t : UInt64;
   h : Cardinal;
   map : PGlobalVarsHashMap;
begin
   t:=GetSystemMilliseconds;

   h:=TNameObjectHash.HashName(aName);
   map:=@Maps[h and High(Maps)];

   map.Lock.BeginWrite;
   try
      gv:=map.Objects[h, aName];
      if gv=nil then begin
         gv:=TGlobalVar(vGVPool.Acquire);
         map.Objects[h, aName]:=gv;
         gv.Expire:=cNoExpire;
         Result:=delta;
      end else begin
         if gv.Expire>=t then
            Result:=delta+gv.Value
         else Result:=delta;
      end;
      gv.Value:=Result;
   finally
      map.Lock.EndWrite;
   end;
end;

// CompareExchange
//
function TGlobalVars.CompareExchange(const aName : String; const value, comparand : Variant) : Variant;
var
   gv : TGlobalVar;
   t : UInt64;
   h : Cardinal;
   map : PGlobalVarsHashMap;
begin
   t:=GetSystemMilliseconds;

   h:=TNameObjectHash.HashName(aName);
   map:=@Maps[h and High(Maps)];

   map.Lock.BeginWrite;
   try
      gv:=map.Objects[h, aName];
      if (gv<>nil) and (gv.Expire>=t) then
         Result:=gv.Value
      else Result:=Unassigned;

      if (VarType(Result)=VarType(comparand)) and (Result=comparand) then begin
         if gv=nil then begin
            gv:=TGlobalVar(vGVPool.Acquire);
            gv.Expire:=cNoExpire;
            map.Objects[h, aName]:=gv;
         end;
         gv.Value:=value;
      end;
   finally
      map.Lock.EndWrite;
   end;
end;

// Collect
//
procedure TGlobalVars.Collect;
var
   i : Integer;
begin
   for i:=0 to High(Maps) do
      Maps[i].Collect;
end;

// IncrementalCollect
//
procedure TGlobalVars.IncrementalCollect;
begin
   GC.IncrementalCollect;
end;

// Count
//
function  TGlobalVars.Count : Integer;
var
   i : Integer;
begin
   Result:=0;
   for i:=0 to High(Maps) do
      Inc(Result, Maps[i].Hash.Count);
end;

// ------------------
// ------------------ TGlobalVarsGarbageCollector ------------------
// ------------------

// Create
//
constructor TGlobalVarsGarbageCollector.Create(v : PGlobalVars);
begin
   FVars:=v;
end;

// IncrementalCollect
//
procedure TGlobalVarsGarbageCollector.IncrementalCollect;
var
   t, n : Integer;
begin
   t := cGarbageCollectionIntervalMilliseconds;

   vGVPool.Clean(16+(vGVPool.Count shr 2));

   FOffset:=(FOffset+1) and High(FVars.Maps);
   n:=FVars.Maps[FOffset].Collect;
   if n > 128 then begin
      if n > 1024 then
         t := 10
      else t := 100;
   end;
   FVars.GCTimer:=TTimerTimeout.Create(t, IncrementalCollect)
end;

end.
