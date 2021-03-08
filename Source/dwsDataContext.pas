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
unit dwsDataContext;

{$I dws.inc}

interface

uses
   dwsXPlatform, dwsUtils, dwsXXHash;

type

   TData = array of Variant;
   PData = ^TData;
   TDataArray = array [0..MaxInt shr 5] of Variant;
   PDataArray = ^TDataArray;
   TVarDataArray = array [0..MaxInt shr 5] of TVarData;
   PVarDataArray = ^TVarDataArray;
   PIUnknown = ^IUnknown;

   IDataContext = interface (IGetSelf)
      ['{306EAD7F-1FEC-4D6F-8579-F48D75C5C1FF}']
      function GetAsVariant(addr : NativeInt) : Variant;
      procedure SetAsVariant(addr : NativeInt; const value : Variant);
      function GetAsInteger(addr : NativeInt) : Int64;
      procedure SetAsInteger(addr : NativeInt; const value : Int64);
      function GetAsFloat(addr : NativeInt) : Double;
      procedure SetAsFloat(addr : NativeInt; const value : Double);
      function GetAsBoolean(addr : NativeInt) : Boolean;
      procedure SetAsBoolean(addr : NativeInt; const value : Boolean);
      function GetAsString(addr : NativeInt) : String;
      procedure SetAsString(addr : NativeInt; const value : String);
      function GetAsInterface(addr : NativeInt) : IUnknown;
      procedure SetAsInterface(addr : NativeInt; const value : IUnknown);

      function Addr : NativeInt;
      function DataLength : NativeInt;

      property AsVariant[addr : NativeInt] : Variant read GetAsVariant write SetAsVariant; default;

      function AsPData : PData;

      procedure CreateOffset(offset : NativeInt; var result : IDataContext);

      property  AsInteger[addr : NativeInt] : Int64 read GetAsInteger write SetAsInteger;
      property  AsBoolean[addr : NativeInt] : Boolean read GetAsBoolean write SetAsBoolean;
      property  AsFloat[addr : NativeInt] : Double read GetAsFloat write SetAsFloat;
      property  AsString[addr : NativeInt] : String read GetAsString write SetAsString;
      property  AsInterface[addr : NativeInt] : IUnknown read GetAsInterface write SetAsInterface;

      procedure EvalAsVariant(addr : NativeInt; var result : Variant);
      procedure EvalAsString(addr : NativeInt; var result : String);
      procedure EvalAsInterface(addr : NativeInt; var result : IUnknown);

      function IsEmpty(addr : NativeInt) : Boolean;
      function VarType(addr : NativeInt) : TVarType;

      procedure CopyData(const destData : TData; destAddr, size : NativeInt);
      procedure WriteData(const src : IDataContext; size : NativeInt); overload;
      procedure WriteData(destAddr : NativeInt; const src : IDataContext; size : NativeInt); overload;
      procedure WriteData(const srcData : TData; srcAddr, size : NativeInt); overload;
      function  SameData(addr : NativeInt; const otherData : TData; otherAddr, size : NativeInt) : Boolean;

      function  IncInteger(addr : NativeInt; delta : Int64) : Int64;

      function  HashCode(size : NativeInt) : Cardinal;
   end;

   TDataContext = class;

   IDataContextPool = interface
      function Create(const aData : TData; anAddr : NativeInt) : TDataContext;
      procedure Cleanup;
   end;

   TDataContextPool = class (TInterfacedObject, IDataContextPool)
      private
         FHead : TDataContext;
         FAll : TDataContext;

      protected
         function CreateEmpty : TDataContext;
         function Pop : TDataContext; inline;
         procedure Push(ref : TDataContext); inline;
         procedure Cleanup;

         function CreateData(const aData : TData; anAddr : NativeInt) : TDataContext;
         function CreateOffset(offset : NativeInt; ref : TDataContext) : TDataContext;

         function IDataContextPool.Create = CreateData;
   end;
   PDataPtrPool= ^TDataContextPool;

   TDataContext = class(TInterfacedObject, IDataContext, IGetSelf)
      private
         FAddr : NativeInt;
         FData : TData;
         FNext : TDataContext;
         FPool : TDataContextPool;
         FAllNext : TDataContext;
{$IFDEF DELPHI_2010_MINUS}
      protected // D2009 needs protected here to "see" these methods in inherited classes
{$ENDIF}
         function GetAsVariant(addr : NativeInt) : Variant; inline;
         procedure SetAsVariant(addr : NativeInt; const value : Variant); inline;
         function GetAsInteger(addr : NativeInt) : Int64; inline;
         procedure SetAsInteger(addr : NativeInt; const value : Int64); inline;
         function GetAsFloat(addr : NativeInt) : Double; inline;
         procedure SetAsFloat(addr : NativeInt; const value : Double); inline;
         function GetAsBoolean(addr : NativeInt) : Boolean; inline;
         procedure SetAsBoolean(addr : NativeInt; const value : Boolean); inline;
         function GetAsString(addr : NativeInt) : String; inline;
         procedure SetAsString(addr : NativeInt; const value : String); inline;
         function GetAsInterface(addr : NativeInt) : IUnknown; inline;
         procedure SetAsInterface(addr : NativeInt; const value : IUnknown); inline;

         function _Release: Integer; stdcall;

      protected
         property DirectData : TData read FData;

      public
         constructor CreateStandalone(size : NativeInt);

         function GetSelf : TObject;

         property AsVariant[addr : NativeInt] : Variant read GetAsVariant write SetAsVariant; default;
         function AsPData : PData; inline;
         function Addr : NativeInt;
         function DataLength : NativeInt; inline;
         procedure Offset(delta : NativeInt); inline;

         procedure CreateOffset(offset : NativeInt; var result : IDataContext);

         procedure EvalAsVariant(addr : NativeInt; var result : Variant); inline;
         procedure EvalAsString(addr : NativeInt; var result : String); inline;
         procedure EvalAsInterface(addr : NativeInt; var result : IUnknown); inline;

         property  AsInteger[addr : NativeInt] : Int64 read GetAsInteger write SetAsInteger;
         property  AsBoolean[addr : NativeInt] : Boolean read GetAsBoolean write SetAsBoolean;
         property  AsFloat[addr : NativeInt] : Double read GetAsFloat write SetAsFloat;
         property  AsString[addr : NativeInt] : String read GetAsString write SetAsString;
         property  AsInterface[addr : NativeInt] : IUnknown read GetAsInterface write SetAsInterface;

         function IsEmpty(addr : NativeInt) : Boolean;
         function VarType(addr : NativeInt) : TVarType; virtual;

         procedure InternalCopyData(sourceAddr, destAddr, size : NativeInt); inline;

         procedure CopyData(const destData : TData; destAddr, size : NativeInt); overload; inline;
         procedure CopyData(addr : NativeInt; const destData : TData; destAddr, size : NativeInt); overload; inline;
         procedure CopyData(addr : NativeInt; const destPVariant : PVariant; size : NativeInt); overload; inline;

         procedure WriteData(const src : IDataContext; size : NativeInt); overload; inline;
         procedure WriteData(const src : IDataContext; srcAddr, size : NativeInt); overload; inline;
         procedure WriteData(destAddr : NativeInt; const src : IDataContext; size : NativeInt); overload; inline;
         procedure WriteData(const srcData : TData; srcAddr, size : NativeInt); overload; inline;

         procedure MoveData(srcAddr, destAddr, size : NativeInt); inline;

         function  SameData(addr : NativeInt; const otherData : TData; otherAddr, size : NativeInt) : Boolean; overload; inline;
         function  SameData(addr : NativeInt; const otherData : IDataContext; size : NativeInt) : Boolean; overload; inline;

         function IndexOfData(const item : IDataContext; fromIndex, toIndex, itemSize : NativeInt) : NativeInt;
         function IndexOfValue(const item : Variant; fromIndex, toIndex : NativeInt) : NativeInt;
         function IndexOfString(const item : String; fromIndex : NativeInt) : NativeInt;
         function IndexOfInteger(const item : Int64; fromIndex : NativeInt) : NativeInt;
         function IndexOfFloat(const item : Double; fromIndex : NativeInt) : NativeInt;

         procedure ReplaceData(const newData : TData); virtual;
         procedure ClearData; virtual;
         procedure SetDataLength(n : NativeInt);

         function  IncInteger(addr : NativeInt; delta : Int64) : Int64;

         function  HashCode(size : NativeInt) : Cardinal;
   end;

   TGetPDataFunc = function : PData of object;

   TRelativeDataContext = class (TInterfacedObject, IDataContext, IGetSelf)
      private
         FGetPData : TGetPDataFunc;
         FAddr : NativeInt;

      public
         constructor Create(const getPData : TGetPDataFunc; addr : NativeInt);

         function GetSelf : TObject;

         function GetAsVariant(addr : NativeInt) : Variant;
         procedure SetAsVariant(addr : NativeInt; const value : Variant);
         function GetAsInteger(addr : NativeInt) : Int64;
         procedure SetAsInteger(addr : NativeInt; const value : Int64);
         function GetAsFloat(addr : NativeInt) : Double;
         procedure SetAsFloat(addr : NativeInt; const value : Double);
         function GetAsBoolean(addr : NativeInt) : Boolean;
         procedure SetAsBoolean(addr : NativeInt; const value : Boolean);
         function GetAsString(addr : NativeInt) : String;
         procedure SetAsString(addr : NativeInt; const value : String);
         function GetAsInterface(addr : NativeInt) : IUnknown;
         procedure SetAsInterface(addr : NativeInt; const value : IUnknown);

         function Addr : NativeInt;
         function DataLength : NativeInt;

         function AsPData : PData;

         procedure CreateOffset(offset : NativeInt; var result : IDataContext);

         procedure EvalAsVariant(addr : NativeInt; var result : Variant);
         procedure EvalAsString(addr : NativeInt; var result : String);
         procedure EvalAsInterface(addr : NativeInt; var result : IUnknown);

         function IsEmpty(addr : NativeInt) : Boolean;
         function VarType(addr : NativeInt) : TVarType;

         procedure CopyData(const destData : TData; destAddr, size : NativeInt);
         procedure WriteData(const src : IDataContext; size : NativeInt); overload;
         procedure WriteData(destAddr : NativeInt; const src : IDataContext; size : NativeInt); overload;
         procedure WriteData(const srcData : TData; srcAddr, size : NativeInt); overload;
         function SameData(addr : NativeInt; const otherData : TData; otherAddr, size : NativeInt) : Boolean; overload;

         function  IncInteger(addr : NativeInt; delta : Int64) : Int64;

         function  HashCode(size : NativeInt) : Cardinal;
   end;

procedure DWSCopyPVariants(src, dest : PVariant; size : NativeInt); inline;

procedure DWSCopyData(const sourceData : TData; sourceAddr : NativeInt;
                      const destData : TData; destAddr : NativeInt; size : NativeInt); overload;
procedure DWSCopyData(const data : TData; sourceAddr, destAddr : NativeInt; size : NativeInt); overload;

procedure DWSMoveData(const data : TData; sourceAddr, destAddr, size : NativeInt);

function DWSSameData(const data1, data2 : TData; offset1, offset2, size : NativeInt) : Boolean; overload;
function DWSSameData(const data1, data2 : TData) : Boolean; overload;
function DWSSameVariant(const v1, v2 : Variant) : Boolean;

const
   // https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
   cFNV_prime = 16777619;
   cFNV_basis = 2166136261;

procedure DWSHashCode(var partial : Cardinal; const v : Variant); overload;
function DWSHashCode(const v : Variant) : Cardinal; overload;
function DWSHashCode(const data : TData; offset, size : NativeInt) : Cardinal; overload;
function DWSHashCode(p : PVariant; size : NativeInt) : Cardinal; overload;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// DWSCopyPVariants
//
procedure DWSCopyPVariants(src, dest : PVariant; size : NativeInt); inline;
begin
   while size > 0 do begin
      VarCopySafe(dest^, src^);
      Inc(src);
      Inc(dest);
      Dec(size);
   end;
end;

// DWSCopyData
//
procedure DWSCopyData(const sourceData: TData; sourceAddr: NativeInt;
                      const destData: TData; destAddr: NativeInt; size: NativeInt);
var
   src, dest : PVariant;
begin
   src := @sourceData[sourceAddr];
   dest := @destData[destAddr];
   DWSCopyPVariants(src, dest, size);
end;

// DWSCopyData
//
procedure DWSCopyData(const data : TData; sourceAddr, destAddr : NativeInt; size : NativeInt);
var
   i : NativeInt;
begin
   if sourceAddr > destAddr then begin
      for i := 0 to size-1 do
         VarCopySafe(data[destAddr+i], data[sourceAddr+i])
   end else begin
      for i := size-1 downto 0 do
         VarCopySafe(data[destAddr+i], data[sourceAddr+i])
   end;
end;

// DWSMoveData
//
procedure DWSMoveData(const data : TData; sourceAddr, destAddr, size : NativeInt);
const
   cStaticBufferSize = 4*SizeOf(Variant);
var
   bufVariant : array[0..cStaticBufferSize-1] of Byte;
   buf : Pointer;
   sizeBytes : NativeInt;
begin
   if sourceAddr = destAddr then Exit;

   sizeBytes := size * SizeOf(Variant);
   if sizeBytes <= cStaticBufferSize then
      buf := @bufVariant
   else buf := GetMemory(sizeBytes);

   System.Move(data[sourceAddr], buf^, sizeBytes);
   if sourceAddr < destAddr then
      System.Move(data[sourceAddr+size], data[sourceAddr], SizeOf(Variant)*(destAddr-sourceAddr))
   else System.Move(data[destAddr], data[destAddr+size], SizeOf(Variant)*(sourceAddr-destAddr));
   System.Move(buf^, data[destAddr], sizeBytes);

   if buf <> @bufVariant then
      FreeMemory(buf);
end;

// DWSSameData
//
function DWSSameData(const data1, data2 : TData; offset1, offset2, size : NativeInt) : Boolean;
var
   i : NativeInt;
begin
   for i:=0 to size-1 do
      if not DWSSameVariant(data1[offset1+i], data2[offset2+i]) then
         Exit(False);
   Result:=True;
end;

// DWSSameData
//
function DWSSameData(const data1, data2 : TData) : Boolean;
var
   s : NativeInt;
begin
   s:=Length(data1);
   Result:=(s=Length(data2)) and DWSSameData(data1, data2, 0, 0, s);
end;

// DWSSameVariant
//
function DWSSameVariant(const v1, v2 : Variant) : Boolean;
var
   vt : TVarType;
begin
   vt:=TVarData(v1).VType;
   if vt<>TVarData(v2).VType then
      Result:=False
   else begin
      case vt of
         varInt64 :
            Result:=TVarData(v1).VInt64=TVarData(v2).VInt64;
         varBoolean :
            Result:=TVarData(v1).VBoolean=TVarData(v2).VBoolean;
         varDouble :
            Result:=TVarData(v1).VDouble=TVarData(v2).VDouble;
         {$ifdef FPC}
         varString :
            Result:=String(TVarData(v1).VString)=String(TVarData(v2).VString);
         {$else}
         varUString :
            Result:=String(TVarData(v1).VUString)=String(TVarData(v2).VUString);
         {$endif}
         varUnknown :
            Result:=TVarData(v1).VUnknown=TVarData(v2).VUnknown;
      else
         Result:=(v1=v2);
      end;
   end;
end;

// DWSHashCode
//
procedure DWSHashCode(var partial : Cardinal; const v : Variant); overload;
var
   p : PVarData;
begin
   p := @v;
   case p.VType of
      varByte, varBoolean, varShortInt : // 8 bits
         partial := (partial xor SimpleIntegerHash(p.VByte)) * cFNV_prime;
      varSmallint, varWord : begin // 16 bits
         partial := (partial xor SimpleIntegerHash(p.VWord)) * cFNV_prime;
      end;
      varInteger, varSingle, varLongWord, varUnknown, varDispatch : begin // 32 bits
         partial := (partial xor SimpleIntegerHash(p.VLongWord)) * cFNV_prime;
      end;
      varInt64, varDouble, varCurrency, varDate, varUInt64 : begin // 64 bits
         partial := (partial xor SimpleInt64Hash(p.VInt64)) * cFNV_prime;
      end;
      {$ifndef FPC}
      varUString : begin
         if p.VUString <> nil then
            partial := (partial xor SimpleStringHash(String(p.VUString))) * cFNV_prime
         else partial := partial * cFNV_prime;
      end;
      {$endif}
      varString : begin
         if p.VString <> nil then
            partial := (partial xor SimpleByteHash(p.VString, Length(AnsiString(p.VString)))) * cFNV_prime
         else partial := partial * cFNV_prime;
      end;
      varOleStr : begin
         if p.VOleStr <> nil then
            partial := (partial xor SimpleStringHash(p.VOleStr, Length(p.VOleStr))) * cFNV_prime
         else partial := partial * 16777619;
      end;
   else
      partial := (partial xor p.VType) * cFNV_prime;
   end;
end;

function DWSHashCode(const v : Variant) : Cardinal;
begin
   Result := cFNV_basis;
   DWSHashCode(Result, v);
   if Result = 0 then
      Result := cFNV_basis;
end;

function DWSHashCode(const data : TData; offset, size : NativeInt) : Cardinal;
var
   i : NativeInt;
begin
   Result := cFNV_basis;
   for i := offset to offset+size-1 do
      DWSHashCode(Result, data[i]);
   if Result = 0 then
      Result := cFNV_basis;
end;

function DWSHashCode(p : PVariant; size : NativeInt) : Cardinal; overload;
var
   i : NativeInt;
begin
   Result := cFNV_basis;
   for i := 1 to size do begin
      DWSHashCode(Result, p^);
      Inc(p);
   end;
   if Result = 0 then
      Result := cFNV_basis;
end;

// DWSVarIsEmpty
//
function DWSVarIsEmpty(const v : Variant) : Boolean; inline;
begin
   Result :=    (TVarData(v).VType = varEmpty)
             or (
                     (TVarData(v).VType = varUnknown)
                 and (TVarData(v).VUnknown = nil)
                );
end;

// ------------------
// ------------------ TDataContextPool ------------------
// ------------------

// CreateEmpty
//
function TDataContextPool.CreateEmpty : TDataContext;
begin
   Result:=TDataContext.Create;
   Result.FPool:=Self;
   Result.FAllNext:=FAll;
   FAll:=Result;
end;

// Pop
//
function TDataContextPool.Pop : TDataContext;
begin
   if FHead=nil then
      Result:=CreateEmpty
   else begin
      Result:=FHead;
      FHead:=FHead.FNext;
   end;
end;

// Push
//
procedure TDataContextPool.Push(ref : TDataContext);
begin
   if Self=nil then
      ref.Free
   else begin
      ref.FNext:=FHead;
      FHead:=ref;
      ref.FData:=nil;
      ref.FAddr:=0;
   end;
end;

// Cleanup
//
procedure TDataContextPool.Cleanup;
var
   iter : TDataContext;
begin
   // detach all from the pool
   iter:=FAll;
   while iter<>nil do begin
      iter.FPool:=nil;
      iter:=iter.FAllNext;
   end;
   FAll:=nil;
   // free all the pooled ones.
   while FHead<>nil do begin
      iter:=FHead;
      FHead:=iter.FNext;
      iter.Destroy;
   end;
end;

// CreateData
//
function TDataContextPool.CreateData(const aData : TData; anAddr : NativeInt) : TDataContext;
begin
   Result:=Pop;
   Result.FAddr:=anAddr;
   Result.FData:=aData;
end;

// CreateOffset
//
function TDataContextPool.CreateOffset(offset : NativeInt; ref : TDataContext) : TDataContext;
begin
   Result:=Pop;
   Result.FAddr:=ref.FAddr+offset;
   Result.FData:=ref.FData;
end;

// ------------------
// ------------------ TDataContext ------------------
// ------------------

// _Release
//
function TDataContext._Release: Integer;
begin
   Result := InterlockedDecrement(FRefCount);
   if Result = 0 then
      FPool.Push(Self);
end;

// CreateStandalone
//
constructor TDataContext.CreateStandalone(size : NativeInt);
begin
   inherited Create;
   SetLength(FData, size);
end;

// GetSelf
//
function TDataContext.GetSelf : TObject;
begin
   Result:=Self;
end;

// GetAsVariant
//
function TDataContext.GetAsVariant(addr : NativeInt) : Variant;
begin
   VarCopySafe(Result, FData[FAddr+addr]);
end;

// SetAsVariant
//
procedure TDataContext.SetAsVariant(addr : NativeInt; const value : Variant);
begin
   VarCopySafe(FData[FAddr+addr], value);
end;

// GetAsInteger
//
function TDataContext.GetAsInteger(addr : NativeInt) : Int64;
var
   p : PVarData;
begin
   p:=@FData[FAddr+addr];
   if p^.VType=varInt64 then
      Result:=p^.VInt64
   else VariantToInt64(PVariant(p)^, Result);
end;

// SetAsInteger
//
procedure TDataContext.SetAsInteger(addr : NativeInt; const value : Int64);
var
   p : PVarData;
begin
   p:=@FData[FAddr+addr];
   if p^.VType=varInt64 then
      p^.VInt64:=value
   else VarCopySafe(PVariant(p)^, value);
end;

// GetAsFloat
//
function TDataContext.GetAsFloat(addr : NativeInt) : Double;
var
   p : PVarData;
begin
   p:=@FData[FAddr+addr];
   if p^.VType=varDouble then
      Result:=p^.VDouble
   else Result:=VariantToFloat(PVariant(p)^);
end;

// SetAsFloat
//
procedure TDataContext.SetAsFloat(addr : NativeInt; const value : Double);
var
   p : PVarData;
begin
   p:=@FData[FAddr+addr];
   if p^.VType=varDouble then
      p^.VDouble:=value
   else VarCopySafe(PVariant(p)^, value);
end;

// GetAsBoolean
//
function TDataContext.GetAsBoolean(addr : NativeInt) : Boolean;
var
   p : PVarData;
begin
   p:=@FData[FAddr+addr];
   if p^.VType=varBoolean then
      Result:=p^.VBoolean
   else Result:=VariantToBool(PVariant(p)^);
end;

// SetAsBoolean
//
procedure TDataContext.SetAsBoolean(addr : NativeInt; const value : Boolean);
var
   p : PVarData;
begin
   p:=@FData[FAddr+addr];
   if p^.VType=varBoolean then
      p^.VBoolean:=value
   else VarCopySafe(PVariant(p)^, value);
end;

// GetAsString
//
function TDataContext.GetAsString(addr : NativeInt) : String;
begin
   EvalAsString(addr, Result);
end;

// SetAsString
//
procedure TDataContext.SetAsString(addr : NativeInt; const value : String);
var
   p : PVarData;
begin
   p:=@FData[FAddr+addr];
   {$ifdef FPC}
   if p.VType=varString then
      String(p.VString):=value
   {$else}
   if p.VType=varUString then
      String(p.VUString):=value
   {$endif}
   else VarCopySafe(PVariant(p)^, value);
end;

// GetAsInterface
//
function TDataContext.GetAsInterface(addr : NativeInt) : IUnknown;
var
   p : PVarData;
begin
   p:=@FData[FAddr+addr];
   if p^.VType=varUnknown then
      Result:=IUnknown(p^.VUnknown)
   else Result:=PVariant(p)^;
end;

// SetAsInterface
//
procedure TDataContext.SetAsInterface(addr : NativeInt; const value : IUnknown);
var
   p : PVarData;
begin
   p:=@FData[FAddr+addr];
   if p^.VType=varUnknown then
      IUnknown(p^.VUnknown):=value
   else VarCopySafe(PVariant(p)^, value);
end;

// AsPData
//
function TDataContext.AsPData : PData;
begin
   Result:=@FData;
end;

// Addr
//
function TDataContext.Addr : NativeInt;
begin
   Result:=FAddr;
end;

// DataLength
//
function TDataContext.DataLength : NativeInt;
begin
   Result:=System.Length(FData);
end;

// Offset
//
procedure TDataContext.Offset(delta : NativeInt);
begin
   Inc(FAddr, delta);
end;

// CreateOffset
//
procedure TDataContext.CreateOffset(offset : NativeInt; var result : IDataContext);

   function CreateData(context : TDataContext; addr : NativeInt) : TDataContext;
   begin
      Result:=TDataContext.Create;
      Result.FData:=context.FData;
      Result.FAddr:=addr;
   end;

begin
   if FPool<>nil then
      Result:=FPool.CreateOffset(offset, Self)
   else Result:=CreateData(Self, FAddr+offset);
end;

// EvalAsVariant
//
procedure TDataContext.EvalAsVariant(addr : NativeInt; var result : Variant);
begin
   VarCopySafe(result, FData[FAddr+addr]);
end;

// EvalAsString
//
procedure TDataContext.EvalAsString(addr : NativeInt; var result : String);
var
   p : PVarData;
begin
   p:=@FData[FAddr+addr];
   {$ifdef FPC}
   if p.VType=varString then
      result:=String(p.VString)
   else VariantToString(PVariant(p)^, result);
   {$else}
   if p.VType=varUString then
      result:=String(p.VUString)
   else VariantToString(PVariant(p)^, result);
   {$endif}
end;

// EvalAsInterface
//
procedure TDataContext.EvalAsInterface(addr : NativeInt; var result : IUnknown);
var
   p : PVarData;
begin
   p:=@FData[FAddr+addr];
   if p^.VType=varUnknown then
      result:=IUnknown(p^.VUnknown)
   else result:=PVariant(p)^;
end;

// IsEmpty
//
function TDataContext.IsEmpty(addr : NativeInt) : Boolean;
begin
   Result := DWSVarIsEmpty(FData[FAddr+addr]);
end;

// VarType
//
function TDataContext.VarType(addr : NativeInt) : TVarType;
begin
   Result := VarType(FData[FAddr+addr]);
end;

// InternalCopyData
//
procedure TDataContext.InternalCopyData(sourceAddr, destAddr, size : NativeInt);
begin
   DWSCopyData(FData, sourceAddr, destAddr, size);
end;

// CopyData
//
procedure TDataContext.CopyData(const destData : TData; destAddr, size : NativeInt);
begin
   DWSCopyData(FData, FAddr, destData, destAddr, size);
end;

// CopyData
//
procedure TDataContext.CopyData(addr : NativeInt; const destData : TData; destAddr, size : NativeInt);
begin
   DWSCopyData(FData, FAddr+addr, destData, destAddr, size);
end;

// CopyData
//
procedure TDataContext.CopyData(addr : NativeInt; const destPVariant : PVariant; size : NativeInt);
begin
   DWSCopyPVariants(@FData[FAddr+addr], destPVariant, size);
end;

// WriteData
//
procedure TDataContext.WriteData(const src : IDataContext; size : NativeInt);
begin
   WriteData(src, 0, size);
end;

// WriteData
//
procedure TDataContext.WriteData(const src : IDataContext; srcAddr, size : NativeInt);
var
   i : NativeInt;
   pDest : PVariant;
begin
   Assert(FAddr + size <= Length(FData));
   pDest := @FData[FAddr];
   for i := srcAddr to srcAddr + size-1 do begin
      src.EvalAsVariant(i, pDest^);
      Inc(pDest);
   end;
end;

// WriteData
//
procedure TDataContext.WriteData(destAddr : NativeInt; const src : IDataContext; size : NativeInt);
var
   i : NativeInt;
   pDest : PVariant;
begin
   Assert(FAddr + destAddr + size <= Length(FData));
   pDest := @FData[FAddr + destAddr];
   for i := 0 to size-1 do begin
      src.EvalAsVariant(i, pDest^);
      Inc(pDest);
   end;
end;

// WriteData
//
procedure TDataContext.WriteData(const srcData : TData; srcAddr, size : NativeInt);
begin
   DWSCopyData(srcData, srcAddr, FData, FAddr, size);
end;

// MoveData
//
procedure TDataContext.MoveData(srcAddr, destAddr, size : NativeInt);
begin
   DWSMoveData(FData, srcAddr, destAddr, size);
end;

// SameData
//
function TDataContext.SameData(addr : NativeInt; const otherData : TData; otherAddr, size : NativeInt) : Boolean;
begin
   Result:=DWSSameData(FData, otherData, FAddr+addr, otherAddr, size);
end;

// SameData
//
function TDataContext.SameData(addr : NativeInt; const otherData : IDataContext; size : NativeInt) : Boolean;
begin
   Result:=DWSSameData(FData, otherData.AsPData^, FAddr+addr, otherData.Addr, size);
end;

// IndexOfData
//
function TDataContext.IndexOfData(const item : IDataContext; fromIndex, toIndex, itemSize : NativeInt) : NativeInt;
var
   i : NativeInt;
   data : PData;
begin
   data := AsPData;
   for i:=fromIndex to toIndex do
      if item.SameData(0, data^, Addr+i*itemSize, itemSize) then
         Exit(i);
   Result:=-1;
end;

// IndexOfValue
//
function TDataContext.IndexOfValue(const item : Variant; fromIndex, toIndex : NativeInt) : NativeInt;
var
   i : NativeInt;
   data : PData;
begin
   data:=AsPData;
   for i:=fromIndex to toIndex do
      if DWSSameVariant(data^[Addr+i], item) then
         Exit(i);
   Result:=-1;
end;

// IndexOfString
//
function TDataContext.IndexOfString(const item : String; fromIndex : NativeInt) : NativeInt;
var
   i : NativeInt;
   varData : PVarData;
begin
   if fromIndex<DataLength then begin
      varData:=@AsPData^[fromIndex];
      for i:=fromIndex to DataLength-1 do begin
         {$ifdef FPC}
         Assert(varData^.VType=varString);
         if String(varData^.VString)=item then
            Exit(i);
         {$else}
         Assert(varData^.VType=varUString);
         if String(varData^.VUString)=item then
            Exit(i);
         {$endif}
         Inc(varData);
      end;
   end;
   Result:=-1;
end;

// IndexOfInteger
//
function TDataContext.IndexOfInteger(const item : Int64; fromIndex : NativeInt) : NativeInt;
var
   i : NativeInt;
   varData : PVarData;
begin
   if fromIndex<DataLength then begin
      varData:=@AsPData^[fromIndex];
      for i:=fromIndex to DataLength-1 do begin
         Assert(varData^.VType=varInt64);
         if varData^.VInt64=item then
            Exit(i);
         Inc(varData);
      end;
   end;
   Result:=-1;
end;

// IndexOfFloat
//
function TDataContext.IndexOfFloat(const item : Double; fromIndex : NativeInt) : NativeInt;
var
   i : NativeInt;
   varData : PVarData;
begin
   if fromIndex<DataLength then begin
      varData:=@AsPData^[fromIndex];
      for i:=fromIndex to DataLength-1 do begin
         Assert(varData^.VType=varDouble);
         if varData^.VDouble=item then
            Exit(i);
         Inc(varData);
      end;
   end;
   Result:=-1;
end;

// ReplaceData
//
procedure TDataContext.ReplaceData(const newData : TData);
begin
   FData:=newData;
end;

// ClearData
//
procedure TDataContext.ClearData;
begin
   FData:=nil;
   FAddr:=0;
end;

// SetDataLength
//
procedure TDataContext.SetDataLength(n : NativeInt);
begin
   SetLength(FData, n);
end;

// IncInteger
//
function TDataContext.IncInteger(addr : NativeInt; delta : Int64) : Int64;
var
   p : PVarData;
begin
   p := @FData[FAddr+addr];
   Assert(p.VType = varInt64);
   Result := p.VInt64 + delta;
   p.VInt64 := Result;
end;

// HashCode
//
function TDataContext.HashCode(size : NativeInt) : Cardinal;
begin
   Result:=DWSHashCode(FData, FAddr, size);
end;

// ------------------
// ------------------ TRelativeDataContext ------------------
// ------------------

// Create
//
constructor TRelativeDataContext.Create(const getPData : TGetPDataFunc; addr : NativeInt);
begin
   FGetPData:=getPData;
   FAddr:=addr;
end;

// GetSelf
//
function TRelativeDataContext.GetSelf : TObject;
begin
   Result:=Self;
end;

// GetAsVariant
//
function TRelativeDataContext.GetAsVariant(addr : NativeInt) : Variant;
begin
   Result := FGetPData^[FAddr+addr];
end;

// SetAsVariant
//
procedure TRelativeDataContext.SetAsVariant(addr : NativeInt; const value : Variant);
begin
   FGetPData^[FAddr+addr] := value;
end;

// GetAsInteger
//
function TRelativeDataContext.GetAsInteger(addr : NativeInt) : Int64;
begin
   VariantToInt64( FGetPData^[FAddr+addr], Result );
end;

// SetAsInteger
//
procedure TRelativeDataContext.SetAsInteger(addr : NativeInt; const value : Int64);
begin
   VarCopySafe(FGetPData^[FAddr+addr], value);
end;

// GetAsFloat
//
function TRelativeDataContext.GetAsFloat(addr : NativeInt) : Double;
begin
   Result := FGetPData^[FAddr+addr];
end;

// SetAsFloat
//
procedure TRelativeDataContext.SetAsFloat(addr : NativeInt; const value : Double);
begin
   VarCopySafe(FGetPData^[FAddr+addr], value);
end;

// GetAsBoolean
//
function TRelativeDataContext.GetAsBoolean(addr : NativeInt) : Boolean;
begin
   Result := FGetPData^[FAddr+addr];
end;

// SetAsBoolean
//
procedure TRelativeDataContext.SetAsBoolean(addr : NativeInt; const value : Boolean);
begin
   VarCopySafe(FGetPData^[FAddr+addr], value);
end;

// GetAsString
//
function TRelativeDataContext.GetAsString(addr : NativeInt) : String;
begin
   Result := FGetPData^[FAddr+addr];
end;

// SetAsString
//
procedure TRelativeDataContext.SetAsString(addr : NativeInt; const value : String);
begin
   VarCopySafe(FGetPData^[FAddr+addr], value);
end;

// GetAsInterface
//
function TRelativeDataContext.GetAsInterface(addr : NativeInt) : IUnknown;
begin
   Result := FGetPData^[FAddr+addr];
end;

// SetAsInterface
//
procedure TRelativeDataContext.SetAsInterface(addr : NativeInt; const value : IUnknown);
begin
   VarCopySafe(FGetPData^[FAddr+addr], value);
end;

// Addr
//
function TRelativeDataContext.Addr : NativeInt;
begin
   Result := FAddr;
end;

// DataLength
//
function TRelativeDataContext.DataLength : NativeInt;
begin
   Result:=System.Length(FGetPData^);
end;

// AsPData
//
function TRelativeDataContext.AsPData : PData;
begin
   Result:=FGetPData;
end;

// CreateOffset
//
procedure TRelativeDataContext.CreateOffset(offset : NativeInt; var result : IDataContext);
begin
   Result:=TRelativeDataContext.Create(FGetPData, FAddr+offset);
end;

// EvalAsVariant
//
procedure TRelativeDataContext.EvalAsVariant(addr : NativeInt; var result : Variant);
begin
   VarCopySafe(result, FGetPData^[FAddr+addr]);
end;

// EvalAsString
//
procedure TRelativeDataContext.EvalAsString(addr : NativeInt; var result : String);
begin
   result := FGetPData^[FAddr+addr];
end;

// EvalAsInterface
//
procedure TRelativeDataContext.EvalAsInterface(addr : NativeInt; var result : IUnknown);
begin
   result := FGetPData^[FAddr+addr];
end;

// IsEmpty
//
function TRelativeDataContext.IsEmpty(addr : NativeInt) : Boolean;
begin
   Result := DWSVarIsEmpty(FGetPData^[FAddr+addr]);
end;

// VarType
//
function TRelativeDataContext.VarType(addr : NativeInt) : TVarType;
begin
   Result := VarType(FGetPData^[FAddr+addr]);
end;

// CopyData
//
procedure TRelativeDataContext.CopyData(const destData : TData; destAddr, size : NativeInt);
begin
   DWSCopyData(FGetPData^, FAddr, destData, destAddr, size);
end;

// WriteData
//
procedure TRelativeDataContext.WriteData(const src : IDataContext; size : NativeInt);
begin
   DWSCopyData(src.AsPData^, src.Addr, FGetPData^, FAddr, size);
end;

// WriteData
//
procedure TRelativeDataContext.WriteData(destAddr : NativeInt; const src : IDataContext; size : NativeInt);
begin
   DWSCopyData(src.AsPData^, src.Addr, FGetPData^, FAddr+destAddr, size);
end;

// WriteData
//
procedure TRelativeDataContext.WriteData(const srcData : TData; srcAddr, size : NativeInt);
begin
   DWSCopyData(srcData, srcAddr, FGetPData^, FAddr, size);
end;

// SameData
//
function TRelativeDataContext.SameData(addr : NativeInt; const otherData : TData; otherAddr, size : NativeInt) : Boolean;
begin
   Result:=DWSSameData(FGetPData^, otherData, FAddr+addr, otherAddr, size);
end;

// IncInteger
//
function TRelativeDataContext.IncInteger(addr : NativeInt; delta : Int64) : Int64;
var
   p : PVarData;
begin
   p := @FGetPData^[FAddr+addr];
   Assert(p.VType = varInt64);
   Result := p.VInt64 + delta;
   p.VInt64 := Result;
end;

// HashCode
//
function TRelativeDataContext.HashCode(size : NativeInt) : Cardinal;
begin
   Result:=DWSHashCode(FGetPData^, FAddr, size);
end;

end.
