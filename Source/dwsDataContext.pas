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
      function GetAsVariant(addr : Integer) : Variant;
      procedure SetAsVariant(addr : Integer; const value : Variant);
      function GetAsInteger(addr : Integer) : Int64;
      procedure SetAsInteger(addr : Integer; const value : Int64);
      function GetAsFloat(addr : Integer) : Double;
      procedure SetAsFloat(addr : Integer; const value : Double);
      function GetAsBoolean(addr : Integer) : Boolean;
      procedure SetAsBoolean(addr : Integer; const value : Boolean);
      function GetAsString(addr : Integer) : String;
      procedure SetAsString(addr : Integer; const value : String);
      function GetAsInterface(addr : Integer) : IUnknown;
      procedure SetAsInterface(addr : Integer; const value : IUnknown);

      function Addr : Integer;
      function DataLength : Integer;

      property AsVariant[addr : Integer] : Variant read GetAsVariant write SetAsVariant; default;

      function AsPVarDataArray : PVarDataArray;
      function AsPData : PData;
      function AsData : TData;
      function AsPVariant(addr : Integer) : PVariant;

      procedure CreateOffset(offset : Integer; var result : IDataContext);

      property  AsInteger[addr : Integer] : Int64 read GetAsInteger write SetAsInteger;
      property  AsBoolean[addr : Integer] : Boolean read GetAsBoolean write SetAsBoolean;
      property  AsFloat[addr : Integer] : Double read GetAsFloat write SetAsFloat;
      property  AsString[addr : Integer] : String read GetAsString write SetAsString;
      property  AsInterface[addr : Integer] : IUnknown read GetAsInterface write SetAsInterface;

      procedure EvalAsVariant(addr : Integer; var result : Variant);
      procedure EvalAsString(addr : Integer; var result : String);
      procedure EvalAsInterface(addr : Integer; var result : IUnknown);

      procedure CopyData(const destData : TData; destAddr, size : Integer);
      procedure WriteData(const src : IDataContext; size : Integer); overload;
      procedure WriteData(const srcData : TData; srcAddr, size : Integer); overload;
      function SameData(addr : Integer; const otherData : TData; otherAddr, size : Integer) : Boolean; overload;

      function  HashCode(size : Integer) : Cardinal;
   end;

   TDataContext = class;

   IDataContextPool = interface
      function Create(const aData : TData; anAddr : Integer) : TDataContext;
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

         function CreateData(const aData : TData; anAddr : Integer) : TDataContext;
         function CreateOffset(offset : Integer; ref : TDataContext) : TDataContext;

         function IDataContextPool.Create = CreateData;
   end;
   PDataPtrPool= ^TDataContextPool;

   TDataContext = class(TInterfacedObject, IDataContext, IGetSelf)
      private
         FAddr : Integer;
         FData : TData;
         FNext : TDataContext;
         FPool : TDataContextPool;
         FAllNext : TDataContext;
{$IFDEF DELPHI_2010_MINUS}
      protected // D2009 needs protected here to "see" these methods in inherited classes
{$ENDIF}
         function GetAsVariant(addr : Integer) : Variant; inline;
         procedure SetAsVariant(addr : Integer; const value : Variant); inline;
         function GetAsInteger(addr : Integer) : Int64; inline;
         procedure SetAsInteger(addr : Integer; const value : Int64); inline;
         function GetAsFloat(addr : Integer) : Double; inline;
         procedure SetAsFloat(addr : Integer; const value : Double); inline;
         function GetAsBoolean(addr : Integer) : Boolean; inline;
         procedure SetAsBoolean(addr : Integer; const value : Boolean); inline;
         function GetAsString(addr : Integer) : String; inline;
         procedure SetAsString(addr : Integer; const value : String); inline;
         function GetAsInterface(addr : Integer) : IUnknown; inline;
         procedure SetAsInterface(addr : Integer; const value : IUnknown); inline;

         function _AddRef: Integer; stdcall;
         function _Release: Integer; stdcall;

      protected
         property DirectData : TData read FData;

      public
         constructor CreateStandalone(size : Integer);

         function GetSelf : TObject;

         property AsVariant[addr : Integer] : Variant read GetAsVariant write SetAsVariant; default;
         function AsPVarDataArray : PVarDataArray; inline;
         function AsPData : PData; inline;
         function AsData : TData;
         function AsPVariant(addr : Integer) : PVariant; inline;
         function Addr : Integer;
         function DataLength : Integer; inline;
         procedure Offset(delta : Integer); inline;

         procedure CreateOffset(offset : Integer; var result : IDataContext);

         procedure EvalAsVariant(addr : Integer; var result : Variant); inline;
         procedure EvalAsString(addr : Integer; var result : String); inline;
         procedure EvalAsInterface(addr : Integer; var result : IUnknown); inline;

         property  AsInteger[addr : Integer] : Int64 read GetAsInteger write SetAsInteger;
         property  AsBoolean[addr : Integer] : Boolean read GetAsBoolean write SetAsBoolean;
         property  AsFloat[addr : Integer] : Double read GetAsFloat write SetAsFloat;
         property  AsString[addr : Integer] : String read GetAsString write SetAsString;
         property  AsInterface[addr : Integer] : IUnknown read GetAsInterface write SetAsInterface;

         procedure InternalCopyData(destAddr, sourceAddr, size : Integer); inline;
         procedure CopyData(const destData : TData; destAddr, size : Integer); inline;
         procedure WriteData(const src : IDataContext; size : Integer); overload; inline;
         procedure WriteData(destAddr : Integer; const src : IDataContext; size : Integer); overload; inline;
         procedure WriteData(const srcData : TData; srcAddr, size : Integer); overload; inline;
         function  SameData(addr : Integer; const otherData : TData; otherAddr, size : Integer) : Boolean; overload; inline;
         function  SameData(addr : Integer; const otherData : IDataContext; size : Integer) : Boolean; overload; inline;

         procedure ReplaceData(const newData : TData); virtual;
         procedure ClearData; virtual;
         procedure SetDataLength(n : Integer);

         function  HashCode(size : Integer) : Cardinal;
   end;

   TGetPDataFunc = function : PData of object;

   TRelativeDataContext = class (TInterfacedObject, IDataContext, IGetSelf)
      private
         FGetPData : TGetPDataFunc;
         FAddr : Integer;

      public
         constructor Create(const getPData : TGetPDataFunc; addr : Integer);

         function GetSelf : TObject;

         function GetAsVariant(addr : Integer) : Variant;
         procedure SetAsVariant(addr : Integer; const value : Variant);
         function GetAsInteger(addr : Integer) : Int64;
         procedure SetAsInteger(addr : Integer; const value : Int64);
         function GetAsFloat(addr : Integer) : Double;
         procedure SetAsFloat(addr : Integer; const value : Double);
         function GetAsBoolean(addr : Integer) : Boolean;
         procedure SetAsBoolean(addr : Integer; const value : Boolean);
         function GetAsString(addr : Integer) : String;
         procedure SetAsString(addr : Integer; const value : String);
         function GetAsInterface(addr : Integer) : IUnknown;
         procedure SetAsInterface(addr : Integer; const value : IUnknown);

         function Addr : Integer;
         function DataLength : Integer;

         function AsPVarDataArray : PVarDataArray;
         function AsPData : PData;
         function AsData : TData;
         function AsPVariant(addr : Integer) : PVariant;

         procedure CreateOffset(offset : Integer; var result : IDataContext);

         procedure EvalAsVariant(addr : Integer; var result : Variant);
         procedure EvalAsString(addr : Integer; var result : String);
         procedure EvalAsInterface(addr : Integer; var result : IUnknown);

         procedure CopyData(const destData : TData; destAddr, size : Integer);
         procedure WriteData(const src : IDataContext; size : Integer); overload;
         procedure WriteData(const srcData : TData; srcAddr, size : Integer); overload;
         function SameData(addr : Integer; const otherData : TData; otherAddr, size : Integer) : Boolean; overload;

         function  HashCode(size : Integer) : Cardinal;
   end;

procedure DWSCopyData(const sourceData : TData; sourceAddr : Integer;
                      const destData : TData; destAddr : Integer; size : Integer);
function DWSSameData(const data1, data2 : TData; offset1, offset2, size : Integer) : Boolean; overload;
function DWSSameData(const data1, data2 : TData) : Boolean; overload;
function DWSSameVariant(const v1, v2 : Variant) : Boolean;

procedure DWSHashCode(var partial : Cardinal; const v : Variant); overload;
function DWSHashCode(const v : Variant) : Cardinal; overload;
function DWSHashCode(const data : TData; offset, size : Integer) : Cardinal; overload;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// DWSCopyData
//
procedure DWSCopyData(const sourceData: TData; sourceAddr: Integer;
                      const destData: TData; destAddr: Integer; size: Integer);
var
   src, dest : PVariant;
begin
   src := @sourceData[sourceAddr];
   dest := @destData[destAddr];
   while size > 0 do begin
      VarCopySafe(dest^, src^);
      Inc(src);
      Inc(dest);
      Dec(size);
   end;
end;

// DWSSameData
//
function DWSSameData(const data1, data2 : TData; offset1, offset2, size : Integer) : Boolean;
var
   i : Integer;
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
   s : Integer;
begin
   s:=Length(data1);
   Result:=(s=Length(data2)) and DWSSameData(data1, data2, 0, 0, s);
end;

// DWSSameVariant
//
function DWSSameVariant(const v1, v2 : Variant) : Boolean;
var
   vt : Integer;
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
   buf : Cardinal;
   k : PByte;
   n : Integer;
   p : PVarData;
begin
   p := @v;
   k := @p.VByte;
   case p.VType of
      varByte, varBoolean, varShortInt : // 8 bits
         n := 1;
      varSmallint, varWord :  // 16 bits
         n := 2;
      varInteger, varSingle, varLongWord, varUnknown, varDispatch : // 32 bits
         n := 4;
      varInt64, varDouble, varCurrency, varDate, varUInt64 : begin // 64 bits
         n := 8;
      end;
      {$ifndef FPC}
      varUString : begin
         if p.VString <> nil then begin
            buf := SimpleStringHash(String(p.VString));
            k := @buf;
         end else buf := 0;
         n := 4;
      end;
      {$endif}
      varString : begin
         if p.VString <> nil then begin
            k := p.VString;
            n := Length(AnsiString(p.VString));
         end else n := 4;
      end;
      varOleStr : begin
         if p.VOleStr <> nil then begin
            buf := SimpleStringHash(p.VOleStr, Length(p.VOleStr));
            k := @buf;
         end;
         n := 4;
      end;
   else
      k := @p.VType;
      n := 1;
   end;
   for n:=n downto 1 do begin
      partial := (partial xor k^)*16777619;
      Inc(k);
   end;
end;

function DWSHashCode(const v : Variant) : Cardinal;
begin
   Result := 2166136261;
   DWSHashCode(Result, v);
end;

function DWSHashCode(const data : TData; offset, size : Integer) : Cardinal;
var
   i : Integer;
begin
   Result := 2166136261;
   for i := offset to offset+size-1 do
      DWSHashCode(Result, data[i]);
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
function TDataContextPool.CreateData(const aData : TData; anAddr : Integer) : TDataContext;
begin
   Result:=Pop;
   Result.FAddr:=anAddr;
   Result.FData:=aData;
end;

// CreateOffset
//
function TDataContextPool.CreateOffset(offset : Integer; ref : TDataContext) : TDataContext;
begin
   Result:=Pop;
   Result.FAddr:=ref.FAddr+offset;
   Result.FData:=ref.FData;
end;

// ------------------
// ------------------ TDataContext ------------------
// ------------------

// _AddRef
//
function TDataContext._AddRef: Integer;
begin
   Result := InterlockedIncrement(FRefCount);
end;

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
constructor TDataContext.CreateStandalone(size : Integer);
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
function TDataContext.GetAsVariant(addr : Integer) : Variant;
begin
   Result:=FData[FAddr+addr];
end;

// SetAsVariant
//
procedure TDataContext.SetAsVariant(addr : Integer; const value : Variant);
begin
   VarCopySafe(FData[FAddr+addr], value);
end;

// GetAsInteger
//
function TDataContext.GetAsInteger(addr : Integer) : Int64;
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
procedure TDataContext.SetAsInteger(addr : Integer; const value : Int64);
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
function TDataContext.GetAsFloat(addr : Integer) : Double;
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
procedure TDataContext.SetAsFloat(addr : Integer; const value : Double);
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
function TDataContext.GetAsBoolean(addr : Integer) : Boolean;
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
procedure TDataContext.SetAsBoolean(addr : Integer; const value : Boolean);
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
function TDataContext.GetAsString(addr : Integer) : String;
begin
   EvalAsString(addr, Result);
end;

// SetAsString
//
procedure TDataContext.SetAsString(addr : Integer; const value : String);
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
function TDataContext.GetAsInterface(addr : Integer) : IUnknown;
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
procedure TDataContext.SetAsInterface(addr : Integer; const value : IUnknown);
var
   p : PVarData;
begin
   p:=@FData[FAddr+addr];
   if p^.VType=varUnknown then
      IUnknown(p^.VUnknown):=value
   else VarCopySafe(PVariant(p)^, value);
end;

// AsPVarDataArray
//
function TDataContext.AsPVarDataArray : PVarDataArray;
begin
   Result:=@FData[FAddr];
end;

// AsPData
//
function TDataContext.AsPData : PData;
begin
   Result:=@FData;
end;

// AsData
//
function TDataContext.AsData : TData;
begin
   Result:=FData;
end;

// AsPVariant
//
function TDataContext.AsPVariant(addr : Integer) : PVariant;
begin
   Result:=@FData[FAddr+addr];
end;

// Addr
//
function TDataContext.Addr : Integer;
begin
   Result:=FAddr;
end;

// DataLength
//
function TDataContext.DataLength : Integer;
begin
   Result:=System.Length(FData);
end;

// Offset
//
procedure TDataContext.Offset(delta : Integer);
begin
   Inc(FAddr, delta);
end;

// CreateOffset
//
procedure TDataContext.CreateOffset(offset : Integer; var result : IDataContext);

   function CreateData(context : TDataContext; addr : Integer) : TDataContext;
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
procedure TDataContext.EvalAsVariant(addr : Integer; var result : Variant);
begin
   VarCopySafe(result, FData[FAddr+addr]);
end;

// EvalAsString
//
procedure TDataContext.EvalAsString(addr : Integer; var result : String);
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
procedure TDataContext.EvalAsInterface(addr : Integer; var result : IUnknown);
var
   p : PVarData;
begin
   p:=@FData[FAddr+addr];
   if p^.VType=varUnknown then
      result:=IUnknown(p^.VUnknown)
   else result:=PVariant(p)^;
end;

// InternalCopyData
//
procedure TDataContext.InternalCopyData(destAddr, sourceAddr, size : Integer);
var
   i : Integer;
begin
   if sourceAddr > destAddr then begin
      for i := 0 to size-1 do
         VarCopySafe(FData[destAddr+i], FData[sourceAddr+i])
   end else begin
      for i := size-1 downto 0 do
         VarCopySafe(FData[destAddr+i], FData[sourceAddr+i])
   end;
end;

// CopyData
//
procedure TDataContext.CopyData(const destData : TData; destAddr, size : Integer);
begin
   DWSCopyData(FData, FAddr, destData, destAddr, size);
end;

// WriteData
//
procedure TDataContext.WriteData(const src : IDataContext; size : Integer);
begin
   DWSCopyData(src.AsPData^, src.Addr, FData, FAddr, size);
end;

// WriteData
//
procedure TDataContext.WriteData(destAddr : Integer; const src : IDataContext; size : Integer);
begin
   DWSCopyData(src.AsPData^, src.Addr, FData, FAddr+destAddr, size);
end;

// WriteData
//
procedure TDataContext.WriteData(const srcData : TData; srcAddr, size : Integer);
begin
   DWSCopyData(srcData, srcAddr, FData, FAddr, size);
end;

// SameData
//
function TDataContext.SameData(addr : Integer; const otherData : TData; otherAddr, size : Integer) : Boolean;
begin
   Result:=DWSSameData(FData, otherData, FAddr+addr, otherAddr, size);
end;

// SameData
//
function TDataContext.SameData(addr : Integer; const otherData : IDataContext; size : Integer) : Boolean;
begin
   Result:=DWSSameData(FData, otherData.AsPData^, FAddr+addr, otherData.Addr, size);
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
procedure TDataContext.SetDataLength(n : Integer);
begin
   SetLength(FData, n);
end;

// HashCode
//
function TDataContext.HashCode(size : Integer) : Cardinal;
begin
   Result:=DWSHashCode(FData, FAddr, size);
end;

// ------------------
// ------------------ TRelativeDataContext ------------------
// ------------------

// Create
//
constructor TRelativeDataContext.Create(const getPData : TGetPDataFunc; addr : Integer);
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
function TRelativeDataContext.GetAsVariant(addr : Integer) : Variant;
begin
   Result := FGetPData^[FAddr+addr];
end;

// SetAsVariant
//
procedure TRelativeDataContext.SetAsVariant(addr : Integer; const value : Variant);
begin
   FGetPData^[FAddr+addr] := value;
end;

// GetAsInteger
//
function TRelativeDataContext.GetAsInteger(addr : Integer) : Int64;
begin
   VariantToInt64( FGetPData^[FAddr+addr], Result );
end;

// SetAsInteger
//
procedure TRelativeDataContext.SetAsInteger(addr : Integer; const value : Int64);
begin
   VarCopySafe(FGetPData^[FAddr+addr], value);
end;

// GetAsFloat
//
function TRelativeDataContext.GetAsFloat(addr : Integer) : Double;
begin
   Result := FGetPData^[FAddr+addr];
end;

// SetAsFloat
//
procedure TRelativeDataContext.SetAsFloat(addr : Integer; const value : Double);
begin
   VarCopySafe(FGetPData^[FAddr+addr], value);
end;

// GetAsBoolean
//
function TRelativeDataContext.GetAsBoolean(addr : Integer) : Boolean;
begin
   Result := FGetPData^[FAddr+addr];
end;

// SetAsBoolean
//
procedure TRelativeDataContext.SetAsBoolean(addr : Integer; const value : Boolean);
begin
   VarCopySafe(FGetPData^[FAddr+addr], value);
end;

// GetAsString
//
function TRelativeDataContext.GetAsString(addr : Integer) : String;
begin
   Result := FGetPData^[FAddr+addr];
end;

// SetAsString
//
procedure TRelativeDataContext.SetAsString(addr : Integer; const value : String);
begin
   VarCopySafe(FGetPData^[FAddr+addr], value);
end;

// GetAsInterface
//
function TRelativeDataContext.GetAsInterface(addr : Integer) : IUnknown;
begin
   Result := FGetPData^[FAddr+addr];
end;

// SetAsInterface
//
procedure TRelativeDataContext.SetAsInterface(addr : Integer; const value : IUnknown);
begin
   VarCopySafe(FGetPData^[FAddr+addr], value);
end;

// Addr
//
function TRelativeDataContext.Addr : Integer;
begin
   Result := FAddr;
end;

// DataLength
//
function TRelativeDataContext.DataLength : Integer;
begin
   Result:=System.Length(FGetPData^);
end;

// AsPVarDataArray
//
function TRelativeDataContext.AsPVarDataArray : PVarDataArray;
begin
   Result:=@FGetPData^[FAddr];
end;

// AsPData
//
function TRelativeDataContext.AsPData : PData;
begin
   Result:=FGetPData;
end;

// AsData
//
function TRelativeDataContext.AsData : TData;
begin
   Result:=FGetPData^;
end;

// AsPVariant
//
function TRelativeDataContext.AsPVariant(addr : Integer) : PVariant;
begin
   Result:=@FGetPData^[FAddr+addr];
end;

// CreateOffset
//
procedure TRelativeDataContext.CreateOffset(offset : Integer; var result : IDataContext);
begin
   Result:=TRelativeDataContext.Create(FGetPData, FAddr+offset);
end;

// EvalAsVariant
//
procedure TRelativeDataContext.EvalAsVariant(addr : Integer; var result : Variant);
begin
   VarCopySafe(result, FGetPData^[FAddr+addr]);
end;

// EvalAsString
//
procedure TRelativeDataContext.EvalAsString(addr : Integer; var result : String);
begin
   result := FGetPData^[FAddr+addr];
end;

// EvalAsInterface
//
procedure TRelativeDataContext.EvalAsInterface(addr : Integer; var result : IUnknown);
begin
   result := FGetPData^[FAddr+addr];
end;

// CopyData
//
procedure TRelativeDataContext.CopyData(const destData : TData; destAddr, size : Integer);
begin
   DWSCopyData(FGetPData^, FAddr, destData, destAddr, size);
end;

// WriteData
//
procedure TRelativeDataContext.WriteData(const src : IDataContext; size : Integer);
begin
   DWSCopyData(src.AsPData^, src.Addr, FGetPData^, FAddr, size);
end;

// WriteData
//
procedure TRelativeDataContext.WriteData(const srcData : TData; srcAddr, size : Integer);
begin
   DWSCopyData(srcData, srcAddr, FGetPData^, FAddr, size);
end;

// SameData
//
function TRelativeDataContext.SameData(addr : Integer; const otherData : TData; otherAddr, size : Integer) : Boolean;
begin
   Result:=DWSSameData(FGetPData^, otherData, FAddr+addr, otherAddr, size);
end;

// HashCode
//
function TRelativeDataContext.HashCode(size : Integer) : Cardinal;
begin
   Result:=DWSHashCode(FGetPData^, FAddr, size);
end;

end.
