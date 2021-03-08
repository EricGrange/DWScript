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
unit dwsArrayElementContext;

{$I dws.inc}

interface

uses
   Classes, SysUtils,
   dwsSymbols, dwsDataContext, dwsErrors, dwsStrings, dwsUtils;

type
   TArrayElementDataContext = class (TInterfacedObject, IDataContext)
      private
         FArray : IScriptDynArray;
         FIndex : NativeInt;
         FElementSize : Integer;
         FBase : NativeInt;

      protected
         function GetSelf : TObject;

         function ComputeAddr(addr : NativeInt) : NativeInt; inline;

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
         function SameData(addr : NativeInt; const otherData : TData; otherAddr, size : NativeInt) : Boolean;

         function  IncInteger(addr : NativeInt; delta : Int64) : Int64;

         function  HashCode(size : NativeInt) : Cardinal;

      public
         constructor Create(const anArray : IScriptDynArray; anIndex : NativeInt);

   end;


implementation

// ------------------
// ------------------ TArrayElementDataContext ------------------
// ------------------

// Create
//
constructor TArrayElementDataContext.Create(const anArray : IScriptDynArray; anIndex : NativeInt);
begin
   inherited Create;
   if FIndex < 0 then
      raise EScriptError.CreateFmt(RTE_ArrayLowerBoundExceeded, [anIndex]);
   FArray := anArray;
   FIndex := anIndex;
   FElementSize := anArray.ElementSize;
   FBase := FIndex*FElementSize;
end;

// GetSelf
//
function TArrayElementDataContext.GetSelf : TObject;
begin
   Result := Self;
end;

// ComputeAddr
//
function TArrayElementDataContext.ComputeAddr(addr : NativeInt) : NativeInt;
begin
   Assert(Cardinal(addr) < Cardinal(FElementSize));
   if FIndex >= FArray.ArrayLength then
      raise EScriptError.CreateFmt(RTE_ArrayUpperBoundExceeded, [FIndex]);
   Result := FBase + addr;
end;

// GetAsVariant
//
function TArrayElementDataContext.GetAsVariant(addr : NativeInt) : Variant;
begin
   FArray.EvalAsVariant(ComputeAddr(addr), Result);
end;

// SetAsVariant
//
procedure TArrayElementDataContext.SetAsVariant(addr : NativeInt; const value : Variant);
begin
   FArray.SetAsVariant(ComputeAddr(addr), value);
end;

// GetAsInteger
//
function TArrayElementDataContext.GetAsInteger(addr : NativeInt) : Int64;
begin
   Result := FArray.AsInteger[ComputeAddr(addr)];
end;

// SetAsInteger
//
procedure TArrayElementDataContext.SetAsInteger(addr : NativeInt; const value : Int64);
begin
   FArray.AsInteger[ComputeAddr(addr)] := value;
end;

// GetAsFloat
//
function TArrayElementDataContext.GetAsFloat(addr : NativeInt) : Double;
begin
   Result := FArray.AsFloat[ComputeAddr(addr)];
end;

// SetAsFloat
//
procedure TArrayElementDataContext.SetAsFloat(addr : NativeInt; const value : Double);
begin
   FArray.AsFloat[ComputeAddr(addr)] := value;
end;

// GetAsBoolean
//
function TArrayElementDataContext.GetAsBoolean(addr : NativeInt) : Boolean;
begin
   Result := FArray.AsBoolean[FIndex];
end;

// SetAsBoolean
//
procedure TArrayElementDataContext.SetAsBoolean(addr : NativeInt; const value : Boolean);
begin
   FArray.AsBoolean[ComputeAddr(addr)] := value;
end;

// GetAsString
//
function TArrayElementDataContext.GetAsString(addr : NativeInt) : String;
begin
   FArray.EvalAsString(ComputeAddr(addr), Result);
end;

// SetAsString
//
procedure TArrayElementDataContext.SetAsString(addr : NativeInt; const value : String);
begin
   FArray.SetAsString(ComputeAddr(addr), value);
end;

// GetAsInterface
//
function TArrayElementDataContext.GetAsInterface(addr : NativeInt) : IUnknown;
begin
   FArray.EvalAsInterface(ComputeAddr(addr), Result);
end;

// SetAsInterface
//
procedure TArrayElementDataContext.SetAsInterface(addr : NativeInt; const value : IUnknown);
begin
   FArray.SetAsInterface(ComputeAddr(addr), value);
end;

// Addr
//
function TArrayElementDataContext.Addr : NativeInt;
begin
   Result := 0;
end;

// DataLength
//
function TArrayElementDataContext.DataLength : NativeInt;
begin
   Result := FElementSize;
end;

// AsPData
//
function TArrayElementDataContext.AsPData : PData;
begin
   raise Exception.Create('TArrayElementDataContext.AsPData not implemented');
end;

// CreateOffset
//
procedure TArrayElementDataContext.CreateOffset(offset : NativeInt; var result : IDataContext);
var
   dc : TArrayElementDataContext;
begin
   Assert(offset < FElementSize);

   dc := TArrayElementDataContext.Create(FArray, FIndex);
   Inc(dc.FBase, offset);
   Dec(dc.FElementSize, offset);
   Result := dc;
end;

// EvalAsVariant
//
procedure TArrayElementDataContext.EvalAsVariant(addr : NativeInt; var result : Variant);
begin
   FArray.EvalAsVariant(ComputeAddr(addr), result);
end;

// EvalAsString
//
procedure TArrayElementDataContext.EvalAsString(addr : NativeInt; var result : String);
begin
   FArray.EvalAsString(ComputeAddr(addr), result);
end;

// EvalAsInterface
//
procedure TArrayElementDataContext.EvalAsInterface(addr : NativeInt; var result : IUnknown);
begin
   FArray.EvalAsInterface(ComputeAddr(addr), result);
end;

// IsEmpty
//
function TArrayElementDataContext.IsEmpty(addr : NativeInt) : Boolean;
begin
   Result := FArray.IsEmpty(ComputeAddr(addr));
end;

// VarType
//
function TArrayElementDataContext.VarType(addr : NativeInt) : TVarType;
begin
   Result := FArray.VarType(ComputeAddr(addr));
end;

// CopyData
//
procedure TArrayElementDataContext.CopyData(const destData : TData; destAddr, size : NativeInt);
var
   i : NativeInt;
begin
   for i := 0 to size-1 do
      FArray.EvalAsVariant(ComputeAddr(i), destData[destAddr+i]);
end;

// WriteData
//
procedure TArrayElementDataContext.WriteData(const src : IDataContext; size : NativeInt);
var
   p, i : NativeInt;
   v : Variant;
begin
   p := ComputeAddr(0);
   for i := 0 to size-1 do begin
      src.EvalAsVariant(i, v);
      FArray.SetAsVariant(p + i, v);
   end;
end;

// WriteData
//
procedure TArrayElementDataContext.WriteData(destAddr : NativeInt; const src : IDataContext; size : NativeInt);
begin
   raise Exception.Create('TArrayElementDataContext.WriteData(2) not implemented');
end;

// WriteData
//
procedure TArrayElementDataContext.WriteData(const srcData : TData; srcAddr, size : NativeInt);
begin
   raise Exception.Create('TArrayElementDataContext.WriteData(3) not implemented');
end;

// SameData
//
function TArrayElementDataContext.SameData(addr : NativeInt; const otherData : TData; otherAddr, size : NativeInt) : Boolean;
begin
   raise Exception.Create('TArrayElementDataContext.SameData not implemented');
end;

// IncInteger
//
function TArrayElementDataContext.IncInteger(addr : NativeInt; delta : Int64) : Int64;
begin
   addr := ComputeAddr(addr);
   Result := FArray.AsInteger[addr] + delta;
   FArray.AsInteger[addr] := Result;
end;

// HashCode
//
function TArrayElementDataContext.HashCode(size : NativeInt) : Cardinal;
begin
   Result := FArray.HashCode(ComputeAddr(0), size);
end;

end.
