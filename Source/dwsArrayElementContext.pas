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
         FIndex : Integer;
         FElementSize : Integer;
         FBase : Integer;

      protected
         function GetSelf : TObject;

         function ComputeAddr(addr : Integer) : Integer; inline;

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

         function AsPData : PData;

         procedure CreateOffset(offset : Integer; var result : IDataContext);

         procedure EvalAsVariant(addr : Integer; var result : Variant);
         procedure EvalAsString(addr : Integer; var result : String);
         procedure EvalAsInterface(addr : Integer; var result : IUnknown);

         function IsEmpty(addr : Integer) : Boolean;
         function VarType(addr : Integer) : TVarType;

         procedure CopyData(const destData : TData; destAddr, size : Integer);
         procedure WriteData(const src : IDataContext; size : Integer); overload;
         procedure WriteData(destAddr : Integer; const src : IDataContext; size : Integer); overload;
         procedure WriteData(const srcData : TData; srcAddr, size : Integer); overload;
         function SameData(addr : Integer; const otherData : TData; otherAddr, size : Integer) : Boolean;

         function  IncInteger(addr : Integer; delta : Int64) : Int64;

         function  HashCode(size : Integer) : Cardinal;

      public
         constructor Create(const anArray : IScriptDynArray; anIndex : Integer);

   end;


implementation

// ------------------
// ------------------ TArrayElementDataContext ------------------
// ------------------

// Create
//
constructor TArrayElementDataContext.Create(const anArray : IScriptDynArray; anIndex : Integer);
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
function TArrayElementDataContext.ComputeAddr(addr : Integer) : Integer;
begin
   Assert(Cardinal(addr) < Cardinal(FElementSize));
   if FIndex >= FArray.ArrayLength then
      raise EScriptError.CreateFmt(RTE_ArrayUpperBoundExceeded, [FIndex]);
   Result := FBase + addr;
end;

// GetAsVariant
//
function TArrayElementDataContext.GetAsVariant(addr : Integer) : Variant;
begin
   FArray.EvalAsVariant(ComputeAddr(addr), Result);
end;

// SetAsVariant
//
procedure TArrayElementDataContext.SetAsVariant(addr : Integer; const value : Variant);
begin
   FArray.SetAsVariant(ComputeAddr(addr), value);
end;

// GetAsInteger
//
function TArrayElementDataContext.GetAsInteger(addr : Integer) : Int64;
begin
   Result := FArray.AsInteger[ComputeAddr(addr)];
end;

// SetAsInteger
//
procedure TArrayElementDataContext.SetAsInteger(addr : Integer; const value : Int64);
begin
   FArray.AsInteger[ComputeAddr(addr)] := value;
end;

// GetAsFloat
//
function TArrayElementDataContext.GetAsFloat(addr : Integer) : Double;
begin
   Result := FArray.AsFloat[ComputeAddr(addr)];
end;

// SetAsFloat
//
procedure TArrayElementDataContext.SetAsFloat(addr : Integer; const value : Double);
begin
   FArray.AsFloat[ComputeAddr(addr)] := value;
end;

// GetAsBoolean
//
function TArrayElementDataContext.GetAsBoolean(addr : Integer) : Boolean;
begin
   Result := FArray.AsBoolean[FIndex];
end;

// SetAsBoolean
//
procedure TArrayElementDataContext.SetAsBoolean(addr : Integer; const value : Boolean);
begin
   FArray.AsBoolean[ComputeAddr(addr)] := value;
end;

// GetAsString
//
function TArrayElementDataContext.GetAsString(addr : Integer) : String;
begin
   FArray.EvalAsString(ComputeAddr(addr), Result);
end;

// SetAsString
//
procedure TArrayElementDataContext.SetAsString(addr : Integer; const value : String);
begin
   FArray.SetAsString(ComputeAddr(addr), value);
end;

// GetAsInterface
//
function TArrayElementDataContext.GetAsInterface(addr : Integer) : IUnknown;
begin
   FArray.EvalAsInterface(ComputeAddr(addr), Result);
end;

// SetAsInterface
//
procedure TArrayElementDataContext.SetAsInterface(addr : Integer; const value : IUnknown);
begin
   FArray.SetAsInterface(ComputeAddr(addr), value);
end;

// Addr
//
function TArrayElementDataContext.Addr : Integer;
begin
   Result := 0;
end;

// DataLength
//
function TArrayElementDataContext.DataLength : Integer;
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
procedure TArrayElementDataContext.CreateOffset(offset : Integer; var result : IDataContext);
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
procedure TArrayElementDataContext.EvalAsVariant(addr : Integer; var result : Variant);
begin
   FArray.EvalAsVariant(ComputeAddr(addr), result);
end;

// EvalAsString
//
procedure TArrayElementDataContext.EvalAsString(addr : Integer; var result : String);
begin
   FArray.EvalAsString(ComputeAddr(addr), result);
end;

// EvalAsInterface
//
procedure TArrayElementDataContext.EvalAsInterface(addr : Integer; var result : IUnknown);
begin
   FArray.EvalAsInterface(ComputeAddr(addr), result);
end;

// IsEmpty
//
function TArrayElementDataContext.IsEmpty(addr : Integer) : Boolean;
begin
   Result := FArray.IsEmpty(ComputeAddr(addr));
end;

// VarType
//
function TArrayElementDataContext.VarType(addr : Integer) : TVarType;
begin
   Result := FArray.VarType(ComputeAddr(addr));
end;

// CopyData
//
procedure TArrayElementDataContext.CopyData(const destData : TData; destAddr, size : Integer);
var
   i : Integer;
begin
   for i := 0 to size-1 do
      FArray.EvalAsVariant(ComputeAddr(i), destData[destAddr+i]);
end;

// WriteData
//
procedure TArrayElementDataContext.WriteData(const src : IDataContext; size : Integer);
var
   p, i : Integer;
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
procedure TArrayElementDataContext.WriteData(destAddr : Integer; const src : IDataContext; size : Integer);
begin
   raise Exception.Create('TArrayElementDataContext.WriteData(2) not implemented');
end;

// WriteData
//
procedure TArrayElementDataContext.WriteData(const srcData : TData; srcAddr, size : Integer);
begin
   raise Exception.Create('TArrayElementDataContext.WriteData(3) not implemented');
end;

// SameData
//
function TArrayElementDataContext.SameData(addr : Integer; const otherData : TData; otherAddr, size : Integer) : Boolean;
begin
   raise Exception.Create('TArrayElementDataContext.SameData not implemented');
end;

// IncInteger
//
function TArrayElementDataContext.IncInteger(addr : Integer; delta : Int64) : Int64;
begin
   addr := ComputeAddr(addr);
   Result := FArray.AsInteger[addr] + delta;
   FArray.AsInteger[addr] := Result;
end;

// HashCode
//
function TArrayElementDataContext.HashCode(size : Integer) : Cardinal;
begin
   Result := FArray.HashCode(ComputeAddr(0), size);
end;

end.
