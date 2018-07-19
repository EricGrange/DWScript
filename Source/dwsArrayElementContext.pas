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

         function ComputeReadAddr(addr : Integer) : Integer; inline;
         function ComputeWriteAddr(addr : Integer) : Integer; inline;

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
         function AsPVariant(addr : Integer) : PVariant;

         procedure CreateOffset(offset : Integer; var result : IDataContext);

         procedure EvalAsVariant(addr : Integer; var result : Variant);
         procedure EvalAsString(addr : Integer; var result : String);
         procedure EvalAsInterface(addr : Integer; var result : IUnknown);

         procedure CopyData(const destData : TData; destAddr, size : Integer);
         procedure WriteData(const src : IDataContext; size : Integer); overload;
         procedure WriteData(destAddr : Integer; const src : IDataContext; size : Integer); overload;
         procedure WriteData(const srcData : TData; srcAddr, size : Integer); overload;
         function SameData(addr : Integer; const otherData : TData; otherAddr, size : Integer) : Boolean;

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

// ComputeReadAddr
//
function TArrayElementDataContext.ComputeReadAddr(addr : Integer) : Integer;
begin
   Assert(Cardinal(addr) < Cardinal(FElementSize));
   if FIndex >= FArray.ArrayLength then
      raise EScriptError.CreateFmt(RTE_ArrayUpperBoundExceeded, [FIndex]);
   Result := FBase + addr;
end;

// ComputeWriteAddr
//
function TArrayElementDataContext.ComputeWriteAddr(addr : Integer) : Integer;
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
   Result := FArray.AsVariant[ComputeReadAddr(addr)];
end;

// SetAsVariant
//
procedure TArrayElementDataContext.SetAsVariant(addr : Integer; const value : Variant);
begin
   FArray.AsVariant[ComputeWriteAddr(addr)] := value;
end;

// GetAsInteger
//
function TArrayElementDataContext.GetAsInteger(addr : Integer) : Int64;
begin
   Result := FArray.AsInteger[ComputeReadAddr(addr)];
end;

// SetAsInteger
//
procedure TArrayElementDataContext.SetAsInteger(addr : Integer; const value : Int64);
begin
   FArray.AsInteger[ComputeWriteAddr(addr)] := value;
end;

// GetAsFloat
//
function TArrayElementDataContext.GetAsFloat(addr : Integer) : Double;
begin
   Result := FArray.AsFloat[ComputeReadAddr(addr)];
end;

// SetAsFloat
//
procedure TArrayElementDataContext.SetAsFloat(addr : Integer; const value : Double);
begin
   FArray.AsFloat[ComputeWriteAddr(addr)] := value;
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
   FArray.AsBoolean[ComputeWriteAddr(addr)] := value;
end;

// GetAsString
//
function TArrayElementDataContext.GetAsString(addr : Integer) : String;
begin
   Result := FArray.AsString[ComputeReadAddr(addr)];
end;

// SetAsString
//
procedure TArrayElementDataContext.SetAsString(addr : Integer; const value : String);
begin
   FArray.AsString[ComputeWriteAddr(addr)] := value;
end;

// GetAsInterface
//
function TArrayElementDataContext.GetAsInterface(addr : Integer) : IUnknown;
begin
   Result := FArray.AsInterface[ComputeReadAddr(addr)];
end;

// SetAsInterface
//
procedure TArrayElementDataContext.SetAsInterface(addr : Integer; const value : IUnknown);
begin
   FArray.AsInterface[ComputeWriteAddr(addr)] := value;
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

// AsPVariant
//
function TArrayElementDataContext.AsPVariant(addr : Integer) : PVariant;
begin
   Result := FArray.AsPVariant(ComputeReadAddr(addr));
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
   FArray.EvalAsVariant(ComputeReadAddr(addr), result);
end;

// EvalAsString
//
procedure TArrayElementDataContext.EvalAsString(addr : Integer; var result : String);
begin
   FArray.EvalAsString(ComputeReadAddr(addr), result);
end;

// EvalAsInterface
//
procedure TArrayElementDataContext.EvalAsInterface(addr : Integer; var result : IUnknown);
begin
   FArray.EvalAsInterface(ComputeReadAddr(addr), result);
end;

// CopyData
//
procedure TArrayElementDataContext.CopyData(const destData : TData; destAddr, size : Integer);
begin
   raise Exception.Create('TArrayElementDataContext.CopyData not implemented');
end;

// WriteData
//
procedure TArrayElementDataContext.WriteData(const src : IDataContext; size : Integer);
begin
   DWSCopyPVariants(src.AsPVariant(0), FArray.AsPVariant(ComputeWriteAddr(0)), size);
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

// HashCode
//
function TArrayElementDataContext.HashCode(size : Integer) : Cardinal;
begin
   Result := DWSHashCode(FArray.AsPVariant(0), size);
end;

end.
