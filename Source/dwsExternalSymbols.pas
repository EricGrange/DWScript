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
unit dwsExternalSymbols;

{$I dws.inc}

interface

uses
   dwsUtils,
   dwsExprs, dwsSymbols;

type

   IExternalSymbolHandler = interface
      ['{9217DE55-C4A6-40F4-99FC-3186967B96B5}']
      procedure Assign(exec : TdwsExecution; symbol : TDataSymbol; expr : TTypedExpr; var handled : Boolean);
      procedure Eval(exec : TdwsExecution; symbol : TDataSymbol; var handled : Boolean; var result : Variant);
   end;

   TExternalSymbolHandler = class (TInterfacedSelfObject, IExternalSymbolHandler)
      public
         procedure Assign(exec : TdwsExecution; symbol : TDataSymbol; expr : TTypedExpr; var handled : Boolean); virtual;
         procedure Eval(exec : TdwsExecution; symbol : TDataSymbol; var handled : Boolean; var result : Variant); virtual;

         class procedure Register(symbol : TSymbol; const handler : IExternalSymbolHandler); static;

         class procedure HandleAssign(exec : TdwsExecution; symbol : TDataSymbol; expr : TTypedExpr; var handled : Boolean); static;
         class procedure HandleEval(exec : TdwsExecution; symbol : TDataSymbol; var handled : Boolean; var result : Variant); static;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   TSymbolHandlers = class (TRefCountedObject)
      FSymbol : TSymbol;
      FHandlers : TSimpleList<IExternalSymbolHandler>;
      destructor Destroy; override;
   end;
   TSymbolHandlersList = class (TSortedList<TSymbolHandlers>)
      function Compare(const item1, item2 : TSymbolHandlers) : Integer; override;
      function Find(symbol : TSymbol) : TSymbolHandlers;
   end;

var
   vSearch : TSymbolHandlers;
   vRegisteredHandlers : TSymbolHandlersList;

// ------------------
// ------------------ TSymbolHandlers ------------------
// ------------------

// Destroy
//
destructor TSymbolHandlers.Destroy;
begin
   inherited;
   FHandlers.Free;
end;

// ------------------
// ------------------ TSymbolHandlersList ------------------
// ------------------

// Compare
//
function TSymbolHandlersList.Compare(const item1, item2 : TSymbolHandlers) : Integer;
begin
   if NativeUInt(item1.FSymbol)>NativeUInt(item2.FSymbol) then
      Result:=1
   else if NativeUInt(item1.FSymbol)<NativeUInt(item2.FSymbol) then
      Result:=-1
   else Result:=0;
end;

// Find
//
function TSymbolHandlersList.Find(symbol : TSymbol) : TSymbolHandlers;
var
   lo, hi, mid : Integer;
begin
   lo:=0;
   hi:=Count-1;
   while lo<=hi do begin
      mid:=(lo+hi) shr 1;
      Result:=GetItem(mid);
      if NativeUInt(Result.FSymbol)<NativeUInt(symbol) then
         lo:=mid+1
      else begin
         hi:=mid- 1;
         if NativeUInt(Result.FSymbol)=NativeUInt(symbol) then
            Exit;
      end;
   end;
   Result:=nil;
end;

// ------------------
// ------------------ TExternalSymbolHandler ------------------
// ------------------

// Assign
//
procedure TExternalSymbolHandler.Assign(exec : TdwsExecution; symbol : TDataSymbol; expr : TTypedExpr; var handled : Boolean);
begin
   handled:=False;
end;

// Eval
//
procedure TExternalSymbolHandler.Eval(exec : TdwsExecution; symbol : TDataSymbol; var handled : Boolean; var result : Variant);
begin
   handled:=False;
end;

// Register
//
class procedure TExternalSymbolHandler.Register(symbol : TSymbol; const handler : IExternalSymbolHandler);
var
   i : Integer;
   h : TSymbolHandlers;
begin
   h:=TSymbolHandlers.Create;
   h.FSymbol:=symbol;
   i:=vRegisteredHandlers.IndexOf(h);
   if i>=0 then begin
      h.Free;
      h:=vRegisteredHandlers[i];
   end else begin
      vRegisteredHandlers.Add(h);
      h.FHandlers:=TSimpleList<IExternalSymbolHandler>.Create;
   end;
   h.FHandlers.Add(handler);
end;

// HandleAssign
//
class procedure TExternalSymbolHandler.HandleAssign(exec : TdwsExecution; symbol : TDataSymbol; expr : TTypedExpr; var handled : Boolean);
var
   h : TSymbolHandlers;
   i : Integer;
begin
   h:=vRegisteredHandlers.Find(symbol);
   if h<>nil then begin
      for i:=0 to h.FHandlers.Count-1 do begin
         h.FHandlers[i].Assign(exec, symbol, expr, handled);
         if handled then exit;
      end;
   end else begin
      handled:=False;
   end;
end;

// HandleEval
//
class procedure TExternalSymbolHandler.HandleEval(exec : TdwsExecution; symbol : TDataSymbol; var handled : Boolean; var result : Variant);
var
   h : TSymbolHandlers;
   i : Integer;
begin
   h:=vRegisteredHandlers.Find(symbol);
   if h<>nil then begin
      for i:=0 to h.FHandlers.Count-1 do begin
         h.FHandlers[i].Eval(exec, symbol, handled, result);
         if handled then exit;
      end;
   end else begin
      handled:=False;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   vRegisteredHandlers:=TSymbolHandlersList.Create;
   vSearch:=TSymbolHandlers.Create;

finalization

   vSearch.Free;
   vRegisteredHandlers.Clean;
   vRegisteredHandlers.Free;

end.
