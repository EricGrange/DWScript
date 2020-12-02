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
{    Eric Grange                                                       }
{                                                                      }
{**********************************************************************}
unit dwsSmartLink;

interface

uses Classes, SysUtils, dwsUtils, dwsSymbols, dwsExprs, dwsCoreExprs,
   dwsStrings, dwsUnitSymbols;

   // smart-linking support classes and utilities

type

   TdwsSmartLinkSymbol = class
      private
         FSymbol : TSymbol;
         FReferences : TTightList;

      protected
         function GetReference(idx : Integer) : TSymbol; inline;

      public
         constructor Create(aSymbol : TSymbol);
         destructor Destroy; override;

         procedure AddLinkTo(toSymbol : TSymbol);

         property Symbol : TSymbol read FSymbol write FSymbol;
         property Reference[idx : Integer] : TSymbol read GetReference;
         property ReferenceCount : Integer read FReferences.FCount;
   end;

   TdwsSmartLinkSymbolHash = class(TSimpleHash<TdwsSmartLinkSymbol>)
      private
         FLookup : TdwsSmartLinkSymbol;

      protected
         function SameItem(const item1, item2 : TdwsSmartLinkSymbol) : Boolean; override;
         function GetItemHashCode(const item1 : TdwsSmartLinkSymbol) : Integer; override;

      public
         constructor Create;
         destructor Destroy; override;

         function FindOrCreate(symbol : TSymbol) : TdwsSmartLinkSymbol;
   end;

   TdwsSmartLink = class
      private
         FProg : IdwsProgram;
         FScanStack : TSimpleStack<TSymbol>;
         FSymbols : TdwsSmartLinkSymbolHash;

      protected
         procedure ScanProg;
         procedure ScanContext(aContext : TdwsSourceContext);

      public
         constructor Create(const aProg : IdwsProgram);
         destructor Destroy; override;

         function SymbolIsUsed(const sym : TSymbol) : Boolean;

         property Prog : IdwsProgram read FProg;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsSmartLinkSymbolHash ------------------
// ------------------

// Create
//
constructor TdwsSmartLinkSymbolHash.Create;
begin
   inherited;
   FLookup:=TdwsSmartLinkSymbol.Create(nil);
end;

// Destroy
//
destructor TdwsSmartLinkSymbolHash.Destroy;
begin
   inherited;
   FLookup.Free;
end;

// FindOrCreate
//
function TdwsSmartLinkSymbolHash.FindOrCreate(symbol : TSymbol) : TdwsSmartLinkSymbol;
begin
   FLookup.Symbol:=symbol;
   if not Match(Result) then begin
      Result:=TdwsSmartLinkSymbol.Create(symbol);
      Add(Result);
   end;
end;

// SameItem
//
function TdwsSmartLinkSymbolHash.SameItem(const item1, item2 : TdwsSmartLinkSymbol) : Boolean;
begin
   Result:=(item1.Symbol=item2.Symbol);
end;

// GetItemHashCode
//
function TdwsSmartLinkSymbolHash.GetItemHashCode(const item1 : TdwsSmartLinkSymbol) : Integer;
begin
   Result := SimplePointerHash(item1.Symbol);
end;

// ------------------
// ------------------ TdwsSmartLinkSymbol ------------------
// ------------------

// Create
//
constructor TdwsSmartLinkSymbol.Create(aSymbol : TSymbol);
begin
   inherited Create;
   FSymbol:=aSymbol;
end;

// Destroy
//
destructor TdwsSmartLinkSymbol.Destroy;
begin
   inherited;
   FReferences.Free;
end;

// AddLinkTo
//
procedure TdwsSmartLinkSymbol.AddLinkTo(toSymbol : TSymbol);
begin
   FReferences.Add(toSymbol);
end;

// GetReference
//
function TdwsSmartLinkSymbol.GetReference(idx : Integer) : TSymbol;
begin
   Result:=TSymbol(FReferences.List[idx]);
end;

// ------------------
// ------------------ TdwsSmartLink ------------------
// ------------------

// Create
//
constructor TdwsSmartLink.Create(const aProg : IdwsProgram);
begin
   inherited Create;
   FScanStack:=TSimpleStack<TSymbol>.Create;
   FSymbols:=TdwsSmartLinkSymbolHash.Create;
   FProg:=aProg;
end;

// Destroy
//
destructor TdwsSmartLink.Destroy;
begin
   FSymbols.Free;
   FScanStack.Free;
   inherited;
end;

// SymbolIsUsed
//
function TdwsSmartLink.SymbolIsUsed(const sym : TSymbol) : Boolean;
begin
   Result:=(Self<>nil);
end;

// ScanProg
//
procedure TdwsSmartLink.ScanProg;
var
   i : Integer;
   symDict : TdwsSymbolDictionary;
   symPos : TSymbolPositionList;
   symMap : TdwsSourceContextMap;
begin
   symDict:=FProg.SymbolDictionary;
   for i:=0 to symDict.Count do begin
      symPos:=symDict.Items[i];
      FSymbols.FindOrCreate(symPos.Symbol);
   end;

   symMap:=FProg.SourceContextMap;
   for i:=0 to symMap.Count-1 do
      ScanContext(symMap.Context[i]);
end;

// ScanContext
//
procedure TdwsSmartLink.ScanContext(aContext : TdwsSourceContext);
var
   i : Integer;
begin
   if aContext.ParentSym<>nil then
      FScanStack.Push(aContext.ParentSym);

//   FProg.SymbolDictionary.EnumerateInRange(

   for i:=0 to aContext.Count-1 do
      ScanContext(aContext.SubContext[i]);

   if aContext.ParentSym<>nil then
      FScanStack.Pop;
end;

end.
