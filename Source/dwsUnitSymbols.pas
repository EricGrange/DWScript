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
unit dwsUnitSymbols;

{$I dws.inc}

interface

uses
   SysUtils, Classes,
   dwsUtils, dwsSymbols, dwsErrors, dwsXPlatform,
   dwsStrings, dwsTokenTypes, dwsDataContext, dwsUnicode, dwsXXHash;

type

   TIdwsUnitFlag = (ufImplicitUse, ufOwnsSymbolTable);
   TIdwsUnitFlags = set of TIdwsUnitFlag;

   // Interface for units
   IdwsUnit = interface
      procedure BeforeAdditionTo(dwscript : TObject);
      function  GetSelf : TObject;
      function  GetUnitName : String;
      function  GetDependencies : TStringList;
      function  GetUnitFlags : TIdwsUnitFlags;
      function  GetDeprecatedMessage : String;
   end;

   TIdwsUnitList = class (TSimpleList<IdwsUnit>)
      public
         function IndexOfName(const unitName : String) : Integer;
         function IndexOf(const aUnit : IdwsUnit) : Integer;
         procedure AddUnits(list : TIdwsUnitList);
         function FindDuplicateUnitName : String;
   end;

   TSystemSymbolTable = class;
   TUnitSymbolTable = class;
   TUnitImplementationTable = class;
   TUnitMainSymbol = class;
   TUnitMainSymbols = class;
   TUnitSymbol = class;

   // Invisible symbol for source code
   TSourceSymbol = class (TSymbol)
      public
   end;

   TUnitMainSymbolArray = array of TUnitMainSymbol;

   // Invisible symbol for units (e. g. for TdwsUnit)
   TUnitMainSymbol = class sealed (TSourceSymbol)
      private
         FTable : TUnitSymbolTable;
         FInterfaceTable : TSymbolTable;
         FImplementationTable : TUnitImplementationTable;
         FStoredParents : TTightList;
         FInitializationRank : Integer;
         FInitializationExpr : TExprBase;
         FFinalizationExpr : TExprBase;
         FDeprecatedMessage : String;
         FDependencies : TUnitMainSymbolArray;

      public
         constructor Create(const name : String; table : TUnitSymbolTable;
                            unitSyms : TUnitMainSymbols);
         destructor Destroy; override;

         procedure Initialize(const msgs : TdwsCompileMessageList); override;

         procedure CreateInterfaceTable;
         procedure UnParentInterfaceTable;

         procedure StoreParents;
         procedure RestoreParents;

         function ReferenceInSymbolTable(aTable : TSymbolTable; implicit : Boolean) : TUnitSymbol;

         function HasSymbol(sym : TSymbol) : Boolean;

         procedure AddDependency(ums : TUnitMainSymbol);

         procedure AddInitializationExpr(anExpr : TExprBase);

         property Table : TUnitSymbolTable read FTable;

         property InterfaceTable : TSymbolTable read FInterfaceTable;
         property ImplementationTable : TUnitImplementationTable read FImplementationTable;

         property InitializationRank : Integer read FInitializationRank write FInitializationRank;
         property InitializationExpr : TExprBase read FInitializationExpr;
         property FinalizationExpr : TExprBase read FFinalizationExpr write FFinalizationExpr;
         property DeprecatedMessage : String read FDeprecatedMessage write FDeprecatedMessage;
         property Dependencies : TUnitMainSymbolArray read FDependencies;
   end;

   // list of unit main symbols (one per prog)
   TUnitMainSymbols = class(TObjectList<TUnitMainSymbol>)
      private

      protected

      public
         procedure Initialize(const msgs : TdwsCompileMessageList);

         function Find(const unitName : String) : TUnitMainSymbol;

         procedure CollectPublishedSymbols(symbolList : TSimpleSymbolList;
                                           ignoreImplementationPublished : Boolean);
   end;

   // TUnitSymbolTable
   //
   TUnitSymbolTable = class (TSymbolTable)
      private
         FUnitMainSymbol : TUnitMainSymbol;

      public
         class function IsUnitTable : Boolean; override;

         property UnitMainSymbol : TUnitMainSymbol read FUnitMainSymbol write FUnitMainSymbol;
   end;

   // TUnitPrivateTable
   //
   TUnitPrivateTable = class(TSymbolTable)
      private
         FUnitMainSymbol : TUnitMainSymbol;

      public
         constructor Create(unitMainSymbol : TUnitMainSymbol);

         class function IsUnitTable : Boolean; override;

         property UnitMainSymbol : TUnitMainSymbol read FUnitMainSymbol;
   end;

   // TUnitImplementationTable
   //
   TUnitImplementationTable = class(TUnitPrivateTable)
      public
         constructor Create(unitMainSymbol : TUnitMainSymbol);

         class function IsUnitTable : Boolean; override;

         function FindLocal(const aName : String) : TSymbol; override;
         function EnumerateHelpers(helpedType : TTypeSymbol; const callback : THelperSymbolEnumerationCallback) : Boolean; override;
   end;

   // Invisible symbol for included source code
   TIncludeSymbol = class (TSourceSymbol)
      public
         constructor Create(const fileName : String);
   end;

   TUnitSymbolProc = procedure (unitSymbol : TUnitSymbol) of object;

   // Front end for units, serves for explicit unit resolution "unitName.symbolName"
   TUnitSymbol = class sealed (TTypeSymbol)
      private
         FMain : TUnitMainSymbol;
         FNameSpace : TNameObjectHash;
         FImplicit : Boolean;

      public
         constructor Create(mainSymbol : TUnitMainSymbol; const name : String);
         destructor Destroy; override;

         procedure InitDataContext(const data : IDataContext; offset : NativeInt); override;

         procedure RegisterNameSpaceUnit(unitSymbol : TUnitSymbol);
         function  FindNameSpaceUnit(const name : String) : TUnitSymbol;
         function  PossibleNameSpace(const name : String) : Boolean;

         function IsDeprecated : Boolean;

         property Main : TUnitMainSymbol read FMain write FMain;
         property Implicit : Boolean read FImplicit write FImplicit;

         function HasNameSpace : Boolean;
         procedure EnumerateNameSpaceUnits(const proc : TUnitSymbolProc);

         function Table : TUnitSymbolTable; inline;
         function InterfaceTable : TSymbolTable; inline;
         function ImplementationTable : TUnitImplementationTable; inline;
   end;

   TUnitSymbolList = class(TObjectList<TUnitSymbol>);
   TUnitSymbolRefList = class(TUnitSymbolList)
      public
         destructor Destroy; override;
   end;

   // unit namespaces, aggregate unit symbols
   TUnitNamespaceSymbol = class (TSourceSymbol)
      private
         FUnitSymbols : TUnitSymbolList;

      public
         constructor Create(const name : String);
         destructor Destroy; override;

         property UnitSymbols : TUnitSymbolList read FUnitSymbols;
   end;

   TStaticSymbolTable = class;

   IStaticSymbolTable = interface
      function SymbolTable : TStaticSymbolTable;
   end;

   // TStaticSymbolTable
   //
   TStaticSymbolTable = class (TUnitSymbolTable, IStaticSymbolTable)
      private
         FRefCount : Integer;
         FInitialized : Boolean;

      protected
         function SymbolTable : TStaticSymbolTable; overload;

      public
         destructor Destroy; override;

         procedure Initialize(const msgs : TdwsCompileMessageList); override;
         procedure InsertParent(index : Integer; parent : TSymbolTable); override;
         function  RemoveParent(parent : TSymbolTable) : Integer; override;
         procedure ClearParents; override;

         function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID : TGUID; out Obj) : HResult; stdcall;
         function _AddRef : Integer; stdcall;
         function _Release : Integer; stdcall;
   end;

   // TLinkedSymbolTable
   //
   TLinkedSymbolTable = class sealed (TUnitSymbolTable)
      private
         FParent : IStaticSymbolTable;
         FParentSymbolTable : TStaticSymbolTable;

      public
         constructor Create(const parent : IStaticSymbolTable);

         function FindLocal(const Name : String) : TSymbol; override;
         function FindSymbol(const Name : String; minVisibility : TdwsVisibility;
                              ofClass : TSymbolClass = nil) : TSymbol; override;
         procedure Initialize(const msgs : TdwsCompileMessageList); override;

         function EnumerateLocalSymbolsOfName(const aName : String;
                              const callback : TSymbolEnumerationCallback) : Boolean; override;
         function EnumerateSymbolsOfNameInScope(const aName : String;
                              const callback : TSymbolEnumerationCallback) : Boolean; override;

         function EnumerateLocalHelpers(helpedType : TTypeSymbol;
                              const callback : THelperSymbolEnumerationCallback) : Boolean; override;
         function EnumerateHelpers(helpedType : TTypeSymbol;
                              const callback : THelperSymbolEnumerationCallback) : Boolean; override;

         function EnumerateLocalOperatorsFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol;
                                             const callback : TOperatorSymbolEnumerationCallback) : Boolean; override;
         function EnumerateOperatorsFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol;
                                        const callback : TOperatorSymbolEnumerationCallback) : Boolean; override;
         function HasSameLocalOperator(anOpSym : TOperatorSymbol) : Boolean; override;

         property Parent : IStaticSymbolTable read FParent;
         property ParentSymbolTable : TStaticSymbolTable read FParentSymbolTable;
   end;

   ISystemSymbolTable = interface(IStaticSymbolTable)
      function SymbolTable : TSystemSymbolTable;
   end;

   // TSystemSymbolTable
   //
   TSystemSymbolTable = class (TStaticSymbolTable, ISystemSymbolTable)
      private
         FBaseSymbolTypes : TdwsBaseSymbolTypes;

      protected
         function SymbolTable : TSystemSymbolTable; overload;

      public
         destructor Destroy; override;

         property BaseSymbolTypes : TdwsBaseSymbolTypes read FBaseSymbolTypes;

         property TypInteger : TBaseIntegerSymbol read FBaseSymbolTypes.TypInteger write FBaseSymbolTypes.TypInteger;
         property TypBoolean : TBaseBooleanSymbol read FBaseSymbolTypes.TypBoolean write FBaseSymbolTypes.TypBoolean;
         property TypFloat : TBaseFloatSymbol read FBaseSymbolTypes.TypFloat write FBaseSymbolTypes.TypFloat;
         property TypString : TBaseStringSymbol read FBaseSymbolTypes.TypString write FBaseSymbolTypes.TypString;
         property TypVariant : TBaseVariantSymbol read FBaseSymbolTypes.TypVariant write FBaseSymbolTypes.TypVariant;

         property TypNil : TNilSymbol read FBaseSymbolTypes.TypNil write FBaseSymbolTypes.TypNil;
         property TypObject : TClassSymbol read FBaseSymbolTypes.TypObject write FBaseSymbolTypes.TypObject;
         property TypTObject : TClassSymbol read FBaseSymbolTypes.TypTObject write FBaseSymbolTypes.TypTObject;
         property TypClass : TClassOfSymbol read FBaseSymbolTypes.TypClass write FBaseSymbolTypes.TypClass;

         property TypException : TClassSymbol read FBaseSymbolTypes.TypException write FBaseSymbolTypes.TypException;

         property TypInterface : TInterfaceSymbol read FBaseSymbolTypes.TypInterface write FBaseSymbolTypes.TypInterface;

         property TypAnyType : TAnyTypeSymbol read FBaseSymbolTypes.TypAnyType write FBaseSymbolTypes.TypAnyType;
         property TypAnyFunc : TAnyFuncSymbol read FBaseSymbolTypes.TypAnyFunc write FBaseSymbolTypes.TypAnyFunc;

         property TypCustomAttribute : TClassSymbol read FBaseSymbolTypes.TypCustomAttribute write FBaseSymbolTypes.TypCustomAttribute;
   end;

   // TProgramSymbolTable
   //
   TProgramSymbolTable = class (TSymbolTable)
      private
         FDestructionList : TTightList;

      public
         destructor Destroy; override;

         procedure AddToDestructionList(sym : TSymbol);
         procedure RemoveFromDestructionList(sym : TSymbol);
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsExprs, dwsCoreExprs;

// ------------------
// ------------------ TIdwsUnitList ------------------
// ------------------

// IndexOf (name)
//
function TIdwsUnitList.IndexOfName(const unitName : String) : Integer;
begin
   for Result:=0 to Count-1 do
      if UnicodeSameText(Items[Result].GetUnitName, unitName) then
         Exit;
   Result:=-1;
end;

// AddUnits
//
procedure TIdwsUnitList.AddUnits(list : TIdwsUnitList);
var
   i : Integer;
begin
   for i:=0 to list.Count-1 do
      Add(list[i]);
end;

// FindDuplicateUnitName
//
function TIdwsUnitList.FindDuplicateUnitName : String;
var
   i : Integer;
begin
   // Check for duplicate unit names
   for i:=0 to Count-1 do begin
      Result:=Items[i].GetUnitName;
      if IndexOfName(Result)<>i then
         Exit;
   end;
   Result:='';
end;

// IndexOf (IdwsUnit)
//
function TIdwsUnitList.IndexOf(const aUnit : IdwsUnit) : Integer;
begin
   for Result:=0 to Count-1 do
      if Items[Result]=aUnit then
         Exit;
   Result:=-1;
end;

// ------------------
// ------------------ TStaticSymbolTable ------------------
// ------------------

// Destroy
//
destructor TStaticSymbolTable.Destroy;
begin
   Assert(FRefCount=0);
   ClearParents;
   inherited;
end;

// _AddRef
//
function TStaticSymbolTable._AddRef : Integer;
begin
   Result := AtomicIncrement(FRefCount);
end;

// _Release
//
function TStaticSymbolTable._Release : Integer;
begin
   Result := AtomicDecrement(FRefCount);
   if Result=0 then
      Destroy;
end;

// InsertParent
//
procedure TStaticSymbolTable.InsertParent(index : Integer; parent : TSymbolTable);
var
   staticTable : TStaticSymbolTable;
begin
   // accept only static parents
   if parent is TLinkedSymbolTable then
      staticTable:=TLinkedSymbolTable(parent).ParentSymbolTable
   else begin
      Assert((parent is TStaticSymbolTable), CPE_NoStaticSymbols);
      staticTable:=TStaticSymbolTable(parent);
   end;

   staticTable._AddRef;
   inherited InsertParent(index, staticTable);
end;

// RemoveParent
//
function TStaticSymbolTable.RemoveParent(parent : TSymbolTable) : Integer;
begin
   (parent as TStaticSymbolTable)._Release;
   Result:=inherited RemoveParent(parent);
end;

// ClearParents
//
procedure TStaticSymbolTable.ClearParents;
var
   i : Integer;
begin
   for i:=0 to ParentCount-1 do
      TStaticSymbolTable(Parents[i])._Release;
   inherited;
end;

// QueryInterface
//
function TStaticSymbolTable.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID : TGUID; out Obj) : HResult;
begin
   if GetInterface(IID, Obj) then
      Result:=0
   else Result:=E_NOINTERFACE;
end;

// Initialize
//
procedure TStaticSymbolTable.Initialize(const msgs : TdwsCompileMessageList);
begin
   if not FInitialized then begin
      inherited;
      FInitialized:=True;
   end;
end;

// SymbolTable
//
function TStaticSymbolTable.SymbolTable : TStaticSymbolTable;
begin
   Result:=Self;
end;

// ------------------
// ------------------ TLinkedSymbolTable ------------------
// ------------------

// Create
//
constructor TLinkedSymbolTable.Create(const parent : IStaticSymbolTable);
begin
   inherited Create(nil, nil);
   FParent:=parent;
   FParentSymbolTable:=parent.SymbolTable;
end;

function TLinkedSymbolTable.FindLocal(const Name : String) : TSymbol;
begin
   Result := FParentSymbolTable.FindLocal(Name);
   if not Assigned(Result) then
      Result := inherited FindLocal(Name);
end;

function TLinkedSymbolTable.FindSymbol(const Name : String; minVisibility : TdwsVisibility;
                                       ofClass : TSymbolClass = nil): TSymbol;
begin
  Result := FParentSymbolTable.FindSymbol(Name, minVisibility, ofClass);
  if not Assigned(Result) then
    Result := inherited FindSymbol(Name, minVisibility, ofClass);
end;

procedure TLinkedSymbolTable.Initialize(const msgs : TdwsCompileMessageList);
begin
  FParentSymbolTable.Initialize(msgs);
  inherited;
end;

// EnumerateLocalSymbolsOfName
//
function TLinkedSymbolTable.EnumerateLocalSymbolsOfName(const aName : String; const callback : TSymbolEnumerationCallback) : Boolean;
begin
   Result:=FParentSymbolTable.EnumerateLocalSymbolsOfName(aName, callback);
end;

// EnumerateSymbolsOfNameInScope
//
function TLinkedSymbolTable.EnumerateSymbolsOfNameInScope(const aName : String; const callback : TSymbolEnumerationCallback) : Boolean;
begin
   Result:=FParentSymbolTable.EnumerateSymbolsOfNameInScope(aName, callback);
end;

// EnumerateLocalHelpers
//
function TLinkedSymbolTable.EnumerateLocalHelpers(helpedType : TTypeSymbol;
            const callback : THelperSymbolEnumerationCallback) : Boolean;
begin
   Result:=FParentSymbolTable.EnumerateLocalHelpers(helpedType, callback);
end;

// EnumerateHelpers
//
function TLinkedSymbolTable.EnumerateHelpers(helpedType : TTypeSymbol;
            const callback : THelperSymbolEnumerationCallback) : Boolean;
begin
   Result:=FParentSymbolTable.EnumerateHelpers(helpedType, callback);
end;

// EnumerateLocalOperatorsFor
//
function TLinkedSymbolTable.EnumerateLocalOperatorsFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol;
                                             const callback : TOperatorSymbolEnumerationCallback) : Boolean;
begin
   Result:=FParentSymbolTable.EnumerateLocalOperatorsFor(aToken, aLeftType, aRightType, callback);
end;

// EnumerateOperatorsFor
//
function TLinkedSymbolTable.EnumerateOperatorsFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol;
                                         const callback : TOperatorSymbolEnumerationCallback) : Boolean;
begin
   Result:=FParentSymbolTable.EnumerateOperatorsFor(aToken, aLeftType, aRightType, callback);
end;

// HasSameLocalOperator
//
function TLinkedSymbolTable.HasSameLocalOperator(anOpSym : TOperatorSymbol) : Boolean;
begin
   Result:=FParentSymbolTable.HasSameLocalOperator(anOpSym);
end;

// ------------------
// ------------------ TUnitMainSymbol ------------------
// ------------------

// Create
//
constructor TUnitMainSymbol.Create(const name : String; table : TUnitSymbolTable;
                                   unitSyms : TUnitMainSymbols);
begin
   inherited Create(Name, nil);
   FTable:=Table;
   unitSyms.Add(Self);
end;

// Destroy
//
destructor TUnitMainSymbol.Destroy;
begin
   FInitializationExpr.Free;
   FFinalizationExpr.Free;
   FInterfaceTable.Free;
   FImplementationTable.Free;
   FTable.Free;
   FStoredParents.Clear;
   inherited;
end;

// Initialize
//
procedure TUnitMainSymbol.Initialize(const msgs : TdwsCompileMessageList);
begin
   FTable.Initialize(msgs);
   if FImplementationTable<>nil then
      FImplementationTable.Initialize(msgs);
end;

// CreateInterfaceTable
//
procedure TUnitMainSymbol.CreateInterfaceTable;
begin
   Assert(not Assigned(FInterfaceTable));

   FInterfaceTable:=TSymbolTable.Create;
   Table.AddParent(FInterfaceTable);
end;

// UnParentInterfaceTable
//
procedure TUnitMainSymbol.UnParentInterfaceTable;
begin
   Table.RemoveParent(FInterfaceTable);
end;

// StoreParents
//
procedure TUnitMainSymbol.StoreParents;
var
   i : Integer;
begin
   if Self=nil then Exit;
   Assert(FStoredParents.Count=0);
   Assert(Table.ParentCount>0);
   for i:=0 to Table.ParentCount-1 do
      FStoredParents.Add(Table.Parents[i]);
   Table.ClearParents;
end;

// RestoreParents
//
procedure TUnitMainSymbol.RestoreParents;
var
   sp : TRefCountedObject;
begin
   if Self=nil then Exit;
   if FStoredParents.Count=0 then Exit;
   Assert(Table.ParentCount=0);
   for sp in FStoredParents do
      Table.AddParent(TSymbolTable(sp));
   FStoredParents.Clear;
end;

// HasSymbol
//
function TUnitMainSymbol.HasSymbol(sym : TSymbol) : Boolean;
begin
   if Self=nil then
      Result:=False
   else Result:=Table.HasSymbol(sym) or ImplementationTable.HasSymbol(sym);
end;

// AddDependency
//
procedure TUnitMainSymbol.AddDependency(ums : TUnitMainSymbol);
var
   n : Integer;
begin
   n:=Length(FDependencies);
   SetLength(FDependencies, n+1);
   FDependencies[n]:=ums;
end;

// AddInitializationExpr
//
procedure TUnitMainSymbol.AddInitializationExpr(anExpr : TExprBase);
var
   block : TBlockExprNoTable;
begin
   if FInitializationExpr = nil then
      FInitializationExpr := anExpr
   else if FInitializationExpr is TBlockExprNoTable then
      TBlockExpr(FInitializationExpr).AddStatement(anExpr as TProgramExpr)
   else begin
      block := TBlockExprNoTable.Create(FInitializationExpr.ScriptPos);
      block.AddStatement(FInitializationExpr as TProgramExpr);
      block.AddStatement(anExpr as TProgramExpr);
      FInitializationExpr := block;
   end;
end;

// ReferenceInSymbolTable
//
function TUnitMainSymbol.ReferenceInSymbolTable(aTable : TSymbolTable; implicit : Boolean) : TUnitSymbol;
var
   p : Integer;
   nameSpace : TUnitSymbol;
   part : String;
begin
   p:=Pos('.', Name);
   if p>0 then
      part:=Copy(Name, 1, p-1)
   else part:=Name;

   nameSpace := TUnitSymbol(aTable.FindLocalOfClass(part, TUnitSymbol));
   if nameSpace=nil then begin
      nameSpace:=TUnitSymbol.Create(nil, part);
      nameSpace.Implicit:=implicit;
      aTable.AddSymbol(nameSpace);
   end;

   if p>0 then begin
      Result:=TUnitSymbol.Create(Self, Name);
      aTable.AddSymbol(Result);
      nameSpace.RegisterNameSpaceUnit(Result);
   end else begin
      Result:=nameSpace;
      Assert((nameSpace.Main=nil) or (nameSpace.Main=Self));
      nameSpace.Main:=Self;
   end;

   aTable.InsertParent(0, Table);
end;

// ------------------
// ------------------ TIncludeSymbol ------------------
// ------------------

// Create
//
constructor TIncludeSymbol.Create(const fileName : String);
begin
   inherited Create('$i '+fileName, nil);
end;

// ------------------
// ------------------ TUnitMainSymbols ------------------
// ------------------

// Find
//
function TUnitMainSymbols.Find(const unitName : String) : TUnitMainSymbol;
var
   i : Integer;
begin
   for i:=0 to Count-1 do begin
      Result:=Items[i];
      if UnicodeSameText(Result.Name, unitName) then
         Exit(Result);
   end;
   Result:=nil;
end;

// CollectPublishedSymbols
//
procedure TUnitMainSymbols.CollectPublishedSymbols(symbolList : TSimpleSymbolList;
                                                   ignoreImplementationPublished : Boolean);
var
   i : Integer;
   ums : TUnitMainSymbol;
begin
   for i:=0 to Count-1 do begin
      ums:=items[i];
      if ums.Table<>nil then
         ums.Table.CollectPublishedSymbols(symbolList);
      if (not ignoreImplementationPublished) and (ums.ImplementationTable<>nil) then
         ums.ImplementationTable.CollectPublishedSymbols(symbolList);
   end;
end;

// Initialize
//
procedure TUnitMainSymbols.Initialize(const msgs : TdwsCompileMessageList);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Items[i].Initialize(msgs);
end;

// ------------------
// ------------------ TUnitSymbolTable ------------------
// ------------------

// IsUnitTable
//
class function TUnitSymbolTable.IsUnitTable : Boolean;
begin
   Result:=True;
end;

// ------------------
// ------------------ TUnitSymbol ------------------
// ------------------

// Create
//
constructor TUnitSymbol.Create(mainSymbol : TUnitMainSymbol; const name : String);
begin
   inherited Create(name, nil);
   FMain:=mainSymbol;
end;

// Destroy
//
destructor TUnitSymbol.Destroy;
begin
   inherited;
   FNameSpace.Free;
end;

// InitDataContext
//
procedure TUnitSymbol.InitDataContext(const data : IDataContext; offset : NativeInt);
begin
   // nothing
end;

// RegisterNameSpaceUnit
//
procedure TUnitSymbol.RegisterNameSpaceUnit(unitSymbol : TUnitSymbol);
var
   lcName : String;
begin
   if FNameSpace=nil then begin
      FNameSpace := TNameObjectHash.Create;
   end;
   UnicodeLowerCase(unitSymbol.Name, lcName);
   FNameSpace[lcName] := unitSymbol;
end;

// FindNameSpaceUnit
//
function TUnitSymbol.FindNameSpaceUnit(const name : String) : TUnitSymbol;

   function FindInNameSpace : TUnitSymbol;
   var
      lcName : String;
   begin
      UnicodeLowerCase(name, lcName);
      Result := TUnitSymbol(FNameSpace[lcName]);
   end;

begin
   Result:=nil;
   if (Main<>nil) and UnicodeSameText(name, Self.Name) then
      Result:=Self
   else if FNameSpace<>nil then begin
      Result := FindInNameSpace;
   end;
end;

// PossibleNameSpace
//
function TUnitSymbol.PossibleNameSpace(const name : String) : Boolean;
var
   i, lenCandidate, lenName : Integer;
   bucket : PNameObjectHashBucket;
begin
   if FNameSpace=nil then Exit(False);
   lenName := Length(name);
   for i := 0 to FNameSpace.HighIndex do begin
      bucket := FNameSpace.Bucket[i];
      if bucket^.HashCode = 0 then Continue;
      lenCandidate := Length(bucket^.Name);
      if lenCandidate >=  lenName then begin
         if UnicodeCompareLen(Pointer(name), Pointer(bucket^.Name), lenName) = 0 then
            Exit(True);
      end;
   end;
   Result := False;
end;

// IsDeprecated
//
function TUnitSymbol.IsDeprecated : Boolean;
begin
   Result:=(Main.DeprecatedMessage<>'');
end;

// HasNameSpace
//
function TUnitSymbol.HasNameSpace : Boolean;
begin
   Result := (FNameSpace<>nil);
end;

// EnumerateNameSpaceUnits
//
procedure TUnitSymbol.EnumerateNameSpaceUnits(const proc : TUnitSymbolProc);
var
   i : Integer;
   unitSym : TUnitSymbol;
begin
   if FNameSpace = nil then Exit;
   for i := 0 to FNameSpace.HighIndex do begin
      unitSym := TUnitSymbol(FNameSpace.BucketObject[i]);
      if unitSym <> nil then
         proc(unitSym);
   end;
end;

// Table
//
function TUnitSymbol.Table : TUnitSymbolTable;
begin
   if Main<>nil then
      Result:=Main.Table
   else Result:=nil;
end;

// InterfaceTable
//
function TUnitSymbol.InterfaceTable : TSymbolTable;
begin
   if Main<>nil then
      Result:=Main.InterfaceTable
   else Result:=nil;
end;

// ImplementationTable
//
function TUnitSymbol.ImplementationTable : TUnitImplementationTable;
begin
   if Main<>nil then
      Result:=Main.ImplementationTable
   else Result:=nil;
end;

// ------------------
// ------------------ TUnitPrivateTable ------------------
// ------------------

// Create
//
constructor TUnitPrivateTable.Create(unitMainSymbol : TUnitMainSymbol);
begin
   inherited Create(unitMainSymbol.Table, unitMainSymbol.Table.AddrGenerator);
   FUnitMainSymbol:=unitMainSymbol;
end;

// IsUnitTable
//
class function TUnitPrivateTable.IsUnitTable : Boolean;
begin
   Result:=True;
end;

// ------------------
// ------------------ TUnitImplementationTable ------------------
// ------------------

// Create
//
constructor TUnitImplementationTable.Create(unitMainSymbol : TUnitMainSymbol);
begin
   inherited Create(unitMainSymbol);
   unitMainSymbol.FImplementationTable:=Self;
   AddParent(unitMainSymbol.InterfaceTable);
end;

// IsUnitTable
//
class function TUnitImplementationTable.IsUnitTable : Boolean;
begin
   Result:=False;
end;

// FindLocal
//
function TUnitImplementationTable.FindLocal(const aName : String) : TSymbol;
begin
   Result:=inherited FindLocal(aName);
   if Result=nil then
      Result:=UnitMainSymbol.Table.FindLocal(aName);
end;

// EnumerateHelpers
//
function TUnitImplementationTable.EnumerateHelpers(helpedType : TTypeSymbol; const callback : THelperSymbolEnumerationCallback) : Boolean;
begin
   Result:=UnitMainSymbol.Table.EnumerateHelpers(helpedType, callback);
   if not Result then
      Result:=inherited EnumerateHelpers(helpedType, callback);
end;

// ------------------
// ------------------ TSystemSymbolTable ------------------
// ------------------

// Destroy
//
destructor TSystemSymbolTable.Destroy;
begin
   FBaseSymbolTypes.TypNil.Free;
   inherited;
end;

// SymbolTable
//
function TSystemSymbolTable.SymbolTable : TSystemSymbolTable;
begin
   Result:=Self;
end;

// ------------------
// ------------------ TProgramSymbolTable ------------------
// ------------------

// Destroy
//
destructor TProgramSymbolTable.Destroy;
begin
   inherited;
   FDestructionList.Clean;
end;

// AddToDestructionList
//
procedure TProgramSymbolTable.AddToDestructionList(sym : TSymbol);
begin
   FDestructionList.Add(sym);
end;

// RemoveFromDestructionList
//
procedure TProgramSymbolTable.RemoveFromDestructionList(sym : TSymbol);
begin
   FDestructionList.Remove(sym);
end;

// ------------------
// ------------------ TUnitNamespaceSymbol ------------------
// ------------------

// Create
//
constructor TUnitNamespaceSymbol.Create(const name : String);
begin
   inherited Create(name, nil);
   FUnitSymbols:=TUnitSymbolList.Create;
end;

// Destroy
//
destructor TUnitNamespaceSymbol.Destroy;
begin
   FUnitSymbols.Free;
   inherited;
end;

// ------------------
// ------------------ TUnitSymbolRefList ------------------
// ------------------

// Destroy
//
destructor TUnitSymbolRefList.Destroy;
begin
   ExtractAll;
   inherited;
end;

end.
