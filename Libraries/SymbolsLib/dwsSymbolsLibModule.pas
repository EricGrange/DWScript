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
unit dwsSymbolsLibModule;

{$I dws.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, dwsComp, dwsExprs, dwsUtils,
  dwsUnitSymbols;

type
  TdwsSymbolsLib = class(TDataModule)
    dwsUnit: TdwsUnit;
    procedure dwsUnitClassesTSymbolsMethodsFirstEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTSymbolsMethodsLastEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTSymbolsMethodsNextEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTSymbolsMethodsPreviousEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTSymbolsMethodsEofEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTSymbolsMethodsCaptionEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTSymbolsMethodsDescriptionEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTSymbolsMethodsGetMembersEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTSymbolsMethodsSymbolTypeEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTSymbolsMethodsDestroyEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTSymbolsMethodsNameEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTSymbolsMethodsLocateEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTSymbolsMethodsGetSuperClassEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTSymbolsMethodsGetParametersEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTSymbolsConstructorsCreateMainEval(Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsUnitClassesTSymbolsConstructorsCreateUidEval(Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsUnitClassesTSymbolsConstructorsCreateUnitEval(Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsUnitClassesTSymbolsMethodsVisibilityEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTSymbolsMethodsQualifiedNameEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTSymbolsMethodsIsTypeEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTSymbolsMethodsMetaSymbolEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTSymbolsMethodsInternalClassNameEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTSymbolsMethodsTypeNameEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTSymbolsMethodsGetTypeEval(Info: TProgramInfo;
      ExtObject: TObject);
  private
    FScript: TDelphiWebScript;
    procedure SetScript(const Value: TDelphiWebScript);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property Script: TDelphiWebScript read FScript write SetScript;
  end;

var
  dwsSymbolsLib: TdwsSymbolsLib;

implementation

{$R *.DFM}

uses
   dwsSymbols;

resourcestring
   RStrUnitSNotFound = 'Unit "%s" not found!';

type
  TSymbols = class
  private
    FIndex: Integer;
    FCount: Integer;
    FTable: TSymbolTable;
    FCurrentSymbol: TSymbol;
  public
    constructor Create(Table: TSymbolTable); overload;
    constructor Create(Symbol: TSymbol); overload;

    class function CreateSymbolAsVariant(info : TProgramInfo; sym : TSymbol) : Variant;
    class function CreateTableAsVariant(info : TProgramInfo; table : TSymbolTable) : Variant;

    procedure SetIndex(Index: Integer);
    procedure SetSymbol(Symbol: TSymbol);
    function CurrentSymbol : TSymbol;
    property Count: Integer read FCount;
    property Index: Integer read FIndex;
  end;

const
   stUnknown = -1;
   stAlias = 0;
   stArray = 1;
   stClass = 2;
   stConstant = 3;
   stField = 4;
   stFunction = 5;
   stParam = 6;
   stProperty = 7;
   stRecord = 8;
   stUnit = 9;
   stVariable = 10;
   stInterface = 11;
   stEnumeration = 12;
   stMetaClass = 13;

{ TSymbols }

constructor TSymbols.Create(Table: TSymbolTable);
begin
   FTable := Table;
   FCount := Table.Count;
   SetIndex(0);
end;

constructor TSymbols.Create(Symbol: TSymbol);
begin
  SetSymbol(Symbol);
end;

// CreateSymbolAsVariant
//
class function TSymbols.CreateSymbolAsVariant(info : TProgramInfo; sym : TSymbol) : Variant;
var
   syms : TSymbols;
begin
   if sym <> nil then begin
      syms := TSymbols.Create(sym);
      Result := Info.Vars['TSymbols'].GetConstructor('Create', syms).Call.Value
   end else Result := IUnknown(nil);
end;

// CreateTableAsVariant
//
class function TSymbols.CreateTableAsVariant(info : TProgramInfo; table : TSymbolTable) : Variant;
var
   syms : TSymbols;
begin
   if table <> nil then begin
      syms := TSymbols.Create(table);
      Result := Info.Vars['TSymbols'].GetConstructor('Create', syms).Call.Value
   end else Result := IUnknown(nil);
end;

procedure TSymbols.SetIndex(Index: Integer);
begin
  FIndex := Index;
  if Assigned(FTable) and (Index >= 0) and (Index < FCount) then
    FCurrentSymbol := FTable.Symbols[Index]
  else
    FCurrentSymbol := nil;
end;

procedure TSymbols.SetSymbol(Symbol: TSymbol);
begin
  FTable := nil;
  FCount := 0;
  FCurrentSymbol := Symbol;
end;

// CurrentSymbol
//
function TSymbols.CurrentSymbol : TSymbol;
begin
   if (Self = nil) or (FCurrentSymbol = nil) then
      raise Exception.Create('No active symbol');
   Result := FCurrentSymbol;
end;

{ TdwsSymbolsLib }

procedure TdwsSymbolsLib.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Script) then
    SetScript(nil)
end;

procedure TdwsSymbolsLib.SetScript(const Value: TDelphiWebScript);
var
  x: Integer;
begin
  if Assigned(FScript) then
    FScript.RemoveFreeNotification(Self);
  if Assigned(Value) then
    Value.FreeNotification(Self);

  FScript := Value;
  for x := 0 to ComponentCount - 1 do
    if Components[x] is TdwsUnit then
      TdwsUnit(Components[x]).Script := Value;
end;

procedure TdwsSymbolsLib.dwsUnitClassesTSymbolsConstructorsCreateMainEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
   ExtObject := TSymbols.Create(Info.Execution.Prog.RootTable);
end;

procedure TdwsSymbolsLib.dwsUnitClassesTSymbolsConstructorsCreateUnitEval(
  Info: TProgramInfo; var ExtObject: TObject);
var
   sym: TSymbol;
   name : String;
begin
   name := Info.ParamAsString[0];
   sym := Info.Execution.Prog.RootTable.FindSymbol(name, cvMagic, TUnitSymbol);
   if Assigned(sym) and (sym is TUnitSymbol) then
      ExtObject := TSymbols.Create(TUnitSymbol(sym).Table)
   else raise Exception.CreateFmt(RStrUnitSNotFound, [ name ]);
end;

procedure TdwsSymbolsLib.dwsUnitClassesTSymbolsConstructorsCreateUidEval(
  Info: TProgramInfo; var ExtObject: TObject);
var
  table: TSymbolTable;
  sym: TSymbol;
  funcSym : TFuncSymbol;
  uid, name: String;
  p: Integer;
begin
  table := Info.Execution.Prog.RootTable;
  uid := Info.ValueAsString['Uid'];
  sym := nil;

  while Length(uid) > 0 do
  begin
    p := Pos('$', uid);
    if p = 0 then
      p := Length(uid) + 1;

    name := Copy(uid, 1, p - 1);
    Delete(uid, 1, p);

    sym := table.FindLocal(name);

    if Length(uid) = 0 then
      Break;

    if sym is TUnitSymbol then
      table := TUnitSymbol(sym).Table
    else if sym is TStructuredTypeSymbol then
      table := TClassSymbol(sym).Members
    else begin
      funcSym:=sym.AsFuncSymbol;
      if funcSym<>nil then
         table := funcSym.Params;
    end;
  end;

  ExtObject := TSymbols.Create(sym);
end;

procedure TdwsSymbolsLib.dwsUnitClassesTSymbolsMethodsFirstEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   TSymbols(ExtObject).SetIndex(0);
end;

procedure TdwsSymbolsLib.dwsUnitClassesTSymbolsMethodsLastEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   TSymbols(ExtObject).SetIndex(TSymbols(ExtObject).Count - 1);
end;

procedure TdwsSymbolsLib.dwsUnitClassesTSymbolsMethodsNextEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TSymbols(ExtObject).SetIndex(TSymbols(ExtObject).Index + 1);
end;

procedure TdwsSymbolsLib.dwsUnitClassesTSymbolsMethodsPreviousEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TSymbols(ExtObject).SetIndex(TSymbols(ExtObject).Index - 1);
end;

procedure TdwsSymbolsLib.dwsUnitClassesTSymbolsMethodsQualifiedNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := TSymbols(ExtObject).CurrentSymbol.QualifiedName;
end;

procedure TdwsSymbolsLib.dwsUnitClassesTSymbolsMethodsEofEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := not Assigned(TSymbols(ExtObject).FCurrentSymbol);
end;

procedure TdwsSymbolsLib.dwsUnitClassesTSymbolsMethodsCaptionEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TSymbols(ExtObject).CurrentSymbol.Caption;
end;

procedure TdwsSymbolsLib.dwsUnitClassesTSymbolsMethodsDescriptionEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TSymbols(ExtObject).CurrentSymbol.Description;
end;

procedure TdwsSymbolsLib.dwsUnitClassesTSymbolsMethodsGetMembersEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   sym : TSymbol;
   table : TSymbolTable;
begin
   sym := TSymbols(ExtObject).CurrentSymbol;
   if sym is TCompositeTypeSymbol then
      table := TCompositeTypeSymbol(sym).Members
   else if sym is TEnumerationSymbol then
      table := TEnumerationSymbol(sym).Elements
   else if sym is TUnitSymbol then
      table := TUnitSymbol(sym).Table
   else table := nil;

   Info.ResultAsVariant := TSymbols.CreateTableAsVariant(Info, table);
end;

procedure TdwsSymbolsLib.dwsUnitClassesTSymbolsMethodsSymbolTypeEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   sym : TSymbol;
   result : Integer;
begin
   sym := TSymbols(ExtObject).CurrentSymbol;
   result := stUnknown;

   if sym is TArraySymbol then
      result := stArray
   else if sym is TAliasSymbol then
      result := stAlias
   else if sym is TCompositeTypeSymbol then begin
      if sym is TClassSymbol then
         result := stClass
      else if sym is TInterfaceSymbol then
         result := stInterface
      else if sym is TRecordSymbol then
         result := stRecord
   end else if sym is TConstSymbol then
      result := stConstant
   else if sym is TFieldSymbol then
      result := stField
   else if sym.AsFuncSymbol<>nil then
      result := stFunction
   else if sym is TParamSymbol then
      result := stParam
   else if sym is TPropertySymbol then
      result := stProperty
   else if sym is TUnitSymbol then
      result := stUnit
   else if sym is TDataSymbol then
      result := stVariable
   else if sym is TEnumerationSymbol then
      result := stEnumeration
   else if sym is TStructuredTypeMetaSymbol then
      result := stMetaClass;

  Info.ResultAsInteger := result;
end;

procedure TdwsSymbolsLib.dwsUnitClassesTSymbolsMethodsTypeNameEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   typ : TTypeSymbol;
begin
   typ := TSymbols(ExtObject).CurrentSymbol.Typ;
   if typ <> nil then
      Info.ResultAsString := typ.Name
   else Info.ResultAsString := 'void';
end;

procedure TdwsSymbolsLib.dwsUnitClassesTSymbolsMethodsVisibilityEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   sym: TSymbol;
begin
   sym := TSymbols(ExtObject).CurrentSymbol;
   if sym is TFieldSymbol then
      Info.ResultAsInteger := Ord(TFieldSymbol(sym).Visibility)
   else if sym is TPropertySymbol then
      Info.ResultAsInteger := Ord(TPropertySymbol(sym).Visibility)
   else if sym is TMethodSymbol then
      Info.ResultAsInteger := Ord(TMethodSymbol(sym).Visibility)
   else Info.ResultAsInteger := 0;
end;

procedure TdwsSymbolsLib.dwsUnitClassesTSymbolsMethodsDestroyEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  ExtObject.Free;
end;

procedure TdwsSymbolsLib.dwsUnitClassesTSymbolsMethodsNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TSymbols(ExtObject).CurrentSymbol.Name;
end;

procedure TdwsSymbolsLib.dwsUnitClassesTSymbolsMethodsLocateEval(
  Info: TProgramInfo; ExtObject: TObject);
var
  x: Integer;
  table: TSymbolTable;
  name: String;
  wasFound: Boolean;
begin
  table := TSymbols(ExtObject).FTable;
  name := Info.ParamAsString[0];
  wasFound := False;
  for x := 0 to table.Count - 1 do
    if UnicodeSameText(table[x].Name, name) then
    begin
      TSymbols(ExtObject).FIndex := x;
      wasFound := True;
      break;
    end;
  Info.ResultAsBoolean := wasFound;
end;

procedure TdwsSymbolsLib.dwsUnitClassesTSymbolsMethodsMetaSymbolEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   sym : TSymbol;
begin
   sym := TSymbols(ExtObject).CurrentSymbol;
   if sym is TStructuredTypeSymbol then
      sym := TStructuredTypeSymbol(sym).MetaSymbol
   else sym := nil;
   Info.ResultAsVariant := TSymbols.CreateSymbolAsVariant(Info, sym);
end;

procedure TdwsSymbolsLib.dwsUnitClassesTSymbolsMethodsGetSuperClassEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   sym : TSymbol;
begin
   sym := TSymbols(ExtObject).CurrentSymbol;
   if sym is TClassSymbol then
      sym := TClassSymbol(sym).Parent
   else if sym is TInterfaceSymbol then
      sym := TInterfaceSymbol(sym).Parent
   else sym := nil;

   Info.ResultAsVariant := TSymbols.CreateSymbolAsVariant(Info, sym);
end;

procedure TdwsSymbolsLib.dwsUnitClassesTSymbolsMethodsGetTypeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsVariant := TSymbols.CreateSymbolAsVariant(Info, TSymbols(ExtObject).CurrentSymbol.Typ);
end;

procedure TdwsSymbolsLib.dwsUnitClassesTSymbolsMethodsInternalClassNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := TSymbols(ExtObject).CurrentSymbol.ClassName;
end;

procedure TdwsSymbolsLib.dwsUnitClassesTSymbolsMethodsIsTypeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsBoolean := TSymbols(ExtObject).CurrentSymbol.IsType;
end;

procedure TdwsSymbolsLib.dwsUnitClassesTSymbolsMethodsGetParametersEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   sym : TSymbol;
   table : TSymbolTable;
begin
   sym := TSymbols(ExtObject).CurrentSymbol;
   if sym is TFuncSymbol then
      table := TFuncSymbol(sym).Params
   else table := nil;

  Info.ResultAsVariant := TSymbols.CreateTableAsVariant(Info, table);
end;

end.
