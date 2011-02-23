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
{$I dws.inc}
unit dwsSymbolsLibModule;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  dwsComp, dwsExprs;

type
  TdwsSymbolsLib = class(TDataModule)
    dwsUnit1: TdwsUnit;
    procedure dwsUnit1ClassesTSymbolsMethodsFirstEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnit1ClassesTSymbolsMethodsLastEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnit1ClassesTSymbolsMethodsNextEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnit1ClassesTSymbolsMethodsPreviousEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnit1ClassesTSymbolsMethodsEofEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnit1ClassesTSymbolsMethodsCaptionEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnit1ClassesTSymbolsMethodsDescriptionEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnit1ClassesTSymbolsMethodsGetMembersEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnit1ClassesTSymbolsMethodsSymbolTypeEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnit1ClassesTSymbolsMethodsDestroyEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnit1ClassesTSymbolsMethodsNameEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnit1ClassesTSymbolsMethodsLocateEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnit1ClassesTSymbolsMethodsGetSuperClassEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnit1ClassesTSymbolsMethodsGetParametersEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnit1ClassesTSymbolsConstructorsCreateMainEval(
      Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsUnit1ClassesTSymbolsConstructorsCreateUidEval(
      Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsUnit1ClassesTSymbolsConstructorsCreateUnitEval(
      Info: TProgramInfo; var ExtObject: TObject);
  private
    FScript: TDelphiWebScript;
    procedure SetScript(const Value: TDelphiWebScript);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property Script: TDelphiWebScript read FScript write SetScript;
  end;

procedure Register;

var
  dwsSymbolsLib: TdwsSymbolsLib;

implementation

{$R *.DFM}

uses
  dwsSymbols;

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
    procedure SetIndex(Index: Integer);
    procedure SetSymbol(Symbol: TSymbol);
    property CurrentSymbol: TSymbol read FCurrentSymbol;
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
  stMember = 6;
  stParam = 7;
  stProperty = 8;
  stRecord = 9;
  stUnit = 10;
  stVariable = 11;

procedure Register;
begin
  RegisterComponents('dws', [TdwsSymbolsLib]);
end;

{ TSymbols }

constructor TSymbols.Create(Table: TSymbolTable);
begin
  FTable := Table;
  FCount := Table.Count;
  SetIndex(0)
end;

constructor TSymbols.Create(Symbol: TSymbol);
begin
  SetSymbol(Symbol);
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

procedure TdwsSymbolsLib.dwsUnit1ClassesTSymbolsConstructorsCreateMainEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
  ExtObject := TSymbols.Create(Info.Execution.Prog.RootTable);
end;

procedure TdwsSymbolsLib.dwsUnit1ClassesTSymbolsConstructorsCreateUnitEval(
  Info: TProgramInfo; var ExtObject: TObject);
var
  sym: TSymbol;
begin
  sym := Info.Execution.Prog.RootTable.FindLocal(Info.ValueAsString['Name']);
  if Assigned(sym) and (sym is TUnitSymbol) then
    ExtObject := TSymbols.Create(TUnitSymbol(sym).Table)
  else
    raise Exception.CreateFmt('Unit "%s" not found!', [Info.ValueAsString['Name']]);
end;

procedure TdwsSymbolsLib.dwsUnit1ClassesTSymbolsConstructorsCreateUidEval(
  Info: TProgramInfo; var ExtObject: TObject);
var
  table: TSymbolTable;
  sym: TSymbol;
  uid, name: string;
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
    else if sym is TClassSymbol then
      table := TClassSymbol(sym).Members
    else if sym is TFuncSymbol then
      table := TFuncSymbol(sym).Params
    else if sym is TRecordSymbol then
      table := TRecordSymbol(sym).Members;
  end;

  ExtObject := TSymbols.Create(sym);
end;

procedure TdwsSymbolsLib.dwsUnit1ClassesTSymbolsMethodsFirstEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TSymbols(ExtObject).SetIndex(0);
end;

procedure TdwsSymbolsLib.dwsUnit1ClassesTSymbolsMethodsLastEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TSymbols(ExtObject).SetIndex(TSymbols(ExtObject).Count - 1);
end;

procedure TdwsSymbolsLib.dwsUnit1ClassesTSymbolsMethodsNextEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TSymbols(ExtObject).SetIndex(TSymbols(ExtObject).Index + 1);
end;

procedure TdwsSymbolsLib.dwsUnit1ClassesTSymbolsMethodsPreviousEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TSymbols(ExtObject).SetIndex(TSymbols(ExtObject).Index - 1);
end;

procedure TdwsSymbolsLib.dwsUnit1ClassesTSymbolsMethodsEofEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := not Assigned(TSymbols(ExtObject).FCurrentSymbol);
end;

procedure TdwsSymbolsLib.dwsUnit1ClassesTSymbolsMethodsCaptionEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TSymbols(ExtObject).CurrentSymbol.Caption;
end;

procedure TdwsSymbolsLib.dwsUnit1ClassesTSymbolsMethodsDescriptionEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TSymbols(ExtObject).CurrentSymbol.Description;
end;

procedure TdwsSymbolsLib.dwsUnit1ClassesTSymbolsMethodsGetMembersEval(
  Info: TProgramInfo; ExtObject: TObject);
var
  sym: TSymbol;
begin
  sym := TSymbols(ExtObject).CurrentSymbol;
  if sym is TRecordSymbol then
    Info.ResultAsVariant := Info.Vars['TSymbols'].GetConstructor('Create',
      TSymbols.Create(TRecordSymbol(sym).Members)).Call.Value
  else if sym is TClassSymbol then
    Info.ResultAsVariant := Info.Vars['TSymbols'].GetConstructor('Create',
      TSymbols.Create(TClassSymbol(sym).Members)).Call.Value
  else if sym is TUnitSymbol then
    Info.ResultAsVariant := Info.Vars['TSymbols'].GetConstructor('Create',
      TSymbols.Create(TUnitSymbol(sym).Table)).Call.Value
end;

procedure TdwsSymbolsLib.dwsUnit1ClassesTSymbolsMethodsSymbolTypeEval(
  Info: TProgramInfo; ExtObject: TObject);
var
  sym: TSymbol;
begin
  sym := TSymbols(ExtObject).CurrentSymbol;
  if sym is TArraySymbol then
    Info.ResultAsInteger := stArray
  else if sym is TAliasSymbol then
    Info.ResultAsInteger := stAlias
  else if sym is TClassSymbol then
    Info.ResultAsInteger := stClass
  else if sym is TConstSymbol then
    Info.ResultAsInteger := stConstant
  else if sym is TFieldSymbol then
    Info.ResultAsInteger := stField
  else if sym is TFuncSymbol then
    Info.ResultAsInteger := stFunction
  else if sym is TMemberSymbol then
    Info.ResultAsInteger := stMember
  else if sym is TParamSymbol then
    Info.ResultAsInteger := stParam
  else if sym is TPropertySymbol then
    Info.ResultAsInteger := stProperty
  else if sym is TRecordSymbol then
    Info.ResultAsInteger := stRecord
  else if sym is TUnitSymbol then
    Info.ResultAsInteger := stUnit
  else if sym is TDataSymbol then
    Info.ResultAsInteger := stVariable
  else
    Info.ResultAsInteger := stUnknown;
end;

procedure TdwsSymbolsLib.dwsUnit1ClassesTSymbolsMethodsDestroyEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  ExtObject.Free;
end;

procedure TdwsSymbolsLib.dwsUnit1ClassesTSymbolsMethodsNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TSymbols(ExtObject).CurrentSymbol.Name;
end;

procedure TdwsSymbolsLib.dwsUnit1ClassesTSymbolsMethodsLocateEval(
  Info: TProgramInfo; ExtObject: TObject);
var
  x: Integer;
  table: TSymbolTable;
  name: string;
  wasFound: Boolean;
begin
  table := TSymbols(ExtObject).FTable;
  name := Info.ValueAsString['Name'];
  wasFound := False;
  for x := 0 to table.Count - 1 do
    if SameText(table[x].Name, name) then
    begin
      TSymbols(ExtObject).FIndex := x;
      wasFound := True;
      break;
    end;
  Info.ResultAsBoolean := wasFound;
end;

procedure TdwsSymbolsLib.dwsUnit1ClassesTSymbolsMethodsGetSuperClassEval(
  Info: TProgramInfo; ExtObject: TObject);
var
  sym: TSymbol;
begin
  sym := TSymbols(ExtObject).CurrentSymbol;
  if sym is TClassSymbol then
    Info.ResultAsVariant := Info.Vars['TSymbols'].GetConstructor('Create',
      TSymbols.Create(TClassSymbol(sym).Parent)).Call.Value
  else
    Info.ResultAsInteger := 0;
end;

procedure TdwsSymbolsLib.dwsUnit1ClassesTSymbolsMethodsGetParametersEval(
  Info: TProgramInfo; ExtObject: TObject);
var
  sym: TSymbol;
begin
  sym := TSymbols(ExtObject).CurrentSymbol;
  Info.ResultAsVariant := Info.Vars['TSymbols'].GetConstructor('Create',
    TSymbols.Create(TFuncSymbol(sym).Params)).Call.Value
end;

end.
