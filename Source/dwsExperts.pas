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
// Design-Time only, do NOT include this unit in runtime packages.
unit dwsExperts;

{$I dws.inc}

interface

procedure Register;

implementation

uses Classes, Forms, SysUtils, DesignIntf, DesignEditors,
  dwsStrings, dwsComp, dwsSymbols;

type
  TdwsDataTypeProperty = class(TStringProperty)
  protected
    { Protected Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TdwsAncestorProperty = class(TStringProperty)
  protected
    { Protected Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TdwsDataTypeProperty }

function TdwsDataTypeProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TdwsDataTypeProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  dwsUnit: TdwsUnit;
  sl: TStringList;
begin
  if not (GetComponent(0) is TdwsSymbol) then
    exit;

  dwsUnit := TdwsSymbol(GetComponent(0)).GetUnit;

  sl := TStringList.Create;
  try
    dwsUnit.GetDataTypes(sl);
    sl.Sorted := True;
    sl.Duplicates := dupIgnore;

    // feed the dropdown list
    for i := 0 to sl.Count - 1 do
      if Length(sl[i]) > 0 then
        Proc(sl[i]);
  finally
    sl.Free;
  end;
end;

{ TdwsAncestorProperty }

function TdwsAncestorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TdwsAncestorProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  sl: TStringList;
  dwsUnit: TdwsUnit;
begin
  if GetComponent(0) is TdwsSymbol then
  begin
    dwsUnit := TdwsSymbol(GetComponent(0)).GetUnit;

    sl := TStringList.Create;
    try
      dwsUnit.GetClassTypes(sl);
      sl.Sorted := True;
      sl.Duplicates := dupIgnore;

      // feed the dropdown list
      for i := 0 to sl.Count - 1 do
        if Length(sl[i]) > 0 then
          Proc(sl[i]);
    finally
      sl.Free;
    end;
  end;
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TDataType), TdwsFunction, 'ResultType',
    TdwsDataTypeProperty);
  RegisterPropertyEditor(TypeInfo(TDataType), TdwsVariable, 'DataType',
    TdwsDataTypeProperty);
  RegisterPropertyEditor(TypeInfo(TDataType), TdwsProperty, 'DataType',
    TdwsDataTypeProperty);
  RegisterPropertyEditor(TypeInfo(TDataType), TdwsProperty, 'IndexType',
    TdwsDataTypeProperty);
  RegisterPropertyEditor(TypeInfo(TDataType), TdwsGlobal, 'DataType',
    TdwsDataTypeProperty);
  RegisterPropertyEditor(TypeInfo(TDataType), TdwsParameter, 'DataType',
    TdwsDataTypeProperty);
  RegisterPropertyEditor(TypeInfo(TDataType), TdwsField, 'DataType',
    TdwsDataTypeProperty);
  RegisterPropertyEditor(TypeInfo(TDataType), TdwsMember, 'DataType',
    TdwsDataTypeProperty);
  RegisterPropertyEditor(TypeInfo(TDataType), TdwsSet, 'BaseType',
    TdwsDataTypeProperty);
  RegisterPropertyEditor(TypeInfo(TDataType), TdwsArray, 'DataType',
    TdwsDataTypeProperty);
  RegisterPropertyEditor(TypeInfo(TDataType), TdwsSet, 'BaseType',
    TdwsDataTypeProperty);
  RegisterPropertyEditor(TypeInfo(TDataType), TdwsClass, 'Ancestor',
    TdwsAncestorProperty);
end;

end.

