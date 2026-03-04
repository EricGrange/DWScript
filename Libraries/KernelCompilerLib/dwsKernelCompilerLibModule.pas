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
unit dwsKernelCompilerLibModule;

{$I dws.inc}

interface

uses
  System.Classes, System.SysUtils,
  dwsComp, dwsSymbols, dwsUnitSymbols, dwsOperators;

type
  TKCLUnit = class(TdwsUnit)
  protected
    procedure AddUnitSymbols(systemTable : TSystemSymbolTable; Table: TSymbolTable; operators : TOperators); override;
    procedure StandardCleanUp(ExternalObject: TObject);
  end;

  TdwsKernelCompilerLib = class(TDataModule)
  private
    FdwsKernelCompilerUnit: TKCLUnit;
    procedure SetScript(const Value: TDelphiWebScript);
    function GetScript: TDelphiWebScript;
  public
    constructor Create(AOwner: TComponent); override;
    property dwsKernelCompilerUnit : TKCLUnit read FdwsKernelCompilerUnit;
    property Script : TDelphiWebScript read GetScript write SetScript;
  end;

implementation

uses
  dwsKernelCompiler
{$IFDEF WIN64_ASM}
  , dwsKernelCompilerSSE2
{$ENDIF}
  ;

{$R *.dfm}

{ TKCLUnit }

procedure TKCLUnit.AddUnitSymbols(systemTable: TSystemSymbolTable; Table: TSymbolTable; operators: TOperators);
begin
  inherited;
  RegisterKernelCompilerSymbols(systemTable, Table, StandardCleanUp);
{$IFDEF WIN64_ASM}
  RegisterSSE2CompilerSymbols(Table);
{$ENDIF}
end;

procedure TKCLUnit.StandardCleanUp(ExternalObject: TObject);
begin
  ExternalObject.Free;
end;

{ TdwsKernelCompilerLib }

constructor TdwsKernelCompilerLib.Create(AOwner: TComponent);
begin
  inherited;
  FdwsKernelCompilerUnit := TKCLUnit.Create(Self);
  FdwsKernelCompilerUnit.UnitName := 'KernelCompiler';
end;

function TdwsKernelCompilerLib.GetScript: TDelphiWebScript;
begin
  Result := FdwsKernelCompilerUnit.Script;
end;

procedure TdwsKernelCompilerLib.SetScript(const Value: TDelphiWebScript);
begin
  FdwsKernelCompilerUnit.Script := Value;
end;

end.
