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
{    The Initial Developer of the Original DWS Code is Matthias        }
{    Ackermann.                                                        }
{    For other initial contributors, see DWS contributors.txt          }
{    Ackermann.                                                        }
{    DWS code is currently maintained by Eric Grange.                  }
{                                                                      }
{    Current maintainer of the IDE utility: Brian Frost                }
{                                                                      }
{**********************************************************************}
unit UDwsIdeLocalVariablesFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, dwsUtils, UDwsIdeDefs;

type
  TDwsIdeLocalVariablesFrame = class(TFrame)
    ListView: TListView;
    PanelHeader: TPanel;
  private
    FDwsIde: IDwsIde;
  public
    procedure Redraw;
    property  DwsIde: IDwsIde read FDwsIde write FDwsIde;
  end;

implementation

{$R *.dfm}

uses
  dwsSymbols, dwsExprs;


{ TDwsIdeLocalVariablesFrame }

procedure TDwsIdeLocalVariablesFrame.Redraw;

  procedure AppendSymbol( const AName : string );
  var
    S : string;
    Item : TListItem;
  begin
    S := DebuggerEvaluate( FDwsIde.DwsIde_GetDebugger, AName );

    Item := ListView.Items.Add;
    Item.Caption := AName;
    Item.SubItems.Add( S );
  end;

  procedure AppendSymbolsToDisplay( ATable : TSymbolTable; AExec : TdwsProgramExecution );
  var
    I   : integer;
    Sym : TSymbol;
  begin
    for I := 0 to ATable.Count-1 do
    begin
      Sym := ATable[I];
      if Sym is TDataSymbol then
        AppendSymbol( Sym.Name );
    end
  end;

  procedure AppendParamsToDisplay( AProc : TdwsProcedure; AExec : TdwsProgramExecution );
  var
    I   : integer;
    Sym : TSymbol;
  begin
    for I := 0 to AProc.Func.Params.Count-1 do
    begin
      Sym := AProc.Func.Params[I];
      if Sym is TDataSymbol then
        AppendSymbol( Sym.Name );
    end;

    // If it is a function, get the function result
    Sym := AProc.Func.Result;
    if Assigned( Sym ) then
      AppendSymbol( Sym.Name );
  end;

var
  ProgramExecution : TdwsProgramExecution;
begin
  ListView.Items.BeginUpdate;
  try
    ListView.Items.Clear;

    ProgramExecution := TdwsProgramExecution( FDwsIDE.DwsIde_GetDebugger.Execution );

    if ProgramExecution.CurrentProg is TdwsProcedure then
      AppendParamsToDisplay( TdwsProcedure( ProgramExecution.CurrentProg ), ProgramExecution );

    AppendSymbolsToDisplay( ProgramExecution.CurrentProg.Table, ProgramExecution );
  finally
    ListView.Items.EndUpdate;
  end;
end;

end.
