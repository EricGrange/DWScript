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
unit UDwsIdeCallStackFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  dwsUtils,
  UDwsIdeDefs,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Menus, ActnList;

type
  TDwsIdeCallStackFrame = class(TFrame)
    PanelHeader: TPanel;
    memCallStack: TMemo;
  private
    FDwsIde : IDwsIde;
  public
    procedure Redraw;
    property  DwsIde : IDwsIde read FDwsIde write FDwsIde;
  end;

implementation

{$R *.dfm}

uses
  dwsSymbols, dwsExprs, dwsDebugger;


{ TDwsIdeCallStackFrame }

procedure TDwsIdeCallStackFrame.Redraw;

  function GetFunctionName( AExprLocation : TdwsExprLocation ) : string;
  begin
    if AExprLocation.prog is TdwsProcedure then
      Result:=TdwsProcedure(AExprLocation.prog).Func.QualifiedName
    else
      Result := '';
  end;

var
  R : TdwsExprLocationArray;
  I : integer;
  S : string;
  Exec : TdwsProgramExecution;
  Proc : TdwsProcedure;
begin
  memCallStack.Lines.BeginUpdate;
  try
    memCallStack.Lines.Clear;
    Exec := TdwsProgramExecution( FDwsIde.DwsIde_GetDebugger.Execution );

    if Exec.CurrentProg is TdwsProcedure then
    begin
      Proc := TdwsProcedure( Exec.CurrentProg );
      memCallStack.Lines.Add( Proc.Func.Name );

      R := Exec.GetCallStack;
      for I := 0 to Length(R)-1 do
      begin
        S := GetFunctionName( R[I] );
        if S <> '' then
          memCallStack.Lines.Add( S );
      end;
    end;
  finally
    memCallStack.Lines.EndUpdate;
  end;
end;

end.
