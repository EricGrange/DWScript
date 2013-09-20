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
unit UDwsIdeWatchesFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Menus, ActnList,
  UDwsIdeDefs;

type
  TDwsIdeWatchesFrame = class(TFrame)
    actAddWatch: TAction;
    actDeleteWatch: TAction;
    actEditWatch: TAction;
    ActionList: TActionList;
    lvWatches: TListView;
    MenuItemAddWatch: TMenuItem;
    MenuItemDeleteWatch: TMenuItem;
    MenuItemEditWatch: TMenuItem;
    PanelHeader: TPanel;
    WatchWindowPopupMenu: TPopupMenu;
    procedure actDeleteWatchExecute(Sender: TObject);
    procedure actDeleteWatchUpdate(Sender: TObject);
    procedure actAddWatchExecute(Sender: TObject);
    procedure actEditWatchExecute(Sender: TObject);
    procedure actEditWatchUpdate(Sender: TObject);
  private
    FDwsIde : IDwsIde;
    function  CurrentWatchIndex : integer;
  public
    procedure Redraw;
    property  DwsIde : IDwsIde
                read FDwsIde
                write FDwsIde;
  end;

implementation

{$R *.dfm}

uses
  dwsUtils, dwsSymbols, dwsExprs, dwsDebugger;


{ TfrmDwsIdeWatchesFrame }

procedure TDwsIdeWatchesFrame.actAddWatchExecute(Sender: TObject);
var
  S : string;
begin
  S := '';
  if InputQuery( 'Add Watch', 'Enter watch expression', S ) then
  begin
    FDwsIde.DwsIde_GetDebugger.Watches.Add( S );
    Redraw;
  end;
end;

procedure TDwsIdeWatchesFrame.actDeleteWatchExecute(Sender: TObject);
var
  I : integer;
  Watch : TdwsDebuggerWatch;
begin
  I := CurrentWatchIndex;
  if I >= 0 then
  begin
    Watch := FDwsIde.DwsIde_GetDebugger.Watches[I];
    FDwsIde.DwsIde_GetDebugger.Watches.Extract( Watch );
    Watch.Free;
    Redraw;
  end;
end;

procedure TDwsIdeWatchesFrame.actDeleteWatchUpdate(Sender: TObject);
begin
  with Sender as TAction do
    Enabled := CurrentWatchIndex >= 0;
end;

procedure TDwsIdeWatchesFrame.actEditWatchExecute(Sender: TObject);
var
  I : integer;
  S : string;
begin
  I := CurrentWatchIndex;
  if I >= 0 then
  begin
    S := FDwsIde.DwsIde_GetDebugger.Watches[I].ExpressionText;
    if InputQuery( 'Edit watch', 'Edit Expression', S ) then
    begin
      FDwsIde.DwsIde_GetDebugger.Watches[I].ExpressionText := S;
      Redraw;
    end;
  end;
end;

procedure TDwsIdeWatchesFrame.actEditWatchUpdate(Sender: TObject);
begin
  with Sender as TAction do
    Enabled := CurrentWatchIndex >= 0;
end;

function TDwsIdeWatchesFrame.CurrentWatchIndex: integer;
begin
  if lvWatches.ItemFocused <> nil then
    Result := lvWatches.ItemFocused.Index
  else
    Result := -1;
end;


procedure TDwsIdeWatchesFrame.Redraw;
var
  I : integer;
  S : string;
  V : variant;
  Item : TListItem;
  Watch : TdwsDebuggerWatch;
begin
  lvWatches.Items.BeginUpdate;
  try
    lvWatches.Items.Clear;

    FDwsIde.DwsIde_GetDebugger.Watches.Update;

    for I  := 0 to FDwsIde.DwsIde_GetDebugger.Watches.Count-1 do
    begin
      Watch := FDwsIde.DwsIde_GetDebugger.Watches[I];
      if Watch.ValueInfo = nil then
        S := '[Process not accessible]'
      else
      begin
        V := Watch.ValueInfo.Value;
        S := VarToStr( V );
        if VarIsStr( V ) then
           S := '''' + S + '''';
      end;

      Item := lvWatches.Items.Add;
      Item.Caption := Watch.ExpressionText;
      Item.SubItems.Add( S );

      Watch.ClearEvaluator;
    end;
  finally
    lvWatches.Items.EndUpdate;
  end;
end;

end.
