{******************************************************************************}
{                                                                              }
// Copyright (c) 1995-2010 Embarcadero Technologies, Inc.

// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.
{ Cesar Romero <cesar@liws.com.br>                                             }
{ All Rights Reserved                                                          }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ Version: MPL 1.1                                                             }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License          }
{ Version 1.1 (the "License"); you may not use this file except in compliance  }
{ with the License.                                                            }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/          }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ How to use this unit:                                                        }
{                                                                              }
{ 1) add to the project                                                        }
{ 2) in the base dock form, implement the event OnStartDock like this:         }
{                                                                              }
{ procedure TBaseDockForm.FormStartDock(Sender: TObject; var DragObject:       }
{   TDragDockObject);                                                          }
{ begin                                                                        }
{   DragObject:= TTransparentDragDockObject.Create(Self);                      }
{ end;                                                                         } 
{                                                                              }
{******************************************************************************}
unit DockingUtils;

interface

uses
  Classes, Windows, SysUtils, Graphics, Controls, ExtCtrls, Forms, DockTabSet;

type
  TTransparentForm = class(TForm)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

  TTransparentDragDockObject = class(TDragDockObjectEx)
  protected
    function GetEraseWhenMoving: Boolean; override;
    procedure DrawDragDockImage; override;
    procedure EraseDragDockImage; override;
  public
    constructor Create(AControl: TControl); override;
  end;

implementation

var
  TransparentForm: TTransparentForm;
  AlphaBlendValue: Integer = 100;

{ TTransparentForm }

procedure TTransparentForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle:= Params.ExStyle or WS_EX_TRANSPARENT;
end;

{ TTransparentDragDockObject }

constructor TTransparentDragDockObject.Create(AControl: TControl);
begin
  inherited;
  if TransparentForm = nil then
  begin
    TransparentForm := TTransparentForm.CreateNew(Application);
    TransparentForm.AlphaBlend := True;
    TransparentForm.AlphaBlendValue := AlphaBlendValue;
    TransparentForm.BorderStyle := bsNone;
    TransparentForm.Color := clHighlight;
    TransparentForm.FormStyle := fsStayOnTop;
  end;
end;

procedure TTransparentDragDockObject.EraseDragDockImage;
begin
  TransparentForm.Hide;
end;

procedure TTransparentDragDockObject.DrawDragDockImage;
begin
  if TransparentForm <> nil then
  begin
    TransparentForm.BoundsRect:= DockRect;
    if not TransparentForm.Visible then
      TransparentForm.Show;
  end;
end;

function TTransparentDragDockObject.GetEraseWhenMoving: Boolean;
begin
  Result:= False;
end;

end.
