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
unit UDwsIdeDemoForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, dwsComp, dwsFunctions, dwsVCLGUIFunctions;

type
  TDwsIdeDemoForm = class(TForm)
    Button1: TButton;
    DelphiWebScript1: TDelphiWebScript;
    procedure Button1Click(Sender: TObject);
    function DelphiWebScript1NeedUnit(const unitName: string;
      var unitSource: string): IdwsUnit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


var
  DwsIdeDemoForm: TDwsIdeDemoForm;

implementation

{$R *.dfm}
uses
  SynHighlighterDWS,
  UDwsIdeDefs,
  UDwsIdeForm;


procedure TDwsIdeDemoForm.Button1Click(Sender: TObject);
begin
  // This opens the IDE - note that there is an overloaded version which includes
  // some IDE options such as highlighter, font etc.
  DwsIDE_ShowModal( DelphiWebScript1 );
end;

function TDwsIdeDemoForm.DelphiWebScript1NeedUnit(const unitName: string;
  var unitSource: string): IdwsUnit;
var
  SL : TStrings;
  sFileName : string;
  I : integer;
begin
  for I := 0 to DelphiWebScript1.Config.ScriptPaths.Count-1 do
    begin
    sFileName := DelphiWebScript1.Config.ScriptPaths[I] + '\' + UnitName + '.pas';
    if FileExists( sFileName ) then
      begin
      SL := TStringList.Create;
      try
        SL.LoadFromFile( sFileName );
        UnitSource := SL.Text;
      finally
        SL.Free;
      end;
      Exit;
      end;
    end;
  Raise Exception.CreateFmt( 'Unit file name not found "%s"', [unitName]);
end;



end.
