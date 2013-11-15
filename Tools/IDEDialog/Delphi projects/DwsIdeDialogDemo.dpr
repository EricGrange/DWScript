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


program DwsIdeDialogDemo;

uses
  Forms,
  UDwsIdeConfig in '..\Source\UDwsIdeConfig.pas',
  UDwsIdeForm in '..\Source\UDwsIdeForm.pas' {DwsIdeForm},
  UDwsIdeDemoForm in 'UDwsIdeDemoForm.pas' {DwsIdeDemoForm},
  UDwsIdeLocalVariablesFrame in '..\Source\UDwsIdeLocalVariablesFrame.pas' {DwsIdeLocalVariablesFrame: TFrame},
  UDwsIdeWatchesFrame in '..\Source\UDwsIdeWatchesFrame.pas' {DwsIdeWatchesFrame: TFrame},
  UDwsIdeDefs in '..\Source\UDwsIdeDefs.pas',
  UDwsIdeCallStackFrame in '..\Source\UDwsIdeCallStackFrame.pas' {DwsIdeCallStackFrame: TFrame},
  UDwsIdeGotoLine in '..\Source\UDwsIdeGotoLine.pas' {DwsIdeGotoLineNumber},
  SynHighlighterDWS in '..\..\..\Source\SynEdit\SynHighlighterDWS.pas';

{$R *.res}

begin
  System.ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDwsIdeDemoForm, DwsIdeDemoForm);
  Application.Run;
end.
