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
unit dwsVCLGUIFunctions;

interface

uses Windows, Forms, Dialogs, Classes, dwsFunctions, dwsExprs, dwsSymbols;

type

  TShowMessageFunc = class(TInternalMagicProcedure)
    procedure DoEvalProc(args : TExprBaseList); override;
  end;

  TInputBoxFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

  TdwsGUIFunctions = class(TComponent)
  end;

implementation

const // type constants
  cFloat = 'Float';
  cInteger = 'Integer';
  cString = 'String';
  cBoolean = 'Boolean';

{ TShowMessageFunc }

// DoEvalProc
//
procedure TShowMessageFunc.DoEvalProc(args : TExprBaseList);
begin
   ShowMessage(args.AsString[0]);
end;

{ TInputBoxFunc }

// DoEvalAsString
//
procedure TInputBoxFunc.DoEvalAsString(args : TExprBaseList; var Result : String);
begin
   Result:=InputBox(args.AsString[0], args.AsString[1], args.AsString[2]);
end;

initialization

   RegisterInternalProcedure(TShowMessageFunc, 'ShowMessage', ['msg', cString]);
   RegisterInternalStringFunction(TInputBoxFunc, 'InputBox', ['aCaption', cString, 'aPrompt', cString, 'aDefault', cString]);
  
end.
