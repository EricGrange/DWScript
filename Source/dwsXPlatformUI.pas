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
unit dwsXPlatformUI;

{$I dws.inc}

//
// This unit should concentrate all UI cross-platform aspects,
// cross-Delphi versions, ifdefs and other conditionals
//
// no ifdefs in the main code.

{$WARN SYMBOL_PLATFORM OFF}

interface

uses Windows, Forms, Classes, SysUtils;

procedure ProcessApplicationMessages(sleepMilliSeconds : Integer);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ProcessApplicationMessages
//
procedure ProcessApplicationMessages(sleepMilliSeconds : Integer);
var
   msg: TMsg;
begin
   if PeekMessage(msg, 0, 0, 0, PM_NOREMOVE) then begin
      Application.HandleMessage;
      if PeekMessage(msg, 0, 0, 0, PM_NOREMOVE) then
         Application.ProcessMessages;
   end;
   if sleepMilliSeconds>0 then
      Sleep(sleepMilliSeconds);
end;

end.
