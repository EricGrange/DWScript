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
unit dwsDebugFunctions;

{$I dws.inc}

interface

uses
   Classes, Variants, SysUtils,
   dwsFunctions, dwsExprs, dwsSymbols, dwsUtils, dwsExprList, dwsStrings,
   dwsMagicExprs, dwsUnitSymbols, dwsXPlatform;

type
   TOutputDebugStringFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

implementation

// DoEvalProc
//
procedure TOutputDebugStringFunc.DoEvalProc(const args : TExprBaseListExec);
var
   exec : TdwsExecution;
begin
   exec:=args.Exec;
   if exec.Debugger<>nil then
      exec.Debugger.DebugMessage(args.AsString[0])
   else OutputDebugString(args.AsString[0]);
end;


initialization

   RegisterInternalProcedure(TOutputDebugStringFunc, 'OutputDebugString', ['m', SYS_STRING]);

end.

