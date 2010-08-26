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
unit dwsDebugger;

interface

uses
  Classes, dwsExprs;

type
  TOnDebugStartStopEvent = procedure(MainProg: TdwsProgram) of object;
  TOnDebugEvent = procedure(Prog: TdwsProgram; expr: TNoPosExpr) of object;

  TdwsSimpleDebugger = class(TComponent, IUnknown, IDebugger)
  private
    FOnDebug: TOnDebugEvent;
    FOnStartDebug: TOnDebugStartStopEvent;
    FOnStopDebug: TOnDebugStartStopEvent;
    FOnEnterFunc: TOnDebugEvent;
    FOnLeaveFunc: TOnDebugEvent;
    procedure StartDebug(MainProg: TdwsProgram);
    procedure DoDebug(Prog: TdwsProgram; Expr: TExpr);
    procedure StopDebug(MainProg: TdwsProgram);
    procedure EnterFunc(Prog: TdwsProgram; funcExpr: TNoPosExpr);
    procedure LeaveFunc(Prog: TdwsProgram; funcExpr: TNoPosExpr);
  published
    property OnDebug: TOnDebugEvent read FOnDebug write FOnDebug;
    property OnDebugStart: TOnDebugStartStopEvent read FOnStartDebug write FOnStartDebug;
    property OnDebugStop: TOnDebugStartStopEvent read FOnStopDebug write FOnStopDebug;
    property OnEnterFunc: TOnDebugEvent read FOnEnterFunc write FOnEnterFunc;
    property OnLeaveFunc: TOnDebugEvent read FOnLeaveFunc write FOnLeaveFunc;
  end;

implementation

{ TdwsSimpleDebugger }

procedure TdwsSimpleDebugger.DoDebug(Prog: TdwsProgram; Expr: TExpr);
begin
  if Assigned(FOnDebug) then
    FOnDebug(Prog, Expr);
end;

procedure TdwsSimpleDebugger.EnterFunc(Prog: TdwsProgram; funcExpr: TNoPosExpr);
begin
   if Assigned(FOnEnterFunc) then
      if funcExpr is TFuncExprBase then
         FOnEnterFunc(Prog, TFuncExprBase(funcExpr));
end;

procedure TdwsSimpleDebugger.LeaveFunc(Prog: TdwsProgram; funcExpr: TNoPosExpr);
begin
   if Assigned(FOnLeaveFunc) then
      if funcExpr is TFuncExprBase then
         FOnLeaveFunc(Prog, TFuncExprBase(funcExpr));
end;

procedure TdwsSimpleDebugger.StartDebug(MainProg: TdwsProgram);
begin
  if Assigned(FOnStartDebug) then
    FOnStartDebug(MainProg);
end;

procedure TdwsSimpleDebugger.StopDebug(MainProg: TdwsProgram);
begin
  if Assigned(FOnStopDebug) then
    FOnStopDebug(MainProg);
end;

end.
