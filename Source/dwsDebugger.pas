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
  Classes, dwsExprs, dwsSymbols;

type
  TOnDebugStartStopEvent = procedure(exec: TdwsExecution) of object;
  TOnDebugEvent = procedure(exec: TdwsExecution; expr: TExprBase) of object;

  TdwsSimpleDebugger = class(TComponent, IUnknown, IDebugger)
  private
    FOnDebug: TOnDebugEvent;
    FOnStartDebug: TOnDebugStartStopEvent;
    FOnStopDebug: TOnDebugStartStopEvent;
    FOnEnterFunc: TOnDebugEvent;
    FOnLeaveFunc: TOnDebugEvent;
    procedure StartDebug(exec: TdwsExecution);
    procedure DoDebug(exec: TdwsExecution; expr: TExprBase);
    procedure StopDebug(exec: TdwsExecution);
    procedure EnterFunc(exec: TdwsExecution; funcExpr: TExprBase);
    procedure LeaveFunc(exec: TdwsExecution; funcExpr: TExprBase);
  published
    property OnDebug: TOnDebugEvent read FOnDebug write FOnDebug;
    property OnDebugStart: TOnDebugStartStopEvent read FOnStartDebug write FOnStartDebug;
    property OnDebugStop: TOnDebugStartStopEvent read FOnStopDebug write FOnStopDebug;
    property OnEnterFunc: TOnDebugEvent read FOnEnterFunc write FOnEnterFunc;
    property OnLeaveFunc: TOnDebugEvent read FOnLeaveFunc write FOnLeaveFunc;
  end;

implementation

{ TdwsSimpleDebugger }

procedure TdwsSimpleDebugger.DoDebug(exec: TdwsExecution; expr: TExprBase);
begin
  if Assigned(FOnDebug) then
    FOnDebug(exec, Expr);
end;

procedure TdwsSimpleDebugger.EnterFunc(exec: TdwsExecution; funcExpr: TExprBase);
begin
   if Assigned(FOnEnterFunc) then
      if funcExpr is TFuncExprBase then
         FOnEnterFunc(exec, TFuncExprBase(funcExpr));
end;

procedure TdwsSimpleDebugger.LeaveFunc(exec: TdwsExecution; funcExpr: TExprBase);
begin
   if Assigned(FOnLeaveFunc) then
      if funcExpr is TFuncExprBase then
         FOnLeaveFunc(exec, TFuncExprBase(funcExpr));
end;

procedure TdwsSimpleDebugger.StartDebug(exec: TdwsExecution);
begin
  if Assigned(FOnStartDebug) then
    FOnStartDebug(exec);
end;

procedure TdwsSimpleDebugger.StopDebug(exec: TdwsExecution);
begin
  if Assigned(FOnStopDebug) then
    FOnStopDebug(exec);
end;

end.
