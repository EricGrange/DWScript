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
   Classes, SysUtils, dwsExprs, dwsSymbols, dwsXPlatform, dwsCompiler;

type
   TOnDebugStartStopEvent = procedure(exec: TdwsExecution) of object;
   TOnDebugEvent = procedure(exec: TdwsExecution; expr: TExprBase) of object;

   // TdwsSimpleDebugger
   //
   TdwsSimpleDebugger = class (TComponent, IUnknown, IDebugger)
      private
         FOnDebug : TOnDebugEvent;
         FOnStartDebug : TOnDebugStartStopEvent;
         FOnStopDebug : TOnDebugStartStopEvent;
         FOnEnterFunc : TOnDebugEvent;
         FOnLeaveFunc : TOnDebugEvent;

      protected
         procedure StartDebug(exec : TdwsExecution); virtual;
         procedure DoDebug(exec : TdwsExecution; expr : TExprBase); virtual;
         procedure StopDebug(exec : TdwsExecution); virtual;
         procedure EnterFunc(exec : TdwsExecution; funcExpr : TExprBase); virtual;
         procedure LeaveFunc(exec : TdwsExecution; funcExpr : TExprBase); virtual;

      public


      published
         property OnDebug : TOnDebugEvent read FOnDebug write FOnDebug;
         property OnDebugStart : TOnDebugStartStopEvent read FOnStartDebug write FOnStartDebug;
         property OnDebugStop : TOnDebugStartStopEvent read FOnStopDebug write FOnStopDebug;
         property OnEnterFunc : TOnDebugEvent read FOnEnterFunc write FOnEnterFunc;
         property OnLeaveFunc : TOnDebugEvent read FOnLeaveFunc write FOnLeaveFunc;
   end;

   TdwsDebuggerState = (dsIdle, dsDebugRun, dsDebugSuspended, dsDebugDone);

   TdwsDebuggerAction = (daCanBeginDebug, daCanSuspend, daCanStep, daCanResume,
                         daCanEndDebug, daCanEvaluate);
   TdwsDebuggerActions = set of TdwsDebuggerAction;

   TdwsDebuggerMode = (dmMainThread, dmThreadedSynchronize, dmThreaded);

   // TdwsDebugger
   //
   // Work in progres, compiles, but is NOT operational yet
   TdwsDebugger = class (TdwsSimpleDebugger)
      private
         FExecution : IdwsProgramExecution;
         FOnStateChanged : TNotifyEvent;
         FMode : TdwsDebuggerMode;
         FParams : TVariantDynArray;
         FState : TdwsDebuggerState;
         FScriptSuspended : Boolean;

      protected
         procedure StateChanged;

         procedure ExecuteDebug(const notifyStageChanged : TThreadMethod);

      public
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         procedure BeginDebug(exec : IdwsProgramExecution);
         procedure EndDebug;

         procedure Suspend;
         procedure Resume;

         function Evaluate(const expression : String) : TNoPosExpr;
         function EvaluateAsString(const expression : String) : String;

         function AllowedActions : TdwsDebuggerActions;

         property Execution : IdwsProgramExecution read FExecution;
         property Params : TVariantDynArray read FParams write FParams;
         property State : TdwsDebuggerState read FState;

      published
         property Mode : TdwsDebuggerMode read FMode write FMode;

         property OnStateChanged : TNotifyEvent read FOnStateChanged write FOnStateChanged;
  end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   TThreadedDebugger = class (TThread)
      FMain : TdwsDebugger;
      FExec : IdwsProgramExecution;
      constructor Create(exec : IdwsProgramExecution; main : TdwsDebugger);
      destructor Destroy; override;
      procedure Execute; override;
   end;

   TSynchronizedThreadedDebugger = class (TThreadedDebugger, IDebugger)
      function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
      function _AddRef: Integer; stdcall;
      function _Release: Integer; stdcall;
      procedure StateChanged;
      procedure StartDebug(exec : TdwsExecution);
      procedure DoDebug(exec : TdwsExecution; expr : TExprBase);
      procedure StopDebug(exec : TdwsExecution);
      procedure EnterFunc(exec : TdwsExecution; funcExpr : TExprBase);
      procedure LeaveFunc(exec : TdwsExecution; funcExpr : TExprBase);
   end;

// ------------------
// ------------------ TThreadedDebugger ------------------
// ------------------

// Create
//
constructor TThreadedDebugger.Create(exec : IdwsProgramExecution; main : TdwsDebugger);
begin
   inherited Create;
   FExec:=exec;
   FMain:=main;
   FreeOnTerminate:=True;
end;

// Destroy
//
destructor TThreadedDebugger.Destroy;
begin
   inherited;
   FExec:=nil;
end;

// Execute
//
procedure TThreadedDebugger.Execute;
begin
   FExec.Debugger:=FMain;
   FMain.ExecuteDebug(FMain.StateChanged);
end;

// ------------------
// ------------------ TSynchronizedThreadedDebugger ------------------
// ------------------

// QueryInterface
//
function TSynchronizedThreadedDebugger.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
   if GetInterface(IID, Obj) then
      Result:=S_OK
   else Result:=E_NOINTERFACE
end;

// _AddRef
//
function TSynchronizedThreadedDebugger._AddRef: Integer;
begin
   Result:=-1;   // -1 indicates no reference counting is taking place
end;

// _Release
//
function TSynchronizedThreadedDebugger._Release: Integer;
begin
   Result:=-1;   // -1 indicates no reference counting is taking place
end;

// StateChanged
//
procedure TSynchronizedThreadedDebugger.StateChanged;
begin
   Synchronize(FMain.StateChanged);
end;

// StartDebug
//
procedure TSynchronizedThreadedDebugger.StartDebug(exec : TdwsExecution);
begin
   Synchronize(procedure begin FMain.StartDebug(exec) end);
end;

// DoDebug
//
procedure TSynchronizedThreadedDebugger.DoDebug(exec : TdwsExecution; expr : TExprBase);
begin
   Synchronize(procedure begin FMain.DoDebug(exec, expr) end);
end;

// StopDebug
//
procedure TSynchronizedThreadedDebugger.StopDebug(exec : TdwsExecution);
begin
   Synchronize(procedure begin FMain.StopDebug(exec) end);
end;

// EnterFunc
//
procedure TSynchronizedThreadedDebugger.EnterFunc(exec : TdwsExecution; funcExpr : TExprBase);
begin
   Synchronize(procedure begin FMain.EnterFunc(exec, funcExpr) end);
end;

// LeaveFunc
//
procedure TSynchronizedThreadedDebugger.LeaveFunc(exec : TdwsExecution; funcExpr : TExprBase);
begin
   Synchronize(procedure begin FMain.LeaveFunc(exec, funcExpr) end);
end;

// ------------------
// ------------------ TdwsSimpleDebugger ------------------
// ------------------

// DoDebug
//
procedure TdwsSimpleDebugger.DoDebug(exec: TdwsExecution; expr: TExprBase);
begin
   if Assigned(FOnDebug) then
      FOnDebug(exec, Expr);
end;

// EnterFunc
//
procedure TdwsSimpleDebugger.EnterFunc(exec: TdwsExecution; funcExpr: TExprBase);
begin
   if Assigned(FOnEnterFunc) then
      if funcExpr is TFuncExprBase then
         FOnEnterFunc(exec, TFuncExprBase(funcExpr));
end;

// LeaveFunc
//
procedure TdwsSimpleDebugger.LeaveFunc(exec: TdwsExecution; funcExpr: TExprBase);
begin
   if Assigned(FOnLeaveFunc) then
      if funcExpr is TFuncExprBase then
         FOnLeaveFunc(exec, TFuncExprBase(funcExpr));
end;

// StartDebug
//
procedure TdwsSimpleDebugger.StartDebug(exec: TdwsExecution);
begin
   if Assigned(FOnStartDebug) then
      FOnStartDebug(exec);
end;

// StopDebug
//
procedure TdwsSimpleDebugger.StopDebug(exec: TdwsExecution);
begin
   if Assigned(FOnStopDebug) then
      FOnStopDebug(exec);
end;

// ------------------
// ------------------ TdwsDebugger ------------------
// ------------------

// Create
//
constructor TdwsDebugger.Create(AOwner: TComponent);
begin
   inherited;
   FState:=dsIdle;
end;

// Destroy
//
destructor TdwsDebugger.Destroy;
begin
   if daCanEndDebug in AllowedActions then
      EndDebug;
   inherited;
end;

// StateChanged
//
procedure TdwsDebugger.StateChanged;
begin
   if Assigned(FOnStateChanged) then
      FOnStateChanged(Self);
end;

// BeginDebug
//
procedure TdwsDebugger.BeginDebug(exec : IdwsProgramExecution);
begin
   Assert(not (daCanBeginDebug in AllowedActions), 'BeginDebug not allowed');
   Assert(exec<>nil, 'Execution is nil');

   FExecution:=exec;
   case Mode of
      dmMainThread :
         ExecuteDebug(StateChanged);
      dmThreadedSynchronize :
         TSynchronizedThreadedDebugger.Create(exec, Self);
      dmThreaded :
         TThreadedDebugger.Create(exec, Self);
   else
      Assert(False);
   end;
end;

// EndDebug
//
procedure TdwsDebugger.EndDebug;
begin
   Assert(not (daCanEndDebug in AllowedActions), 'EndDebug not allowed');

   FExecution.Stop;
   while FState<>dsDebugDone do
      ProcessApplicationMessages(25);

   FExecution:=nil;
end;

// Suspend
//
procedure TdwsDebugger.Suspend;
begin
   Assert(not (daCanSuspend in AllowedActions), 'Suspend not allowed');

   FState:=dsDebugSuspended;
   while not FScriptSuspended do
      ProcessApplicationMessages(25);
end;

// Resume
//
procedure TdwsDebugger.Resume;
begin
   Assert(not (daCanResume in AllowedActions), 'Resume not allowed');

   FState:=dsDebugRun;
   while FScriptSuspended do
      ProcessApplicationMessages(25);
end;

// Evaluate
//
function TdwsDebugger.Evaluate(const expression : String) : TNoPosExpr;
begin
   Assert(not (daCanEvaluate in AllowedActions), 'Evaluate not allowed');

   Result:=TdwsCompiler.Evaluate(FExecution, expression);
end;

// EvaluateAsString
//
function TdwsDebugger.EvaluateAsString(const expression : String) : String;
var
   expr : TNoPosExpr;
begin
   try
      expr:=Evaluate(expression);
      try
         Result:='(no result)';
         expr.EvalAsString(FExecution as TdwsExecution, Result);
      finally
         expr.Free;
      end;
   except
      on E : Exception do
         Result:=E.Message;
   end;
end;

// AllowedActions
//
function TdwsDebugger.AllowedActions : TdwsDebuggerActions;
begin
   Result:=[];
   if Assigned(FExecution) then begin
      case FState of
         dsIdle :
            Result:=[daCanBeginDebug];
         dsDebugRun :
            if not FScriptSuspended then
               Result:=[daCanSuspend, daCanEndDebug];
         dsDebugSuspended :
            if FScriptSuspended then
               Result:=[daCanResume, daCanEndDebug, daCanStep, daCanEvaluate];
         dsDebugDone :
            Result:=[daCanEvaluate];
      else
         Assert(False);
      end;
   end;
end;

// ExecuteDebug
//
procedure TdwsDebugger.ExecuteDebug(const notifyStageChanged : TThreadMethod);
begin
   FState:=dsDebugRun;
   try
      FExecution.Debugger:=Self;
      try
         notifyStageChanged();
         if Length(FParams)>0 then
            FExecution.ExecuteParam(FParams)
         else FExecution.Execute;
      finally
         FExecution.Debugger:=nil;
      end;
   finally
      FState:=dsDebugDone;
   end;
   notifyStageChanged();
end;


end.
