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
{    Copyright Eric Grange / Creative IT                               }
{                                                                      }
{**********************************************************************}
unit dwsCoverage;

{$I ../dws.inc}

interface

uses
   Classes, SysUtils,
   dwsUtils, dwsSymbols, dwsDebugger, dwsExprs, dwsErrors, dwsXPlatform,
   dwsScriptSource;


type

   TCoverageStatus = (csUnknown, csNotRunnable, csNotCovered, csCovered);

   TdwsCodeCoverageReportEntry = record
      SourceName : UnicodeString;
      Runnable : Integer;
      NonCovered : Integer;
   end;
   PdwsCodeCoverageReportEntry = ^TdwsCodeCoverageReportEntry;
   TdwsCodeCoverageReportEntries = array of TdwsCodeCoverageReportEntry;

   TdwsCodeCoverageReport = class
      private
         FEntries : TdwsCodeCoverageReportEntries;

      public
         property Entries : TdwsCodeCoverageReportEntries read FEntries;
   end;

   TdwsCoverageDebugger = class (TComponent, IDebugger)
      private
         FDebugger : IDebugger;
         FLastDebugStepExpr : TExprBase;
         FProgram : IdwsProgram;
         FNonCovered : TdwsBreakpointableLines;
         FAllLines : TdwsBreakpointableLines;

      protected
         procedure StartDebug(exec : TdwsExecution);
         procedure DoDebug(exec : TdwsExecution; expr : TExprBase);
         procedure StopDebug(exec : TdwsExecution);
         procedure EnterFunc(exec : TdwsExecution; funcExpr : TExprBase);
         procedure LeaveFunc(exec : TdwsExecution; funcExpr : TExprBase);
         function  LastDebugStepExpr : TExprBase;
         procedure DebugMessage(const msg : UnicodeString);
         procedure NotifyException(exec : TdwsExecution; const exceptObj : IScriptObj);

         procedure SetProgram(const val : IdwsProgram);

      public
         constructor Create(AOwner: TComponent); override;
         destructor  Destroy; override;

         property  Prog : IdwsProgram read FProgram write SetProgram;
         procedure ResetCoverage;

         property Debugger : IDebugger read FDebugger write FDebugger;

         function CoverageStatus(const sourceName : UnicodeString; line : Integer) : TCoverageStatus;

         function CreateReport : TdwsCodeCoverageReport;
         function HasReport : Boolean;
   end;


implementation

// ------------------
// ------------------ TdwsCoverageDebugger ------------------
// ------------------

// Create
//
constructor TdwsCoverageDebugger.Create(AOwner: TComponent);
begin
   inherited;
end;

// Destroy
//
destructor TdwsCoverageDebugger.Destroy;
begin
   FNonCovered.Free;
   FAllLines.Free;
   inherited;
end;

// StartDebug
//
procedure TdwsCoverageDebugger.StartDebug(exec : TdwsExecution);
begin
   if Assigned(FDebugger) then
      FDebugger.StartDebug(exec);
end;

// DoDebug
//
procedure TdwsCoverageDebugger.DoDebug(exec : TdwsExecution; expr : TExprBase);
var
   p : TScriptPos;
   bits : TBits;
begin
   FLastDebugStepExpr:=expr;

   p:=expr.ScriptPos;
   if p.SourceFile<>nil then begin
      bits:=FNonCovered.SourceLines[p.SourceFile.Name];
      if (bits<>nil) and (p.Line<bits.Size) then
         bits[p.Line]:=False;
   end;

   if Assigned(FDebugger) then
      FDebugger.DoDebug(exec, expr);
end;

// StopDebug
//
procedure TdwsCoverageDebugger.StopDebug(exec : TdwsExecution);
begin
   FLastDebugStepExpr:=nil;
   if Assigned(FDebugger) then
      FDebugger.StopDebug(exec);
end;

// EnterFunc
//
procedure TdwsCoverageDebugger.EnterFunc(exec : TdwsExecution; funcExpr : TExprBase);
begin
   if Assigned(FDebugger) then
      FDebugger.EnterFunc(exec, funcExpr);
end;

// LeaveFunc
//
procedure TdwsCoverageDebugger.LeaveFunc(exec : TdwsExecution; funcExpr : TExprBase);
begin
   if Assigned(FDebugger) then
      FDebugger.LeaveFunc(exec, funcExpr);
end;

// LastDebugStepExpr
//
function TdwsCoverageDebugger.LastDebugStepExpr : TExprBase;
begin
   Result:=FLastDebugStepExpr;
end;

// DebugMessage
//
procedure TdwsCoverageDebugger.DebugMessage(const msg : UnicodeString);
begin
   if Assigned(FDebugger) then
      FDebugger.DebugMessage(msg);
end;

// NotifyException
//
procedure TdwsCoverageDebugger.NotifyException(exec : TdwsExecution; const exceptObj : IScriptObj);
begin
   if Assigned(FDebugger) then
      FDebugger.NotifyException(exec, exceptObj);
end;

// SetProgram
//
procedure TdwsCoverageDebugger.SetProgram(const val : IdwsProgram);
begin
   FProgram:=val;
   if val<>nil then
      ResetCoverage;
end;

// ResetCoverage
//
procedure TdwsCoverageDebugger.ResetCoverage;
begin
   FreeAndNil(FAllLines);
   FreeAndNil(FNonCovered);

   if FProgram<>nil then begin
      FAllLines:=TdwsBreakpointableLines.Create(FProgram);
      FNonCovered:=TdwsBreakpointableLines.Create(FProgram);
   end;
end;

// CoverageStatus
//
function TdwsCoverageDebugger.CoverageStatus(const sourceName : UnicodeString; line : Integer) : TCoverageStatus;
var
   bits : TBits;
begin
   if FAllLines=nil then
      Exit(csUnknown);

   bits:=FAllLines.SourceLines[sourceName];
   if bits=nil then
      Exit(csUnknown);

   if line>=bits.Size then
      Exit(csNotRunnable);
   if bits[line] then begin
      if FNonCovered.SourceLines[sourceName].Bits[line] then
         Result:=csNotCovered
      else Result:=csCovered;
   end else Result:=csNotRunnable;
end;

// CreateReport
//
function TdwsCoverageDebugger.CreateReport : TdwsCodeCoverageReport;

   function CountBits(b : TBits) : Integer;
   var
      i : Integer;
   begin
      Result:=0;
      for i:=0 to b.Size-1 do
         if b[i] then Inc(Result);
   end;

var
   i : Integer;
   lsAll : TStringList;
begin
   Result:=TdwsCodeCoverageReport.Create;
   if FAllLines=nil then Exit;

   lsAll:=TStringList.Create;
   try
      FAllLines.Enumerate(lsAll);
      lsAll.Sort;
      SetLength(Result.FEntries, lsAll.Count);
      for i:=0 to lsAll.Count-1 do begin
         Result.FEntries[i].SourceName:=lsAll[i];
         Result.FEntries[i].Runnable:=CountBits(FAllLines.SourceLines[lsAll[i]]);
         Result.FEntries[i].NonCovered:=CountBits(FNonCovered.SourceLines[lsAll[i]]);
      end;
   finally
      lsAll.Free;
   end;
end;

// HasReport
//
function TdwsCoverageDebugger.HasReport : Boolean;
begin
   Result:=(FAllLines<>nil);
end;

end.
