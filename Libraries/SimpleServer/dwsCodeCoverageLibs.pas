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
unit dwsCodeCoverageLibs;

interface

uses
   System.Classes, System.SysUtils, System.Generics.Collections,
   dwsExprs, dwsSymbols, dwsMagicExprs, dwsUtils, dwsXPlatform,
   dwsUnitSymbols, dwsFunctions, dwsLanguageExtension, dwsComp,
   dwsCoverage;

type

   // Manages the coverage aggregate and provides script-callable interface
   TdwsCoverageManager = class
      private
         FAggregate   : TdwsCoverageAggregate;
         FProjectName : String;
         FEnabled     : Boolean;
         FExtension   : TdwsLanguageExtension;  // owned

      public
         constructor Create(const projectName : String);
         destructor  Destroy; override;

         function  AcquireTracker(const prog : IdwsProgram) : TdwsCoverageExecutionTracker;
         procedure ReleaseTracker(tracker : TdwsCoverageExecutionTracker);
         procedure Reset;
         function  GetCCGReport : String;
         function  GetStatusJSON : String;

         property Enabled     : Boolean read FEnabled;
         property ProjectName : String  read FProjectName;
         property Aggregate   : TdwsCoverageAggregate read FAggregate;
   end;

procedure RegisterCodeCoverageLib(script : TDelphiWebScript; manager : TdwsCoverageManager);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   SYS_CODECOVERAGE         = 'CodeCoverage';
   SYS_CODECOVERAGE_REPORT  = 'GetReport';
   SYS_CODECOVERAGE_RESET   = 'Reset';
   SYS_CODECOVERAGE_ENABLED = 'Enabled';
   SYS_CODECOVERAGE_STATUS  = 'StatusJSON';

// ------------------
// Magic function classes (defined in implementation so they can reference TdwsCoverageManager)
// ------------------

type

   TCodeCoverageGetReportFunc = class (TInternalMagicStringFunction)
      private
         FManager : TdwsCoverageManager;
      public
         constructor Create(table : TSystemSymbolTable; codecov : TClassSymbol; manager : TdwsCoverageManager);
         procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); override;
   end;

   TCodeCoverageResetProc = class (TInternalMagicProcedure)
      private
         FManager : TdwsCoverageManager;
      public
         constructor Create(table : TSystemSymbolTable; codecov : TClassSymbol; manager : TdwsCoverageManager);
         procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TCodeCoverageEnabledFunc = class (TInternalMagicBoolFunction)
      private
         FManager : TdwsCoverageManager;
      public
         constructor Create(table : TSystemSymbolTable; codecov : TClassSymbol; manager : TdwsCoverageManager);
         function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; override;
   end;

   TCodeCoverageStatusJSONFunc = class (TInternalMagicStringFunction)
      private
         FManager : TdwsCoverageManager;
      public
         constructor Create(table : TSystemSymbolTable; codecov : TClassSymbol; manager : TdwsCoverageManager);
         procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); override;
   end;

   // Language extension that registers the CodeCoverage static class
   TdwsCodeCoverageLanguageExtension = class (TdwsLanguageExtension)
      private
         FManager : TdwsCoverageManager;  // not owned

      public
         constructor Create(manager : TdwsCoverageManager); reintroduce;
         procedure CreateSystemSymbols(table : TSystemSymbolTable); override;
   end;

// ------------------
// TCodeCoverageGetReportFunc
// ------------------

constructor TCodeCoverageGetReportFunc.Create(table : TSystemSymbolTable; codecov : TClassSymbol; manager : TdwsCoverageManager);
begin
   FManager := manager;
   inherited Create(table, SYS_CODECOVERAGE_REPORT,
                    ['projectName=', SYS_STRING], SYS_STRING,
                    [iffStaticMethod],
                    codecov);
end;

procedure TCodeCoverageGetReportFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : String);
var
   projName : String;
begin
   args.ExprBase[0].EvalAsString(args.Exec, projName);
   if projName = '' then
      projName := FManager.ProjectName;
   Result := FManager.Aggregate.CreateCCGReport(projName);
end;

// ------------------
// TCodeCoverageResetProc
// ------------------

constructor TCodeCoverageResetProc.Create(table : TSystemSymbolTable; codecov : TClassSymbol; manager : TdwsCoverageManager);
begin
   FManager := manager;
   inherited Create(table, SYS_CODECOVERAGE_RESET,
                    [], '',
                    [iffStaticMethod],
                    codecov);
end;

procedure TCodeCoverageResetProc.DoEvalProc(const args : TExprBaseListExec);
begin
   FManager.Reset;
end;

// ------------------
// TCodeCoverageEnabledFunc
// ------------------

constructor TCodeCoverageEnabledFunc.Create(table : TSystemSymbolTable; codecov : TClassSymbol; manager : TdwsCoverageManager);
begin
   FManager := manager;
   inherited Create(table, SYS_CODECOVERAGE_ENABLED,
                    [], SYS_BOOLEAN,
                    [iffStateLess, iffStaticMethod],
                    codecov);
end;

function TCodeCoverageEnabledFunc.DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean;
begin
   Result := FManager.Enabled;
end;

// ------------------
// TCodeCoverageStatusJSONFunc
// ------------------

constructor TCodeCoverageStatusJSONFunc.Create(table : TSystemSymbolTable; codecov : TClassSymbol; manager : TdwsCoverageManager);
begin
   FManager := manager;
   inherited Create(table, SYS_CODECOVERAGE_STATUS,
                    [], SYS_STRING,
                    [iffStaticMethod],
                    codecov);
end;

procedure TCodeCoverageStatusJSONFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : String);
begin
   Result := FManager.GetStatusJSON;
end;

// ------------------
// TdwsCodeCoverageLanguageExtension
// ------------------

constructor TdwsCodeCoverageLanguageExtension.Create(manager : TdwsCoverageManager);
begin
   inherited Create;
   FManager := manager;
end;

procedure TdwsCodeCoverageLanguageExtension.CreateSystemSymbols(table : TSystemSymbolTable);
var
   codecovClass : TClassSymbol;
begin
   codecovClass := TClassSymbol.Create(SYS_CODECOVERAGE, nil);
   codecovClass.InheritFrom(table.TypObject);
   codecovClass.IsStatic  := True;
   codecovClass.IsSealed  := True;
   codecovClass.SetNoVirtualMembers;
   table.AddSymbol(codecovClass);

   TCodeCoverageGetReportFunc.Create(table, codecovClass, FManager);
   TCodeCoverageResetProc.Create(table, codecovClass, FManager);
   TCodeCoverageEnabledFunc.Create(table, codecovClass, FManager);
   TCodeCoverageStatusJSONFunc.Create(table, codecovClass, FManager);
end;

// ------------------
// TdwsCoverageManager
// ------------------

constructor TdwsCoverageManager.Create(const projectName : String);
begin
   inherited Create;
   FProjectName := projectName;
   FEnabled     := True;
   FAggregate   := TdwsCoverageAggregate.Create;
end;

destructor TdwsCoverageManager.Destroy;
begin
   FExtension.Free;
   FAggregate.Free;
   inherited;
end;

function TdwsCoverageManager.AcquireTracker(const prog : IdwsProgram) : TdwsCoverageExecutionTracker;
begin
   FAggregate.EnsureProgram(prog);
   Result := TdwsCoverageExecutionTracker.Create(FAggregate);
end;

procedure TdwsCoverageManager.ReleaseTracker(tracker : TdwsCoverageExecutionTracker);
begin
   if tracker <> nil then
      FAggregate.MergeExecution(tracker.CoveredBits);
end;

procedure TdwsCoverageManager.Reset;
begin
   FAggregate.Reset;
end;

function TdwsCoverageManager.GetCCGReport : String;
begin
   Result := FAggregate.CreateCCGReport(FProjectName);
end;

function TdwsCoverageManager.GetStatusJSON : String;
var
   allBits, ncBits  : TBits;
   totalRunnable    : Int64;
   totalNonCovered  : Int64;
   i                : Integer;
begin
   totalRunnable   := 0;
   totalNonCovered := 0;

   for var pair in FAggregate.AllLines do begin
      allBits := pair.Value;
      for i := 0 to allBits.Size - 1 do
         if allBits[i] then Inc(totalRunnable);
   end;
   for var pair in FAggregate.NonCovered do begin
      ncBits := pair.Value;
      for i := 0 to ncBits.Size - 1 do
         if ncBits[i] then Inc(totalNonCovered);
   end;

   var enabledStr := 'false';
   if FEnabled then enabledStr := 'true';
   Result := Format('{"enabled":%s,"project":%s,"covered":%d,"total":%d}',
      [enabledStr,
       '"' + StringReplace(FProjectName, '"', '\"', [rfReplaceAll]) + '"',
       totalRunnable - totalNonCovered,
       totalRunnable]);
end;

// ------------------
// RegisterCodeCoverageLib
// ------------------

procedure RegisterCodeCoverageLib(script : TDelphiWebScript; manager : TdwsCoverageManager);
var
   ext : TdwsCodeCoverageLanguageExtension;
begin
   ext := TdwsCodeCoverageLanguageExtension.Create(manager);
   // Store the extension in the manager so it gets freed with the manager
   manager.FExtension := ext;
   // Add to script's extension aggregator (not owned by aggregator)
   script.Extensions.Add(ext);
end;

end.
