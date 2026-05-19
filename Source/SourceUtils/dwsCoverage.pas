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
   System.Classes, System.SysUtils, System.Generics.Collections,
   dwsUtils, dwsSymbols, dwsDebugger, dwsExprs, dwsErrors, dwsXPlatform,
   dwsScriptSource, dwsXXHash, dwsUnitSymbols;

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
         FLastSourceFile : TSourceFile;
         FLastBits : TBits;
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

         procedure GetCoverageFiles(destList : TStrings);
         procedure GetCoverageLines(const fileName : String; var breakpointable, covered : TInt64DynArray);

   end;

   // --------------------------------------------------
   // CCG coverage format support
   // --------------------------------------------------

   TdwsFuncCoverageKind = (fckFunc, fckMeth);

   TdwsFuncCoverageInfo = record
      DisplayName : String;
      Kind        : TdwsFuncCoverageKind;
      SourceKey   : String;   // lowercase key into FAllLines / FNonCovered
      StartLine   : Integer;
      EndLine     : Integer;
   end;

   TdwsSourceInfoRec = record
      Name     : String;   // display name
      Location : String;   // file path / location
   end;

   TdwsCoverageAggregate = class;

   // Helper for collecting max line in a function body
   TFuncMaxLineCollector = class
      public
         MaxLine     : Integer;
         SourceFile  : TSourceFile;
         procedure EnumCallback(parent, expr : TExprBase; var abort : Boolean);
   end;

   // Thread-safe accumulator of coverage data across multiple executions
   TdwsCoverageAggregate = class
      private
         FAllLines    : TObjectDictionary<String, TBits>;   // owned
         FNonCovered  : TObjectDictionary<String, TBits>;   // owned
         FSourceInfo  : TDictionary<String, TdwsSourceInfoRec>;
         FFuncInfos   : TList<TdwsFuncCoverageInfo>;
         FKnownProgObjs : TList<TObject>;
         FLock        : TdwsCriticalSection;

         procedure EnumerateFuncInfosFromProg(const prog : IdwsProgram);
         procedure CollectFuncInfosFromTable(table : TSymbolTable; const namePrefix : String);
         procedure AddFuncInfo(funcSym : TFuncSymbol; const namePrefix : String);

      public
         constructor Create;
         destructor  Destroy; override;

         procedure EnsureProgram(const prog : IdwsProgram);
         procedure MergeExecution(coveredBits : TObjectDictionary<String, TBits>);
         procedure Reset;

         function CreateCCGReport(const projectName : String) : String;
         procedure GetStatusCounts(out covered, total : Int64);

         property AllLines   : TObjectDictionary<String, TBits> read FAllLines;
         property NonCovered : TObjectDictionary<String, TBits> read FNonCovered;
   end;

   // Lightweight IDebugger that records covered lines per execution
   TdwsCoverageExecutionTracker = class (TInterfacedObject, IDebugger)
      private
         FAggregate       : TdwsCoverageAggregate;
         FCoveredBits     : TObjectDictionary<String, TBits>;
         FLastSourceFile  : TSourceFile;
         FLastCoveredBits : TBits;
         FLastAllBits     : TBits;  // reference into FAggregate.FAllLines (not owned)

      protected
         // IDebugger
         procedure StartDebug(exec : TdwsExecution);
         procedure DoDebug(exec : TdwsExecution; expr : TExprBase);
         procedure StopDebug(exec : TdwsExecution);
         procedure EnterFunc(exec : TdwsExecution; funcExpr : TExprBase);
         procedure LeaveFunc(exec : TdwsExecution; funcExpr : TExprBase);
         function  LastDebugStepExpr : TExprBase;
         procedure DebugMessage(const msg : UnicodeString);
         procedure NotifyException(exec : TdwsExecution; const exceptObj : IScriptObj);

      public
         constructor Create(aggregate : TdwsCoverageAggregate);
         destructor  Destroy; override;

         property CoveredBits : TObjectDictionary<String, TBits> read FCoveredBits;
   end;

function CompressLineRanges(lines : TList<Integer>) : String;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

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
   FLastSourceFile := nil;
   FLastBits := nil;

   SetProgram((exec as TdwsProgramExecution).Prog);

   if Assigned(FDebugger) then
      FDebugger.StartDebug(exec);
end;

// DoDebug
//
procedure TdwsCoverageDebugger.DoDebug(exec : TdwsExecution; expr : TExprBase);

   function BitsFromSourceFile(sourceFile : TSourceFile) : TBits;
   var
      loc, lcLoc : String;
   begin
      loc := sourceFile.Name;
      if loc = '' then
         loc := sourceFile.Location;
      UnicodeLowerCase(loc, lcLoc);
      Result := FNonCovered.SourceLines[lcLoc];
      FLastSourceFile := sourceFile;
      FLastBits := Result;
   end;

var
   p : TScriptPos;
   bits : TBits;
begin
   FLastDebugStepExpr := expr;

   p := expr.ScriptPos;
   if p.SourceFile <> nil then begin
      if p.SourceFile <> FLastSourceFile then
         bits := BitsFromSourceFile(p.SourceFile)
      else bits := FLastBits;
      if (bits <> nil) and (p.Line < bits.Size) then
         bits[p.Line] := False;
   end;

   if Assigned(FDebugger) then
      FDebugger.DoDebug(exec, expr);
end;

// StopDebug
//
procedure TdwsCoverageDebugger.StopDebug(exec : TdwsExecution);
begin
   FLastDebugStepExpr := nil;
   FLastSourceFile := nil;
   FLastBits := nil;
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
   if FProgram <> val then begin
      FProgram := val;
      if val <> nil then
         ResetCoverage;
   end;
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
   begin
      Result := 0;
      for var i := 0 to b.Size-1 do
         if b[i] then Inc(Result);
   end;

begin
   Result:=TdwsCodeCoverageReport.Create;
   if FAllLines=nil then Exit;

   var lsAll := TStringList.Create;
   try
      FAllLines.Enumerate(lsAll);
      lsAll.Sort;
      SetLength(Result.FEntries, lsAll.Count);
      for var i := 0 to lsAll.Count-1 do begin
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

// GetCoverageFiles
//
procedure TdwsCoverageDebugger.GetCoverageFiles(destList : TStrings);
begin
   if FAllLines <> nil then
      FAllLines.Enumerate(destList);
end;

// GetCoverageLines
//
procedure TdwsCoverageDebugger.GetCoverageLines(const fileName : String; var breakpointable, covered : TInt64DynArray);
begin
   var bitsAlls := FAllLines.SourceLines[fileName];
   var bitsNonCovered := FNonCovered.SourceLines[fileName];
   var nBP := 0;
   var nCovered := 0;
   SetLength(breakpointable, bitsAlls.Size);
   SetLength(covered, bitsAlls.Size);
   for var i := 0 to bitsAlls.Size-1 do begin
      if not bitsAlls[i] then continue;
      breakpointable[nBP] := i;
      Inc(nBP);
      if not bitsNonCovered[i] then begin
         covered[nCovered] := i;
         Inc(nCovered);
      end;
   end;
   SetLength(breakpointable, nBP);
   SetLength(covered, nCovered);
end;

// ------------------
// ------------------ TFuncMaxLineCollector ------------------
// ------------------

procedure TFuncMaxLineCollector.EnumCallback(parent, expr : TExprBase; var abort : Boolean);
var
   p : TScriptPos;
begin
   p := expr.ScriptPos;
   if (p.SourceFile <> nil) and (p.SourceFile = SourceFile) then
      if p.Line > MaxLine then
         MaxLine := p.Line;
end;

// ------------------
// ------------------ TdwsCoverageAggregate ------------------
// ------------------

// Create
//
constructor TdwsCoverageAggregate.Create;
begin
   inherited;
   FAllLines      := TObjectDictionary<String, TBits>.Create([doOwnsValues]);
   FNonCovered    := TObjectDictionary<String, TBits>.Create([doOwnsValues]);
   FSourceInfo    := TDictionary<String, TdwsSourceInfoRec>.Create;
   FFuncInfos     := TList<TdwsFuncCoverageInfo>.Create;
   FKnownProgObjs := TList<TObject>.Create;
   FLock          := TdwsCriticalSection.Create;
end;

// Destroy
//
destructor TdwsCoverageAggregate.Destroy;
begin
   FLock.Free;
   FKnownProgObjs.Free;
   FFuncInfos.Free;
   FSourceInfo.Free;
   FNonCovered.Free;
   FAllLines.Free;
   inherited;
end;

// EnsureProgram
//
procedure TdwsCoverageAggregate.EnsureProgram(const prog : IdwsProgram);
var
   progObj    : TObject;
   bpLines    : TdwsBreakpointableLines;
   srcList    : TStringList;
   i          : Integer;
   srcName    : String;
   srcBits    : TBits;
   existBits  : TBits;
   newBits    : TBits;
   ncBits     : TBits;
   infoRec    : TdwsSourceInfoRec;
begin
   progObj := prog.ProgramObject;
   FLock.Enter;
   try
      // Deduplicate: skip if this program object was already seen
      if FKnownProgObjs.IndexOf(progObj) >= 0 then
         Exit;
      FKnownProgObjs.Add(progObj);

      // Build name→location map from program's source list
      var srcLocMap := TDictionary<String, String>.Create;
      try
         var sourceList := prog.SourceList;
         for var si := 0 to sourceList.Count - 1 do begin
            var sf := sourceList[si].SourceFile;
            if sf <> nil then
               srcLocMap.AddOrSetValue(LowerCase(sf.Name), sf.Location);
         end;

         // Build breakpointable lines for this program
         bpLines := TdwsBreakpointableLines.Create(prog);
         try
            srcList := TStringList.Create;
            try
               bpLines.Enumerate(srcList);
               for i := 0 to srcList.Count - 1 do begin
                  srcName := LowerCase(srcList[i]);
                  srcBits := srcList.Objects[i] as TBits;

                  if not FAllLines.TryGetValue(srcName, existBits) then begin
                     // New source: copy bits into FAllLines and FNonCovered
                     newBits := TBits.Create;
                     newBits.Size := srcBits.Size;
                     for var b := 0 to srcBits.Size - 1 do
                        if srcBits[b] then
                           newBits[b] := True;
                     FAllLines.Add(srcName, newBits);

                     // Non-covered starts as full copy of all lines
                     newBits := TBits.Create;
                     newBits.Size := srcBits.Size;
                     for var b := 0 to srcBits.Size - 1 do
                        if srcBits[b] then
                           newBits[b] := True;
                     FNonCovered.Add(srcName, newBits);

                     // Record source info: Name = display name, Location = file path
                     infoRec.Name := srcList[i];
                     if not srcLocMap.TryGetValue(srcName, infoRec.Location) then
                        infoRec.Location := srcList[i];
                     FSourceInfo.AddOrSetValue(srcName, infoRec);
                  end else begin
                     // Existing source: OR new executable bits into FAllLines
                     // For FNonCovered: only add new bits (don't uncover already-covered lines)
                     if srcBits.Size > existBits.Size then
                        existBits.Size := srcBits.Size;
                     FNonCovered.TryGetValue(srcName, ncBits);
                     if (ncBits <> nil) and (srcBits.Size > ncBits.Size) then
                        ncBits.Size := srcBits.Size;

                     for var b := 0 to srcBits.Size - 1 do begin
                        if srcBits[b] and not existBits[b] then begin
                           // New executable line
                           existBits[b] := True;
                           if ncBits <> nil then
                              ncBits[b] := True;
                        end;
                     end;
                  end;
               end;
            finally
               srcList.Free;
            end;
         finally
            bpLines.Free;
         end;
      finally
         srcLocMap.Free;
      end;

      // Enumerate function infos for this program
      EnumerateFuncInfosFromProg(prog);
   finally
      FLock.Leave;
   end;
end;

// MergeExecution
//
procedure TdwsCoverageAggregate.MergeExecution(coveredBits : TObjectDictionary<String, TBits>);
var
   ncBits : TBits;
   i      : Integer;
begin
   if coveredBits = nil then Exit;
   FLock.Enter;
   try
      for var pair in coveredBits do begin
         if FNonCovered.TryGetValue(pair.Key, ncBits) then begin
            for i := 0 to pair.Value.Size - 1 do
               if pair.Value[i] and (i < ncBits.Size) then
                  ncBits[i] := False;
         end;
      end;
   finally
      FLock.Leave;
   end;
end;

// Reset
//
procedure TdwsCoverageAggregate.Reset;
var
   allBits, ncBits : TBits;
begin
   FLock.Enter;
   try
      // Restore non-covered to full copy of all lines
      for var pair in FAllLines do begin
         allBits := pair.Value;
         if FNonCovered.TryGetValue(pair.Key, ncBits) then begin
            ncBits.Size := allBits.Size;
            for var i := 0 to allBits.Size - 1 do
               ncBits[i] := allBits[i];
         end;
      end;
      // Force re-enumeration of functions on next EnsureProgram
      FFuncInfos.Clear;
      FKnownProgObjs.Clear;
   finally
      FLock.Leave;
   end;
end;

// EnumerateFuncInfosFromProg
//
procedure TdwsCoverageAggregate.EnumerateFuncInfosFromProg(const prog : IdwsProgram);
var
   unitMains : TUnitMainSymbols;
   i         : Integer;
   ums       : TUnitMainSymbol;
begin
   // Walk unit main symbols
   unitMains := prog.UnitMains;
   for i := 0 to unitMains.Count - 1 do begin
      ums := unitMains[i];
      CollectFuncInfosFromTable(ums.Table, '');
      if ums.ImplementationTable <> nil then
         CollectFuncInfosFromTable(ums.ImplementationTable, '');
   end;

   // Walk the main program table
   CollectFuncInfosFromTable(prog.Table, '');
end;

// CollectFuncInfosFromTable
//
procedure TdwsCoverageAggregate.CollectFuncInfosFromTable(table : TSymbolTable; const namePrefix : String);
var
   sym       : TSymbol;
   funcSym   : TFuncSymbol;
   structSym : TStructuredTypeSymbol;
begin
   for sym in table do begin
      funcSym := sym.AsFuncSymbol;
      if funcSym <> nil then begin
         AddFuncInfo(funcSym, namePrefix);
      end else if sym is TStructuredTypeSymbol then begin
         structSym := TStructuredTypeSymbol(sym);
         // Methods of this struct (no namePrefix - they use StructSymbol.Name)
         CollectFuncInfosFromTable(structSym.Members, '');
      end;
   end;
end;

// AddFuncInfo
//
procedure TdwsCoverageAggregate.AddFuncInfo(funcSym : TFuncSymbol; const namePrefix : String);
var
   execSelf    : TObject;
   proc        : TdwsProcedure;
   implPos     : TScriptPos;
   displayName : String;
   key         : String;
   info        : TdwsFuncCoverageInfo;
   collector   : TFuncMaxLineCollector;
begin
   if funcSym.Executable = nil then Exit;
   execSelf := funcSym.Executable.GetSelf;
   if not (execSelf is TdwsProcedure) then Exit;
   proc := TdwsProcedure(execSelf);

   implPos := funcSym.ImplementationPosition;
   if implPos.SourceFile = nil then Exit;

   // Build display name
   if funcSym is TMethodSymbol then begin
      var methSym := TMethodSymbol(funcSym);
      displayName := methSym.StructSymbol.Name + '.' + funcSym.Name;
      info.Kind := fckMeth;
   end else begin
      if funcSym.IsLambda then begin
         if namePrefix <> '' then
            displayName := namePrefix + '.<lambda>'
         else displayName := '<lambda>';
      end else begin
         if namePrefix <> '' then
            displayName := namePrefix + '.' + funcSym.Name
         else displayName := funcSym.Name;
      end;
      info.Kind := fckFunc;
   end;

   // Compute source key
   var srcName := implPos.SourceFile.Name;
   if srcName = '' then
      srcName := implPos.SourceFile.Location;
   key := LowerCase(srcName);

   // Only track if we know this source
   if not FAllLines.ContainsKey(key) then Exit;

   // Walk steppable exprs of the proc to find max line
   collector := TFuncMaxLineCollector.Create;
   try
      collector.MaxLine    := implPos.Line;
      collector.SourceFile := implPos.SourceFile;

      proc.Expr.RecursiveEnumerateSubExprs(collector.EnumCallback);

      if proc.InitExpr.SubExprCount > 0 then
         proc.InitExpr.RecursiveEnumerateSubExprs(collector.EnumCallback);

      info.DisplayName := displayName;
      info.SourceKey   := key;
      info.StartLine   := implPos.Line;
      info.EndLine     := collector.MaxLine;
   finally
      collector.Free;
   end;

   FFuncInfos.Add(info);

   // Recurse into nested functions via the proc's local symbol table
   CollectFuncInfosFromTable(proc.Table, displayName);
end;

// GetStatusCounts
//
procedure TdwsCoverageAggregate.GetStatusCounts(out covered, total : Int64);
var
   allBits, ncBits : TBits;
   totalRunnable   : Int64;
   totalNonCovered : Int64;
   i               : Integer;
begin
   totalRunnable   := 0;
   totalNonCovered := 0;
   FLock.Enter;
   try
      for var pair in FAllLines do begin
         allBits := pair.Value;
         for i := 0 to allBits.Size - 1 do
            if allBits[i] then Inc(totalRunnable);
      end;
      for var pair in FNonCovered do begin
         ncBits := pair.Value;
         for i := 0 to ncBits.Size - 1 do
            if ncBits[i] then Inc(totalNonCovered);
      end;
   finally
      FLock.Leave;
   end;
   total   := totalRunnable;
   covered := totalRunnable - totalNonCovered;
end;

// CompressLineRanges
//
function CompressLineRanges(lines : TList<Integer>) : String;
var
   i, rangeStart, rangeEnd : Integer;
   sb : TStringBuilder;
begin
   if lines.Count = 0 then Exit('');

   lines.Sort;

   sb := TStringBuilder.Create;
   try
      rangeStart := lines[0];
      rangeEnd   := lines[0];

      for i := 1 to lines.Count - 1 do begin
         if lines[i] = rangeEnd + 1 then begin
            rangeEnd := lines[i];
         end else begin
            if sb.Length > 0 then sb.Append(', ');
            if rangeStart = rangeEnd then
               sb.Append(IntToStr(rangeStart))
            else
               sb.Append(IntToStr(rangeStart) + '-' + IntToStr(rangeEnd));
            rangeStart := lines[i];
            rangeEnd   := lines[i];
         end;
      end;
      // Flush last range
      if sb.Length > 0 then sb.Append(', ');
      if rangeStart = rangeEnd then
         sb.Append(IntToStr(rangeStart))
      else
         sb.Append(IntToStr(rangeStart) + '-' + IntToStr(rangeEnd));

      Result := sb.ToString;
   finally
      sb.Free;
   end;
end;

// CreateCCGReport
//
function TdwsCoverageAggregate.CreateCCGReport(const projectName : String) : String;
var
   sb             : TStringBuilder;
   allKeys        : TList<String>;
   key            : String;
   allBits, ncBits : TBits;
   totalRunnable   : Int64;
   totalNonCovered : Int64;
   unitRunnable    : Integer;
   unitNonCovered  : Integer;
   funcsForKey     : TList<TdwsFuncCoverageInfo>;
   fi              : TdwsFuncCoverageInfo;
   gapLines        : TList<Integer>;
   gaps            : String;
   i, j            : Integer;
   pct             : Double;
   infoRec         : TdwsSourceInfoRec;
begin
   FLock.Enter;
   try
      sb := TStringBuilder.Create;
      try
         // Compute global totals
         totalRunnable   := 0;
         totalNonCovered := 0;
         for var pair in FAllLines do begin
            allBits := pair.Value;
            for i := 0 to allBits.Size - 1 do
               if allBits[i] then Inc(totalRunnable);
         end;
         for var pair in FNonCovered do begin
            ncBits := pair.Value;
            for i := 0 to ncBits.Size - 1 do
               if ncBits[i] then Inc(totalNonCovered);
         end;

         // Header
         sb.AppendLine('PROJECT: ' + projectName);
         sb.AppendLine('TIMESTAMP: ' + FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', Now));
         if totalRunnable > 0 then
            pct := (totalRunnable - totalNonCovered) / totalRunnable * 100.0
         else
            pct := 100.0;
         sb.AppendLine(Format('TOTAL_COVERAGE: %.1f%%', [pct]));
         sb.AppendLine(Format('GLOBAL_STATS: %d/%d', [totalRunnable - totalNonCovered, totalRunnable]));

         // Collect and sort keys
         allKeys := TList<String>.Create;
         try
            for var pair in FAllLines do
               allKeys.Add(pair.Key);
            allKeys.Sort;

            for key in allKeys do begin
               // Compute unit coverage
               unitRunnable   := 0;
               unitNonCovered := 0;
               allBits := nil;
               ncBits  := nil;
               FAllLines.TryGetValue(key, allBits);
               FNonCovered.TryGetValue(key, ncBits);
               if allBits <> nil then
                  for i := 0 to allBits.Size - 1 do
                     if allBits[i] then Inc(unitRunnable);
               if ncBits <> nil then
                  for i := 0 to ncBits.Size - 1 do
                     if ncBits[i] then Inc(unitNonCovered);

               // Skip 100% covered units
               if unitNonCovered = 0 then Continue;

               // Get source info
               if not FSourceInfo.TryGetValue(key, infoRec) then begin
                  infoRec.Name     := key;
                  infoRec.Location := key;
               end;

               if unitRunnable > 0 then
                  pct := (unitRunnable - unitNonCovered) / unitRunnable * 100.0
               else
                  pct := 100.0;

               sb.AppendLine('');
               sb.AppendLine('UNIT: ' + infoRec.Name + ' | ' + infoRec.Location);
               sb.AppendLine(Format('  COVERAGE: %.1f%% (%d/%d lines)',
                  [pct, unitRunnable - unitNonCovered, unitRunnable]));

               // Collect funcs for this source key, sorted by StartLine
               funcsForKey := TList<TdwsFuncCoverageInfo>.Create;
               try
                  for j := 0 to FFuncInfos.Count - 1 do
                     if FFuncInfos[j].SourceKey = key then
                        funcsForKey.Add(FFuncInfos[j]);

                  funcsForKey.Sort(TComparer<TdwsFuncCoverageInfo>.Construct(
                     function(const L, R : TdwsFuncCoverageInfo) : Integer
                     begin
                        Result := L.StartLine - R.StartLine;
                     end
                  ));

                  gapLines := TList<Integer>.Create;
                  try
                     for fi in funcsForKey do begin
                        // Compute gaps for this function
                        gapLines.Clear;
                        if (allBits <> nil) and (ncBits <> nil) then begin
                           for i := fi.StartLine to fi.EndLine do begin
                              if (i < allBits.Size) and allBits[i] then
                                 if (i < ncBits.Size) and ncBits[i] then
                                    gapLines.Add(i);
                           end;
                        end;

                        if gapLines.Count = 0 then Continue;

                        gaps := CompressLineRanges(gapLines);
                        sb.AppendLine('');
                        if fi.Kind = fckMeth then
                           sb.AppendLine(Format('  METH: %d-%d %s',
                              [fi.StartLine, fi.EndLine, fi.DisplayName]))
                        else
                           sb.AppendLine(Format('  FUNC: %d-%d %s',
                              [fi.StartLine, fi.EndLine, fi.DisplayName]));
                        sb.AppendLine('    GAPS: ' + gaps);
                     end;
                  finally
                     gapLines.Free;
                  end;
               finally
                  funcsForKey.Free;
               end;
            end;
         finally
            allKeys.Free;
         end;

         Result := sb.ToString;
      finally
         sb.Free;
      end;
   finally
      FLock.Leave;
   end;
end;

// ------------------
// ------------------ TdwsCoverageExecutionTracker ------------------
// ------------------

// Create
//
constructor TdwsCoverageExecutionTracker.Create(aggregate : TdwsCoverageAggregate);
begin
   inherited Create;
   FAggregate   := aggregate;
   FCoveredBits := TObjectDictionary<String, TBits>.Create([doOwnsValues]);
end;

// Destroy
//
destructor TdwsCoverageExecutionTracker.Destroy;
begin
   FCoveredBits.Free;
   inherited;
end;

// StartDebug
//
procedure TdwsCoverageExecutionTracker.StartDebug(exec : TdwsExecution);
begin
   FLastSourceFile  := nil;
   FLastCoveredBits := nil;
   FLastAllBits     := nil;
end;

// DoDebug
//
procedure TdwsCoverageExecutionTracker.DoDebug(exec : TdwsExecution; expr : TExprBase);
var
   p        : TScriptPos;
   srcName  : String;
   key      : String;
   allBits  : TBits;
   covBits  : TBits;
begin
   p := expr.ScriptPos;
   if p.SourceFile = nil then Exit;

   if p.SourceFile <> FLastSourceFile then begin
      FLastSourceFile := p.SourceFile;
      srcName := p.SourceFile.Name;
      if srcName = '' then
         srcName := p.SourceFile.Location;
      key := LowerCase(srcName);

      // Get allBits for size reference (no lock - acceptable minor race)
      if not FAggregate.AllLines.TryGetValue(key, allBits) then begin
         FLastCoveredBits := nil;
         FLastAllBits     := nil;
         Exit;
      end;
      FLastAllBits := allBits;

      // Get or create covBits
      if not FCoveredBits.TryGetValue(key, covBits) then begin
         covBits := TBits.Create;
         covBits.Size := allBits.Size;
         FCoveredBits.Add(key, covBits);
      end;
      FLastCoveredBits := covBits;
   end;

   if (FLastCoveredBits <> nil) and (p.Line < FLastCoveredBits.Size) then
      FLastCoveredBits[p.Line] := True;
end;

// StopDebug
//
procedure TdwsCoverageExecutionTracker.StopDebug(exec : TdwsExecution);
begin
   // Nothing: caller merges via MergeExecution(tracker.CoveredBits)
end;

// EnterFunc
//
procedure TdwsCoverageExecutionTracker.EnterFunc(exec : TdwsExecution; funcExpr : TExprBase);
begin
   // no-op
end;

// LeaveFunc
//
procedure TdwsCoverageExecutionTracker.LeaveFunc(exec : TdwsExecution; funcExpr : TExprBase);
begin
   // no-op
end;

// LastDebugStepExpr
//
function TdwsCoverageExecutionTracker.LastDebugStepExpr : TExprBase;
begin
   Result := nil;
end;

// DebugMessage
//
procedure TdwsCoverageExecutionTracker.DebugMessage(const msg : UnicodeString);
begin
   // no-op
end;

// NotifyException
//
procedure TdwsCoverageExecutionTracker.NotifyException(exec : TdwsExecution; const exceptObj : IScriptObj);
begin
   // no-op
end;

end.
