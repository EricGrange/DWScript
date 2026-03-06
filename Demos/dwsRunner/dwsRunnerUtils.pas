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
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsRunnerUtils;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  dwsXPlatform,
  dwsComp,
  dwsCompiler,
  dwsExprs,
  dwsSymbols,
  dwsUtils,
  dwsFunctions,
  SynZip,
  dwsMathFunctions,
  dwsStringFunctions,
  dwsTimeFunctions,
  dwsVariantFunctions,
  dwsFileFunctions,
  dwsBigIntegerFunctions.GMP,
  dwsMPIR.Bundle,
  dwsClassesLibModule,
  dwsZipLibModule,
  dwsEncodingLibModule,
  dwsCryptoLibModule,
  dwsWebLibModule,
  dwsDatabaseLibModule,
  dwsSystemInfoLibModule,
  dwsKernelCompilerLibModule,
  dwsGraphicLibrary,
  dwsTurboJPEG.Bundle,
  dwsComConnector,
  dwsJSONConnector,
  dwsSynSQLiteDatabase,
  dwsErrors,
  dwsScriptSource,
  dwsCompilerContext,
  dwsSampling,
  dwsRunnerProject;

procedure WriteHeader;
procedure ConfigureScript(script: TDelphiWebScript; defines: TStrings; timeoutMs: Integer);
function CreateScript(defines: TStrings; timeoutMs: Integer) : TDelphiWebScript;

procedure MakeExe(paramOffset : Integer);
procedure DoCheck(project : TRunnerProject; defines: TStrings; timeoutMs: Integer);
procedure DoDeps(project : TRunnerProject; defines: TStrings);
procedure DoProfile(project : TRunnerProject; paramOffset : Integer; defines: TStrings; interval : Integer);
procedure RunRunner(project : TRunnerProject; paramOffset : Integer; embedded : Boolean; defines: TStrings; timeoutMs: Integer);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ConfigureScript
//
procedure ConfigureScript(script: TDelphiWebScript; defines: TStrings; timeoutMs: Integer);
begin
   if defines <> nil then
      script.Config.Conditionals.AddStrings(defines);
   script.Config.TimeoutMilliseconds := timeoutMs;
   script.Config.CompilerOptions := script.Config.CompilerOptions + [coSymbolDictionary];
end;

// CreateScript
//
function CreateScript(defines: TStrings; timeoutMs: Integer) : TDelphiWebScript;
begin
   Result:=TDelphiWebScript.Create(nil);

   TdwsComConnector.Create(Result).Script:=Result;
   TdwsJSONLibModule.Create(Result).Script:=Result;
   TdwsClassesLib.Create(Result).dwsUnit.Script:=Result;
   TdwsEncodingLib.Create(Result).dwsEncoding.Script:=Result;
   TdwsCryptoLib.Create(Result).dwsCrypto.Script:=Result;
   TdwsZipLib.Create(Result).dwsZip.Script:=Result;
   TdwsWebLib.Create(Result).dwsWeb.Script:=Result;
   TdwsDatabaseLib.Create(Result).dwsDatabase.Script:=Result;
   TdwsSystemInfoLibModule.Create(Result).Script:=Result;
   TdwsKernelCompilerLib.Create(Result).dwsKernelCompilerUnit.Script:=Result;

   ConfigureScript(Result, defines, timeoutMs);
end;

// WriteHeader
//
procedure WriteHeader;
begin
   Writeln('dwsRunner - sample code runner for DWScript');
   Writeln('');
end;

{$WARN SYMBOL_PLATFORM OFF}

// MakeExe
//
procedure MakeExe(paramOffset : Integer);
var
   zw : TZipWrite;
   sourceName, zipFileName, exeName : String;
   hUpdate : THandle;
   buf : RawByteString;
   fs : TFileStream;
   zip : TZipProject;
   prog : IdwsProgram;
   script : TDelphiWebScript;
   searchRec : TSearchRec;
   found : Integer;
begin
   WriteHeader;

   if paramOffset > ParamCount then begin
      Writeln('Missing source file for make');
      Exit;
   end;

   sourceName:=ParamStr(paramOffset);
   WriteLn('...Starting make for "', sourceName, '"');

   if not StrEndsWith(sourceName, '.zip') then begin
      zipFileName:=ChangeFileExt(sourceName, '.zip');
      WriteLn('...Zipping to "', zipFileName, '"');
      zw:=TZipWrite.Create(zipFileName);
      try
         if DirectoryExists(sourceName) then begin
            sourceName:=IncludeTrailingPathDelimiter(sourceName);
            found:=FindFirst(sourceName+'*.*', faArchive or faReadOnly, searchRec);
            while found=0 do begin
               zw.AddDeflated(sourceName+searchRec.Name, True, 9);
               found:=FindNext(searchRec);
            end;
            FindClose(searchRec);
         end else zw.AddDeflated(sourceName, True, 9);
      finally
         zw.Free;
      end;
   end else begin
      zipFileName:=sourceName;
   end;
   if paramOffset < ParamCount then
      exeName:=ParamStr(paramOffset + 1)
   else exeName:=ChangeFileExt(zipFileName, '.exe');

   zip:=TZipProject.Create(zipFileName);
   script:=CreateScript(nil, 0);
   try
      prog:=script.Compile(zip.Attach(script));
      try
         if prog.Msgs.Count>0 then begin
            WriteLn('...Compiled with ', prog.Msgs.Count, ' message(s):');
            WriteLn(prog.Msgs.AsInfo);
            if prog.Msgs.HasErrors then begin
               Write('...Generation aborted');
               Exit;
            end;
         end else begin
            WriteLn('...Compiled without errors.');
         end;
      finally
         prog:=nil;
      end;
   finally
      script.Free;
      zip.Free;
   end;

   if not FileCopy(ParamStr(0), exeName, False) then begin
      Writeln('...Failed to create "', exeName, '"');
   end;

   fs:=TFileStream.Create(zipFileName, fmOpenRead or fmShareDenyNone);
   try
      SetLength(buf, fs.Size);
      if Length(buf)<>0 then
         fs.Read(buf[1], Length(buf));
   finally
      fs.Free;
   end;

   hUpdate:=BeginUpdateResource(PChar(exeName), False);
   try
      UpdateResource(hUpdate, RT_RCDATA, 'SCRIPT', 0, Pointer(buf), Length(buf));
   finally
      EndUpdateResource(hUpdate, False);
   end;

   WriteLn('..."', exeName, '" generated successfully!');
end;

// DoCheck
//
procedure DoCheck(project : TRunnerProject; defines: TStrings; timeoutMs: Integer);
var
   script : TDelphiWebScript;
   prog : IdwsProgram;
   source : String;
begin
   script:=CreateScript(defines, timeoutMs);
   try
      source:=project.Attach(script);
      prog:=script.Compile(source);

      if prog.Msgs.Count>0 then begin
         Writeln(prog.Msgs.AsInfo);
         if prog.Msgs.HasErrors then begin
            Writeln('...Check failed with errors.');
            ExitCode:=1;
         end else begin
            Writeln('...Check passed with warnings/hints.');
         end;
      end else begin
         Writeln('...Check passed (no errors or warnings).');
      end;
   finally
      prog:=nil;
      script.Free;
   end;
end;

// DoDeps
//
procedure DoDeps(project : TRunnerProject; defines: TStrings);
var
   script : TDelphiWebScript;
   prog : IdwsProgram;
   source : String;
   i : Integer;
   srcList : TScriptSourceList;
   item : TScriptSourceItem;
begin
   script:=CreateScript(defines, 0);
   try
      source:=project.Attach(script);
      prog:=script.Compile(source);

      if prog.Msgs.HasErrors then begin
         Writeln(prog.Msgs.AsInfo);
         Writeln('...Compilation failed, dependency list may be incomplete.');
         ExitCode:=1;
      end;

      srcList := prog.GetSourceList;
      for i := 0 to srcList.Count - 1 do begin
         item := srcList[i];
         if item.SourceType in [stUnit, stInclude] then begin
            if item.SourceFile.Location <> '' then
               Writeln(item.SourceFile.Location)
            else Writeln(item.SourceFile.Name);
         end;
      end;
   finally
      prog:=nil;
      script.Free;
   end;
end;

// WriteProfileReport
//
procedure WriteProfileReport(samplings : TdwsSamplings);
type
   TProfileItem = record
      Name : String;
      Count : Integer;
   end;
var
   i, j : Integer;
   sample : TdwsSample;
   total : Integer;
   report : array of TProfileItem;
   temp : TProfileItem;
begin
   total := 0;
   for i := 0 to samplings.Count - 1 do
      Inc(total, samplings[i].Count);

   if total = 0 then begin
      Writeln('No samples collected.');
      Exit;
   end;

   SetLength(report, samplings.Count);
   for i := 0 to samplings.Count - 1 do begin
      sample := samplings[i];
      if sample.Line > 0 then
         report[i].Name := Format('%s:%d (%s)', [sample.SourceName, sample.Line, sample.FuncName])
      else report[i].Name := Format('%s (%s)', [sample.SourceName, sample.FuncName]);
      report[i].Count := sample.Count;
   end;

   // Sort by count descending
   for i := 0 to Length(report) - 2 do
      for j := i + 1 to Length(report) - 1 do
         if report[i].Count < report[j].Count then begin
            temp := report[i];
            report[i] := report[j];
            report[j] := temp;
         end;

   Writeln(Format('%-50s %-10s %-10s', ['Function/Line', 'Samples', 'Percentage']));
   Writeln(StringOfChar('-', 72));

   for i := 0 to Length(report) - 1 do begin
      Writeln(Format('%-50s %-10d %-10.1f%%', 
         [report[i].Name, report[i].Count, (report[i].Count / total) * 100]));
   end;
end;

// DoProfile
//
procedure DoProfile(project : TRunnerProject; paramOffset : Integer; defines: TStrings; interval : Integer);
var
   script : TDelphiWebScript;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   source : String;
   params : TVariantDynArray;
   i : Integer;
   profiler : TdwsSamplingDebugger;
begin
   script:=CreateScript(defines, 0);
   profiler := TdwsSamplingDebugger.Create(nil);
   profiler.SamplingInterval := interval;
   try
      source:=project.Attach(script);
      prog:=script.Compile(source);

      if prog.Msgs.HasErrors then begin
         Writeln(prog.Msgs.AsInfo);
         Exit;
      end;

      SetLength(params, ParamCount-paramOffset+2);
      params[0]:=ParamStr(0);
      for i:=paramOffset to ParamCount do
         params[i-paramOffset+1]:=ParamStr(i);

      exec:=prog.CreateNewExecution;
      exec.Debugger := profiler;
      exec.ExecuteParam(params);

      Writeln('Profiling results:');
      WriteProfileReport(profiler.Samplings);
      
   finally
      profiler.Free;
      exec:=nil;
      prog:=nil;
      script.Free;
   end;
end;

// RunRunner
//
procedure RunRunner(project : TRunnerProject; paramOffset : Integer; embedded : Boolean; defines: TStrings; timeoutMs: Integer);
var
   script : TDelphiWebScript;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   source : String;
   params : TVariantDynArray;
   i : Integer;
begin
   script:=CreateScript(defines, timeoutMs);
   try
      source:=project.Attach(script);

      prog:=script.Compile(source);

      if prog.Msgs.Count>0 then begin
         if prog.Msgs.HasErrors or not embedded then
            Writeln(prog.Msgs.AsInfo);
         if prog.Msgs.HasErrors then begin
            ExitCode:=1;
            Exit;
         end;
      end;

      SetLength(params, ParamCount-paramOffset+2);
      params[0]:=ParamStr(0);
      for i:=paramOffset to ParamCount do
         params[i-paramOffset+1]:=ParamStr(i);

      exec:=prog.ExecuteParam(params);
      Writeln(exec.Result.ToString);
      if exec.Msgs.Count>0 then
         Writeln(exec.Msgs.AsInfo);
      
   finally
      exec:=nil;
      prog:=nil;
      script.Free;
   end;
end;

end.
