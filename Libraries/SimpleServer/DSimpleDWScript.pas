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
unit DSimpleDWScript;

interface

{.$define LogCompiles}
{$define ALLOW_JIT}

{$if not Defined(WIN32)}
   {$undef ALLOW_JIT}
{$ifend}

uses
   Windows, SysUtils, Classes, StrUtils, Masks,
   dwsFileSystem, dwsGlobalVarsFunctions, dwsExprList,
   dwsCompiler, dwsHtmlFilter, dwsComp, dwsExprs, dwsUtils, dwsXPlatform,
   dwsJSONConnector, dwsJSON, dwsErrors, dwsUnitSymbols, dwsSymbols,
   {$ifdef ALLOW_JIT}
   dwsJIT, dwsJITx86,
   {$endif}
   dwsJSFilter, dwsJSLibModule, dwsCodeGen,
   dwsWebEnvironment, dwsSystemInfoLibModule, dwsCPUUsage, dwsWebLibModule,
   dwsWebServerHelpers, dwsZipLibModule, dwsIniFileModule,
   dwsDataBase, dwsDataBaseLibModule, dwsWebServerInfo, dwsWebServerLibModule,
   dwsBackgroundWorkersLibModule, dwsSynapseLibModule, dwsCryptoLibModule,
   dwsEncodingLibModule, dwsComConnector, dwsXXHash, dwsHTTPSysServer,
   dwsBigIntegerFunctions.GMP, dwsMPIR.Bundle, dwsCompilerContext, dwsFilter,
   dwsByteBufferFunctions;

type

   TCompiledProgram = record
      Name : String;
      Prog : IdwsProgram;
      Files : IAutoStrings;
      procedure Flush;
   end;

   TCompiledProgramHash = class (TSimpleHash<TCompiledProgram>)
      protected
         function SameItem(const item1, item2 : TCompiledProgram) : Boolean; override;
         function GetItemHashCode(const item1 : TCompiledProgram) : Cardinal; override;
   end;

   TDWSHandlingOption = (optWorker);
   TDWSHandlingOptions = set of TDWSHandlingOption;

   PIdwsProgramExecution = ^IdwsProgramExecution;
   PExecutingScript = ^TExecutingScript;
   TExecutingScript = record
      ID : Integer;
      Exec : PIdwsProgramExecution;
      Start : Int64;
      Options : TDWSHandlingOptions;
      Prev, Next : PExecutingScript;
   end;

   TSimpleDWScript = class(TDataModule)
      DelphiWebScript: TDelphiWebScript;
      dwsHtmlFilter: TdwsHtmlFilter;
      dwsGlobalVarsFunctions: TdwsGlobalVarsFunctions;
      dwsCompileSystem: TdwsRestrictedFileSystem;
      dwsRuntimeFileSystem: TdwsRestrictedFileSystem;
      dwsComConnector: TdwsComConnector;
      procedure DataModuleCreate(Sender: TObject);
      procedure DataModuleDestroy(Sender: TObject);

      procedure DoInclude(const scriptName: String; var scriptSource: String);
      function  DoNeedUnit(const unitName : String; var unitSource : String) : IdwsUnit;

   private
      FScriptTimeoutMilliseconds : Integer;
      FWorkerTimeoutMilliseconds : Integer;
      FCPUUsageLimit : Integer;
      FCPUAffinity : Cardinal;

      FPathVariables : TStrings;

      FSystemInfo : TdwsSystemInfoLibModule;
      FHotPath : String;
      FWebLib : TdwsWebLib;
      FDataBase : TdwsDatabaseLib;
      FJSON : TdwsJSONLibModule;
      FSynapse : TdwsSynapseLib;
      FWebServerLib : TdwsWebServerLib;
      FBkgndWorkers : TdwsBackgroundWorkersLib;

      FCompiledPrograms : TCompiledProgramHash;
      FCompiledProgramsLock : TMultiReadSingleWrite;

      FCompilerLock : TdwsCriticalSection;
      FCompilerFiles : IAutoStrings;

      FCodeGenLock : TdwsCriticalSection;
      FEnableJIT : Boolean;

      FExecutingID : Integer;
      FExecutingScripts : PExecutingScript;
      FExecutingScriptsLock : TMultiReadSingleWrite;

      FStartupScriptName : String;
      FShutdownScriptName : String;
      FBackgroundFileSystem : IdwsFileSystem;

      FErrorLogDirectory : String;

      FFlushMask : TMask;
      FCheckDirectoryChanges : ITimer;
      FLastCheckTime : TFileTime;

   protected
      FJSCompiler : TDelphiWebScript;
      FJSFilter : TdwsJSFilter;

      procedure TryAcquireDWS(const fileName : String; var prog : IdwsProgram);
      procedure CompileDWS(const fileName : String; var prog : IdwsProgram;
                           typ : TFileAccessType);

      procedure LogCompileErrors(const fileName : String; const msgs : TdwsMessageList);
      procedure LogCompilation(const fmt : String; const params : array of const);
      procedure LogExecutionTimeOut(const fileName : String);

      function FlushMatchingMask(const cp : TCompiledProgram) : TSimpleHashAction;

      procedure SetCPUUsageLimit(const val : Integer);
      procedure SetCPUAffinity(const val : Cardinal);

      function WaitForCPULimit : Boolean;

      procedure DoSourceFileStreamOpened(Sender : TObject; const fileName : TFileName; const mode : TdwsFileOpenMode);

      procedure DoBackgroundWork(const request : TWebRequest);

      class procedure Handle500(response : TWebResponse; msgs : TdwsMessageList); static;
      class procedure Handle503(response : TWebResponse); static;

      procedure CheckDirectoryChanges;

   public
      procedure Initialize(const serverInfo : IWebServerInfo);
      procedure Finalize;

      procedure HandleDWS(const fileName : String; typ : TFileAccessType;
                          request : TWebRequest; response : TWebResponse;
                          const options : TDWSHandlingOptions);
      procedure StopDWS;

      procedure HandleP2JS(const fileName : String; request : TWebRequest; response : TWebResponse);

      function CompilationInfoJSON(const sourceName : String; fileType : TFileAccessType) : String;
      function ExecutionInfoJSON(const sourceName : String; fileType : TFileAccessType) : String;
      function CompiledPrograms : TStringDynArray;
      procedure FlushDWSCache(const fileName : String = '');

      procedure LoadCPUOptions(options : TdwsJSONValue);
      procedure LoadDWScriptOptions(options : TdwsJSONValue);

      function ApplyPathVariables(const aPath : String) : String;
      procedure ApplyPathsVariables(paths : TdwsJSONValue; dest : TStrings);

      procedure Startup;
      procedure Shutdown;

      function LiveQueries : String;

      procedure LogError(const msg : String);

      property ScriptTimeoutMilliseconds : Integer read FScriptTimeoutMilliseconds write FScriptTimeoutMilliseconds;
      property WorkerTimeoutMilliseconds : Integer read FWorkerTimeoutMilliseconds write FWorkerTimeoutMilliseconds;

      property CPUUsageLimit : Integer read FCPUUsageLimit write SetCPUUsageLimit;
      property CPUAffinity : Cardinal read FCPUAffinity write SetCPUAffinity;
      property PathVariables : TStrings read FPathVariables;

      property StartupScriptName : String read FStartupScriptName write FStartupScriptName;
      property ShutdownScriptName : String read FShutdownScriptName write FShutdownScriptName;

      property ErrorLogDirectory : String read FErrorLogDirectory write FErrorLogDirectory;
  end;

const
   cDefaultCPUOptions =
      '{'
         // Percentage (0-100) of total system usage
         // f.i. 25 on a quad core is reached by a full core and 3 cores idle
         // zero = no limit
         +'"UsageLimit":0,'
         // Core affinity
         // bitwise CPU affinity flags
         // zero = no affinity
         +'"Affinity":0'
      +'}';

   cDefaultDWScriptOptions =
      '{'
         // Script timeout in milliseconds
         // zero = no limit (not recommended)
         +'"TimeoutMSec": 3000,'
         +'"WorkerTimeoutMSec": 30000,'
         // Size of stack growth chunks
         +'"StackChunkSize": 300,'
         // Maximum stack size per script
         +'"StackMaxSize": 10000,'
         // Maximum recursion depth per script
         +'"MaxRecursionDepth": 512,'
         // Library paths (outside of www folder)
         // By default, assumes a '.lib' subfolder of the folder where the exe is
         +'"LibraryPaths": ["%www%\\.lib"],'
         // Paths which scripts are allowed to perform file operations on
         +'"WorkPaths": ["%www%"],'
         // Conditional Defines that should be preset
         +'"Conditionals": [],'
         // HTML Filter patterns
         +'"PatternOpen": "<?pas",'
         +'"PatternEval": "=",'
         +'"PatternClose": "?>",'
         // Maximum background worker count allowed per work queue
         +'"MaxWorkersPerQueue": 32,'
         // Startup Script Name
         +'"Startup": "%www%\\.startup.pas",'
         // Shutdown Script Name
         +'"Shutdown": "%www%\\.shutdown.pas",'
         // Turns on/off JIT compilation
         +'"JIT": true,'
         // Turns on/off COM support (breaks sandboxing!)
         +'"COM": false'
      +'}';

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$R *.dfm}

const
   cCheckDirectoryChangesInterval = 1000;

procedure TCompiledProgram.Flush;
begin
   Prog := nil;
   Files := nil;
end;

procedure TSimpleDWScript.DataModuleCreate(Sender: TObject);
var
   cryptoLib : TdwsCryptoLib;
begin
   // attempt to minimize probability of ID collisions between server restarts
   FExecutingID:=SimpleIntegerHash(GetTickCount);

   FPathVariables:=TFastCompareTextList.Create;

   FSystemInfo:=TdwsSystemInfoLibModule.Create(Self);
   FSystemInfo.Script:=DelphiWebScript;

   FDataBase:=TdwsDatabaseLib.Create(Self);
   FDataBase.dwsDatabase.Script:=DelphiWebScript;
   TdwsDataBase.OnApplyPathVariables:=ApplyPathVariables;

   FWebLib:=TdwsWebLib.Create(Self);
   FWebLib.dwsWeb.Script:=DelphiWebScript;

   FJSON:=TdwsJSONLibModule.Create(Self);
   FJSON.Script:=DelphiWebScript;

   FSynapse:=TdwsSynapseLib.Create(Self);
   FSynapse.Script:=DelphiWebScript;

   cryptoLib:=TdwsCryptoLib.Create(Self);
   cryptoLib.dwsCrypto.Script:=DelphiWebScript;
   cryptoLib.UseTemporaryStorageForNonces;

   TdwsEncodingLib.Create(Self).dwsEncoding.Script:=DelphiWebScript;

   TdwsZipLib.Create(Self).dwsZip.Script:=DelphiWebScript;

   TdwsIniFileLib.Create(Self).dwsIniFile.Script:=DelphiWebScript;

   FCompiledPrograms := TCompiledProgramHash.Create;
   FCompiledProgramsLock := TMultiReadSingleWrite.Create;

   FCompilerLock := TdwsCriticalSection.Create;
   FCodeGenLock := TdwsCriticalSection.Create;

   FExecutingScriptsLock:=TMultiReadSingleWrite.Create;

   FScriptTimeoutMilliseconds:=3000;
   FWorkerTimeoutMilliseconds:=30000;

   FJSFilter:=TdwsJSFilter.Create(Self);
   dwsHtmlFilter.SubFilter:=FJSFilter;
   FJSCompiler:=TDelphiWebScript.Create(Self);
   FJSFilter.Compiler:=FJSCompiler;
   FJSFilter.PatternOpen:='<script type="pascal">';
   FJSFilter.PatternClose:='</script>';
   FJSFilter.CodeGenPrefix:='<script>'#13#10;
   FJSFilter.CodeGenPostfix:=#13#10'</script>';
   TdwsJSLibModule.Create(Self).Script:=FJSCompiler;
   FJSCompiler.OnNeedUnit:=DoNeedUnit;
   FJSCompiler.OnInclude:=DoInclude;
   FJSCompiler.Config.CompileFileSystem:=dwsCompileSystem;
   FJSCompiler.Config.CompilerOptions:=[
      coOptimize, coAssertions, coSymbolDictionary, coContextMap, coExplicitUnitUses,
      coVariablesAsVarOnly, coAllowClosures
   ];
   FJSFilter.CodeGenOptions:=[
      cgoNoRangeChecks, cgoNoCheckInstantiated, cgoNoCheckLoopStep,
      cgoNoConditions, cgoNoSourceLocations,
      // cgoObfuscate, cgoOptimizeForSize,
      cgoSmartLink, cgoDeVirtualize, cgoNoRTTI
   ];

   dwsCompileSystem.OnFileStreamOpened := DoSourceFileStreamOpened;
end;

procedure TSimpleDWScript.DataModuleDestroy(Sender: TObject);
begin
   FlushDWSCache;

   FCodeGenLock.Free;
   FCompilerLock.Free;
   FCompiledProgramsLock.Free;
   FCompiledPrograms.Free;
   FPathVariables.Free;

   FExecutingScriptsLock.Free;
end;

// HandleDWS
//
procedure TSimpleDWScript.HandleDWS(const fileName : String; typ : TFileAccessType;
                                    request : TWebRequest; response : TWebResponse;
                                    const options : TDWSHandlingOptions);

   procedure HandleStaticFileWebResponse(response : TWebResponse);
   var
      fileName : String;
   begin
      fileName := ApplyPathVariables(UTF8ToString(response.ContentData));
      fileName :=  dwsRuntimeFileSystem.AllocateFileSystem.ValidateFileName(fileName);
      if fileName <> '' then begin
         response.ContentData := UTF8Encode(fileName);
      end else begin
         response.StatusCode := 501;
         response.ContentData := 'Unsupported server-side request';
         response.ContentType := 'text/plain';
      end;
   end;

   procedure HandleScriptResult(response : TWebResponse; scriptResult : TdwsResult);
   begin
      if (response.ContentEncoding = '') and StrBeginsWithA(response.ContentType, 'text/') then
         response.ContentData := scriptResult.ToUTF8String
      else response.ContentData := scriptResult.ToDataString;
   end;

var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   webenv : TWebEnvironment;
   executing : TExecutingScript;
   staticCache : TWebStaticCacheEntry;
begin
   if (CPUUsageLimit>0) and not WaitForCPULimit then begin
      Handle503(response);
      Exit;
   end;

   TryAcquireDWS(fileName, prog);
   if prog=nil then
      CompileDWS(fileName, prog, typ);

   if prog.Msgs.HasErrors then begin
      Handle500(response, prog.Msgs);
      Exit;
   end;

   if prog.ProgramObject.TagInterface <> nil then begin
      staticCache := prog.ProgramObject.TagInterface.GetSelf as TWebStaticCacheEntry;
      staticCache.HandleStatic(request, response);
      Exit;
   end;

   executing.Start := GetSystemMilliseconds;
   exec := prog.CreateNewExecution;
   executing.Exec := @exec;
   executing.Options := options;
   executing.ID := InterlockedIncrement(FExecutingID);

   webenv := TWebEnvironment.Create;
   webenv.WebRequest := request;
   webenv.WebResponse := response;
   exec.Environment := webenv;
   try
      exec.BeginProgram;

      // insert into executing queue
      executing.Prev:=nil;
      FExecutingScriptsLock.BeginWrite;
      executing.Next:=FExecutingScripts;
      if FExecutingScripts<>nil then
         FExecutingScripts.Prev:=@executing;
      FExecutingScripts:=@executing;
      FExecutingScriptsLock.EndWrite;

      if optWorker in options then
         exec.RunProgram(WorkerTimeoutMilliseconds)
      else exec.RunProgram(ScriptTimeoutMilliseconds);

      // extract from executing queue
      FExecutingScriptsLock.BeginWrite;
      if executing.Prev<>nil then
         executing.Prev.Next:=executing.Next
      else FExecutingScripts:=executing.Next;
      if executing.Next<>nil then
         executing.Next.Prev:=executing.Prev;
      FExecutingScriptsLock.EndWrite;

      exec.EndProgram;
   finally
      exec.Environment:=nil;
   end;

   if exec.Msgs.Count>0 then begin
      if exec.ExecutionTimedOut then
         LogExecutionTimeOut(fileName);
      Handle500(response, exec.Msgs);
   end else if response <> nil then begin
      if response.ContentData = '' then
         HandleScriptResult(response, exec.Result);
      if response.ContentType = HTTP_RESP_STATICFILE then
         HandleStaticFileWebResponse(response);
      if shStatic in response.Hints then
         prog.ProgramObject.TagInterface := TWebStaticCacheEntry.Create(response);
   end;
end;

// StopDWS
//
procedure TSimpleDWScript.StopDWS;
var
   p : PExecutingScript;
begin
   FExecutingScriptsLock.BeginRead;
   p:=FExecutingScripts;
   while p<>nil do begin
      p^.Exec^.Stop;
      p:=p.Next;
   end;
   FExecutingScriptsLock.EndRead;
end;

// HandleP2JS
//
procedure TSimpleDWScript.HandleP2JS(const fileName : String; request : TWebRequest; response : TWebResponse);
var
   code, js : String;
   prog : IdwsProgram;
begin
   if (CPUUsageLimit>0) and not WaitForCPULimit then begin
      Handle503(response);
      Exit;
   end;

   TryAcquireDWS(fileName, prog);
   FCompilerLock.Enter;
   try
      FCompilerFiles := TAutoStrings.Create;
      FCodeGenLock.Enter;
      try
         if prog=nil then begin
            code:=dwsCompileSystem.AllocateFileSystem.LoadTextFile(fileName);
            FHotPath:=ExtractFilePath(fileName);
            js:=FJSFilter.CompileToJS(prog, code);
         end else begin
            js:=FJSFilter.CompileToJS(prog, '');
         end;
      finally
         FCodeGenLock.Leave;
      end;
   finally
      FCompilerFiles := nil;
      FCompilerLock.Leave;
   end;
   if (prog<>nil) and prog.Msgs.HasErrors then
      Handle500(response, prog.Msgs)
   else begin
      response.ContentData:='(function(){'#13#10+UTF8Encode(js)+'})();'#13#10;
      response.ContentType:='text/javascript; charset=UTF-8';
   end;
end;

// CompilationInfoJSON
//
function TSimpleDWScript.CompilationInfoJSON(const sourceName : String; fileType : TFileAccessType) : String;
var
   prog : IdwsProgram;
   fileName : String;
begin
   fileName := dwsCompileSystem.AllocateFileSystem.ValidateFileName(sourceName);
   if (fileName = '') and Assigned(FBackgroundFileSystem) then
      fileName := FBackgroundFileSystem.ValidateFileName(sourceName);
   if fileName = '' then
      Exit('"Access denied or does not exist"')
   else begin
      TryAcquireDWS(fileName, prog);
      if prog = nil then
         CompileDWS(fileName, prog, fileType);
      Result := prog.Msgs.AsJSON;
   end;
end;

// ExecutionInfoJSON
//
function TSimpleDWScript.ExecutionInfoJSON(const sourceName : String; fileType : TFileAccessType) : String;
var
   prog : IdwsProgram;
   fileName : String;
   wr : TdwsJSONWriter;
   execStats : TdwsProgramExecStats;
begin
   fileName := dwsCompileSystem.AllocateFileSystem.ValidateFileName(sourceName);
   if (fileName = '') and Assigned(FBackgroundFileSystem) then
      fileName := FBackgroundFileSystem.ValidateFileName(sourceName);
   if fileName = '' then
      Exit('"Access denied or does not exist"')
   else begin
      TryAcquireDWS(fileName, prog);
      if prog = nil then
         FillChar(execStats, SizeOf(execStats), 0)
      else execStats := prog.ProgramObject.ExecStats;
      wr := TdwsJSONWriter.Create;
      try
         wr.BeginObject;
            wr.WriteInteger('count', execStats.Count);
            wr.WriteInteger('totalTimeMSec', execStats.TimeMSec);
         wr.EndObject;
         Result := wr.ToString;
      finally
         wr.Free;
      end;
   end;
end;

// CompiledPrograms
//
type
   TCompiledProgramsEnumerator = class
      a : TStringDynArray;
      i : Integer;
      function Callback(const item : TCompiledProgram) : TSimpleHashAction;
   end;
function TCompiledProgramsEnumerator.Callback(const item : TCompiledProgram) : TSimpleHashAction;
begin
   a[i] := item.Name;
   Inc(i);
   Result := shaNone;
end;
function TSimpleDWScript.CompiledPrograms : TStringDynArray;
var
   enumerator : TCompiledProgramsEnumerator;
begin
   enumerator := TCompiledProgramsEnumerator.Create;
   try
      FCompiledProgramsLock.BeginRead;
      try
         SetLength(enumerator.a, FCompiledPrograms.Count);
         FCompiledPrograms.Enumerate(enumerator.Callback);
      finally
         FCompiledProgramsLock.EndRead;
      end;
      Result := enumerator.a;
   finally
      enumerator.Free;
   end;
end;

// FlushDWSCache
//
procedure TSimpleDWScript.FlushDWSCache(const fileName : String = '');
begin
   FCompiledProgramsLock.BeginWrite;
   try
      if fileName='' then begin
         FCompiledPrograms.Clear;
      end else begin
         if LastDelimiter('*?', fileName) > 0 then
            FFlushMask := TMask.Create(fileName)
         else FFlushMask := TMask.Create('*' + fileName + '*');
         try
            FCompiledPrograms.Enumerate(FlushMatchingMask);
         finally
            FreeAndNil(FFlushMask);
         end;
      end;
   finally
      FCompiledProgramsLock.EndWrite;
   end;
end;

// LoadCPUOptions
//
procedure TSimpleDWScript.LoadCPUOptions(options : TdwsJSONValue);
var
   cpu : TdwsJSONValue;
begin
   cpu:=TdwsJSONValue.ParseString(cDefaultCPUOptions);
   try
      cpu.Extend(options);

      CPUUsageLimit:=cpu['UsageLimit'].AsInteger;
      CPUAffinity:=cpu['Affinity'].AsInteger;
   finally
      cpu.Free;
   end;
end;

// LoadDWScriptOptions
//
procedure TSimpleDWScript.LoadDWScriptOptions(options : TdwsJSONValue);
var
   dws : TdwsJSONValue;
   conditionals : TdwsJSONValue;
   i : Integer;
begin
   dws:=TdwsJSONValue.ParseString(cDefaultDWScriptOptions);
   try
      dws.Extend(options);

      ScriptTimeoutMilliseconds:=dws['TimeoutMSec'].AsInteger;
      WorkerTimeoutMilliseconds:=dws['WorkerTimeoutMSec'].AsInteger;

      DelphiWebScript.Config.MaxDataSize := dws['StackMaxSize'].AsInteger;
      DelphiWebScript.Config.MaxRecursionDepth := dws['MaxRecursionDepth'].AsInteger;

      dwsCompileSystem.Paths.Clear;
      dwsCompileSystem.Paths.Add(IncludeTrailingPathDelimiter(PathVariables.Values['www']));
      ApplyPathsVariables(dws['LibraryPaths'], dwsCompileSystem.Paths);
      dwsCompileSystem.Variables := FPathVariables;

      dwsRuntimeFileSystem.Paths.Clear;
      ApplyPathsVariables(dws['WorkPaths'], dwsRuntimeFileSystem.Paths);
      dwsRuntimeFileSystem.Variables := FPathVariables;

      conditionals := dws['Conditionals'];
      for i := 0 to conditionals.ElementCount-1 do
         DelphiWebScript.Config.Conditionals.Add(conditionals.Elements[0].AsString);

      dwsHtmlFilter.PatternOpen:=dws['PatternOpen'].AsString;
      dwsHtmlFilter.PatternClose:=dws['PatternClose'].AsString;
      dwsHtmlFilter.PatternEval:=dws['PatternEval'].AsString;

      FBkgndWorkers.MaxWorkersPerQueue := dws['MaxWorkersPerQueue'].AsInteger;

      FEnableJIT:=dws['JIT'].AsBoolean;

      FStartupScriptName:=ApplyPathVariables(dws['Startup'].AsString);
      FShutdownScriptName:=ApplyPathVariables(dws['Shutdown'].AsString);

      if dws['COM'].AsBoolean then
         dwsComConnector.Script:=DelphiWebScript;
   finally
      dws.Free;
   end;
end;

// ApplyPathVariables
//
function TSimpleDWScript.ApplyPathVariables(const aPath : String) : String;
begin
   Result := ApplyStringVariables(aPath, FPathVariables, '%');
end;

// TryAcquireDWS
//
procedure TSimpleDWScript.TryAcquireDWS(const fileName : String; var prog : IdwsProgram);
var
   cp : TCompiledProgram;
begin
   cp.Name:=fileName;
   FCompiledProgramsLock.BeginRead;
   try
      if FCompiledPrograms.Match(cp) then
         prog:=cp.Prog;
   finally
      FCompiledProgramsLock.EndRead;
   end;
end;

// CompileDWS
//
procedure TSimpleDWScript.CompileDWS(const fileName : String; var prog : IdwsProgram;
                                     typ : TFileAccessType);
var
   code : String;
   cp : TCompiledProgram;
   {$ifdef ALLOW_JIT}
   jit : TdwsJITx86;
   {$endif}
begin
   FCompilerLock.Enter;
   try
      LogCompilation('Compiling "%s"', [fileName]);
      FCompilerFiles := TAutoStrings.Create;

      code := dwsCompileSystem.AllocateFileSystem.LoadTextFile(fileName);

      // check after compiler lock in case of simultaneous requests
      TryAcquireDWS(fileName, prog);
      if prog<>nil then Exit;

      FHotPath:=ExtractFilePath(fileName);

      if typ=fatDWS then
         DelphiWebScript.Config.Filter:=dwsHtmlFilter
      else DelphiWebScript.Config.Filter:=nil;

      prog:=DelphiWebScript.Compile(code, fileName);

      if not prog.Msgs.HasErrors then begin
         {$ifdef ALLOW_JIT}
         if FEnableJIT and (typ in [fatDWS, fatPAS]) then begin
            jit:=TdwsJITx86.Create;
            try
               jit.Options:=[jitoDoStep, jitoRangeCheck];
               jit.GreedyJIT(prog.ProgramObject);
            finally
               jit.Free;
            end;
         end;
         {$endif}
      end else if ErrorLogDirectory<>'' then begin
         LogCompileErrors(fileName, prog.Msgs);
      end;

      cp.Name  := fileName;

      FCompiledProgramsLock.BeginWrite;
      try
         if not FCompiledPrograms.Match(cp) then begin
            cp.Prog  := prog;
            cp.Files := FCompilerFiles;
            FCompiledPrograms.Add(cp);
         end;
      finally
         FCompiledProgramsLock.EndWrite;
      end;
   finally
      FCompilerFiles := nil;
      FCompilerLock.Leave;
   end;
end;

// LogError
//
procedure TSimpleDWScript.LogError(const msg : String);
var
   buf : String;
begin
   buf := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now)
         +' '+msg+#13#10;
   AppendTextToUTF8File(ErrorLogDirectory+'error.log', UTF8Encode(buf));
end;

// LogCompileErrors
//
procedure TSimpleDWScript.LogCompileErrors(const fileName : String; const msgs : TdwsMessageList);
begin
   LogError('in '+fileName+#13#10+msgs.AsInfo);
end;

// LogCompilation
//
procedure TSimpleDWScript.LogCompilation(const fmt : String; const params : array of const);
begin
   {$ifdef LogCompiles}
   LogError(Format(fmt, params));
   {$endif}
end;

// LogExecutionTimeOut
//
procedure TSimpleDWScript.LogExecutionTimeOut(const fileName : String);
begin
   LogError('timeout for '+fileName);
end;

// FlushMatchingMask
//
function TSimpleDWScript.FlushMatchingMask(const cp : TCompiledProgram) : TSimpleHashAction;
var
   i : Integer;
   list : TStringList;
begin
   list := cp.Files.Value;
   for i := 0 to list.Count-1 do begin
      if FFlushMask.Matches(list[i]) then
         Exit(shaRemove);
   end;
   Result := shaNone;
end;

// DoInclude
//
procedure TSimpleDWScript.DoInclude(const scriptName: String; var scriptSource: String);
var
   pathName : String;
begin
   if Pos('%', scriptName) > 0 then
      pathName := ApplyPathVariables(scriptName)
   else if FHotPath <> '' then
      pathName := FHotPath + scriptName;
   if pathName <> '' then
      scriptSource := dwsCompileSystem.AllocateFileSystem.LoadTextFile(pathName);
end;

// DoNeedUnit
//
function TSimpleDWScript.DoNeedUnit(const unitName : String; var unitSource : String) : IdwsUnit;
begin
   Result:=nil;
   DoInclude(unitName+'.pas', unitSource);
end;

// SetCPUUsageLimit
//
procedure TSimpleDWScript.SetCPUUsageLimit(const val : Integer);
begin
   FCPUUsageLimit:=val;
   if FCPUUsageLimit>0 then
      SystemCPU.Track;
end;

// SetCPUAffinity
//
procedure TSimpleDWScript.SetCPUAffinity(const val : Cardinal);
var
   hProcess : THandle;
   {$if CompilerVersion<23.0}
   procMask, systemMask : DWORD;
   {$else}
   procMask, systemMask : DWORD_PTR;
   {$ifend}
begin
   hProcess:=GetCurrentProcess;
   GetProcessAffinityMask(hProcess, procMask, systemMask);
   procMask:=systemMask and val;
   if procMask=0 then
      procMask:=systemMask;
   SetProcessAffinityMask(hProcess, procMask);
end;

// WaitForCPULimit
//
function TSimpleDWScript.WaitForCPULimit : Boolean;
var
   i : Integer;
begin
   i:=0;
   while SystemCPU.ProcessUsage>CPUUsageLimit do begin
      Sleep(100);
      Inc(i);
      if i=15 then Exit(False);
   end;
   Result:=True;
end;

// DoSourceFileStreamOpened
//
procedure TSimpleDWScript.DoSourceFileStreamOpened(Sender : TObject; const fileName : TFileName; const mode : TdwsFileOpenMode);
begin
   FCompilerFiles.Value.Add(fileName);
end;

// DoBackgroundWork
//
procedure TSimpleDWScript.DoBackgroundWork(const request : TWebRequest);
var
   name : String;
begin
   if Assigned(FBackgroundFileSystem) then begin
      name:=FBackgroundFileSystem.ValidateFileName(request.URL);
      if name<>'' then
         HandleDWS(name, fatPAS, request, nil, [optWorker]);
   end;
end;

// Handle500
//
class procedure TSimpleDWScript.Handle500(response : TWebResponse; msgs : TdwsMessageList);
begin
   if response<>nil then begin
      response.StatusCode:=500;
      response.ContentText['plain']:=msgs.AsInfo;
   end else OutputDebugString(msgs.AsInfo);
end;

// Handle503
//
class procedure TSimpleDWScript.Handle503(response : TWebResponse);
begin
   if response<>nil then begin
      response.StatusCode:=503;
      response.ContentData:='CPU Usage limit reached, please try again later';
      response.ContentType:='text/plain';
   end else OutputDebugString('CPU Usage limit reached');
end;

// CheckDirectoryChanges
//
type
   TFilesList = class (TStringList)
      private
         FProgIndex : Integer;
         FProgs : array of IdwsProgram;
         FChanged : array of Boolean;
         FDWS : TSimpleDWScript;
         function CompareStrings(const S1, S2: UnicodeString): Integer; override;
         function Enumerate(const item : TCompiledProgram) : TSimpleHashAction;
         function RemoveChanged(const item : TCompiledProgram) : TSimpleHashAction;
   end;
function TFilesList.CompareStrings(const S1, S2: UnicodeString): Integer;
begin
   Result := CompareStr(S1, S2);
end;
function TFilesList.Enumerate(const item : TCompiledProgram) : TSimpleHashAction;
var
   i : Integer;
   sourceList : TStringList;
begin
   FProgs[FProgIndex] := item.Prog;
   sourceList := item.Files.Value;
   for i := 0 to sourceList.Count-1 do
      AddObject(sourceList[i], TObject(FProgIndex));
   Inc(FProgIndex);
   Result := shaNone;
end;
function TFilesList.RemoveChanged(const item : TCompiledProgram) : TSimpleHashAction;
var
   i : Integer;
begin
   Result := shaNone;
   for i := 0 to High(FChanged) do begin
      if FProgs[i] = item.Prog then begin
         if FChanged[i] then begin
            item.Flush;
            FDWS.LogCompilation('Flushing "%s"', [item.Name]);
            Result := shaRemove;
         end;
         Break;
      end;
   end;
end;
procedure TSimpleDWScript.CheckDirectoryChanges;
var
   i, n : Integer;
   prevFileName : String;
   nextCheckTime : TFileTime;
   fileChanged, gotChanges : Boolean;
   files : TFilesList;
   info : TWin32FileAttributeData;
begin
   files := TFilesList.Create;
   try
      files.FDWS := Self;
      // speculative check and allocation outside of lock
      n := FCompiledPrograms.Count;
      SetLength(files.FProgs, n);
      // collect all files as fast as possible then release lock
      FCompiledProgramsLock.BeginRead;
      try
         if n <> FCompiledPrograms.Count then
            SetLength(files.FProgs, n);
         FCompiledPrograms.Enumerate(files.Enumerate);
      finally
         FCompiledProgramsLock.EndRead;
      end;
      // sort to avoid duplicate checks
      files.CaseSensitive := False;
      files.Sort;
      SetLength(files.FChanged, Length(files.FProgs));
      prevFileName := '';
      fileChanged := False;
      gotChanges := False;
      GetSystemTimeAsFileTime(nextCheckTime);
      for i := 0 to files.Count-1 do begin
         if files[i] <> prevFileName then begin
            prevFileName := files[i];
            if GetFileAttributesEx(PChar(Pointer(prevFileName)), GetFileExInfoStandard, @info) then begin
               fileChanged := (UInt64(info.ftLastWriteTime) >= UInt64(FLastCheckTime));
               if fileChanged then
                  LogCompilation('"%s" change detected', [prevFileName]);
            end else begin
               LogCompilation('"%s" check error: %s', [prevFileName, SysErrorMessage(GetLastError)]);
               fileChanged := True
            end;
         end;
         if fileChanged then begin
            files.FChanged[Integer(files.Objects[i])] := True;
            gotChanges := True;
         end;
      end;
      // purge programs whose file(s) changed
      if gotChanges then begin
         FCompiledProgramsLock.BeginWrite;
         try
            FCompiledPrograms.Enumerate(files.RemoveChanged);
         finally
            FCompiledProgramsLock.EndWrite;
         end;
      end;
      FLastCheckTime := nextCheckTime;
   finally
      files.Free;
   end;
   FCheckDirectoryChanges := TTimerTimeout.Create(cCheckDirectoryChangesInterval, CheckDirectoryChanges);
end;

// Initialize
//
procedure TSimpleDWScript.Initialize(const serverInfo : IWebServerInfo);
begin
   FWebServerLib:=TdwsWebServerLib.Create(Self);
   FWebServerLib.Server:=serverInfo;
   FWebServerLib.dwsWebServer.Script:=DelphiWebScript;

   FWebLib.Server:=serverInfo;

   FBkgndWorkers:=TdwsBackgroundWorkersLib.Create(Self);
   FBkgndWorkers.dwsBackgroundWorkers.Script:=DelphiWebScript;
   FBkgndWorkers.OnBackgroundWork:=DoBackgroundWork;
   FBkgndWorkers.OnBackgroundLogEvent:=LogError;
end;

// Finalize
//
procedure TSimpleDWScript.Finalize;
begin
   FWebLib.Server:=nil;
   FreeAndNil(FBkgndWorkers);
   FreeAndNil(FWebServerLib);
end;

// ApplyPathsVariables
//
procedure TSimpleDWScript.ApplyPathsVariables(paths : TdwsJSONValue; dest : TStrings);
var
   i : Integer;
   path : String;
begin
   for i:=0 to paths.ElementCount-1 do begin
      path:=paths.Elements[i].AsString;
      if path<>'' then begin
         path:=ApplyPathVariables(path);
         dest.Add(IncludeTrailingPathDelimiter(path));
      end;
   end;
end;

// Startup
//
procedure TSimpleDWScript.Startup;
var
   startupWebRequest : TWebRequest;
begin
   if ErrorLogDirectory <> '' then
      LogError('Startup ' + ParamStr(0));

   FBackgroundFileSystem:=dwsRuntimeFileSystem.AllocateFileSystem;

   GetSystemTimeAsFileTime(FLastCheckTime);
   CheckDirectoryChanges;

   if StartupScriptName <> '' then begin
      startupWebRequest := TEmptyWebRequest.Create;
      try
         HandleDWS(StartupScriptName, fatPAS, startupWebRequest, nil, [optWorker]);
      finally
         startupWebRequest.Free;
      end;
   end;
end;

// Shutdown
//
procedure TSimpleDWScript.Shutdown;
var
   shutdownWebRequest : TWebRequest;
begin
   if ErrorLogDirectory <> '' then
      LogError('Shutdown ' + ParamStr(0));

   FCheckDirectoryChanges := nil;

   FBackgroundFileSystem:=nil;

   StopDWS;

   if ShutdownScriptName <> '' then begin
      shutdownWebRequest := TEmptyWebRequest.Create;
      try
         HandleDWS(ShutdownScriptName, fatPAS, shutdownWebRequest, nil, [optWorker]);
      finally
         shutdownWebRequest.Free;
      end;
   end;
end;

// LiveQueries
//
function TSimpleDWScript.LiveQueries : String;
var
   wr : TdwsJSONWriter;
   exec : PExecutingScript;
   webEnv : TWebEnvironment;
   t : Int64;
begin
   wr := TdwsJSONWriter.Create(nil);
   try
      wr.BeginArray;
      FExecutingScriptsLock.BeginRead;
      try
         t := GetSystemMilliseconds;
         exec := FExecutingScripts;
         while exec <> nil do begin
            webEnv := exec.Exec^.Environment.GetSelf as TWebEnvironment;
            wr.BeginObject;
            wr.WriteName('id').WriteInteger(Cardinal(exec.ID));
            if optWorker in exec.Options then begin
               wr.WriteName('path').WriteString(webEnv.WebRequest.URL);
               wr.WriteName('query').WriteString(UTF8ToString(webEnv.WebRequest.ContentData));
            end else begin
               wr.WriteName('path').WriteString(webEnv.WebRequest.PathInfo);
               wr.WriteName('query').WriteString(webEnv.WebRequest.QueryString);
            end;
            wr.WriteName('ip').WriteString(webEnv.WebRequest.RemoteIP);
            wr.WriteName('ms').WriteInteger(t-exec.Start);
            wr.WriteName('sleeping').WriteBoolean(exec.Exec^.Sleeping);
            wr.WriteName('options').BeginArray;
               if optWorker in exec.Options then wr.WriteString('worker');
            wr.EndArray;
            wr.EndObject;
            exec := exec.Next;
         end;
      finally
         FExecutingScriptsLock.EndRead;
      end;
      wr.EndArray;
      Result := wr.ToString;
   finally
      wr.Free;
   end;
end;

// ------------------
// ------------------ TCompiledProgramHash ------------------
// ------------------

// SameItem
//
function TCompiledProgramHash.SameItem(const item1, item2 : TCompiledProgram) : Boolean;
begin
   Result:=UnicodeSameText(item1.Name, item2.Name)
end;

// GetItemHashCode
//
function TCompiledProgramHash.GetItemHashCode(const item1 : TCompiledProgram) : Cardinal;
begin
   Result:=SimpleStringHash(item1.Name);
end;

end.
