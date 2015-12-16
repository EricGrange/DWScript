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

{.$define EnablePas2Js}

uses
   Windows, SysUtils, Classes, StrUtils,
   dwsFileSystem, dwsGlobalVarsFunctions, dwsExprList,
   dwsCompiler, dwsHtmlFilter, dwsComp, dwsExprs, dwsUtils, dwsXPlatform,
   dwsJSONConnector, dwsJSON, dwsErrors, dwsFunctions, dwsSymbols,
   dwsJIT, dwsJITx86,
{$ifdef EnablePas2Js}
   dwsJSFilter, dwsJSLibModule, dwsCodeGen,
{$endif}
   dwsWebEnvironment, dwsSystemInfoLibModule, dwsCPUUsage, dwsWebLibModule,
   dwsWebServerHelpers, dwsZipLibModule,
   dwsDataBase, dwsDataBaseLibModule, dwsWebServerInfo, dwsWebServerLibModule,
   dwsBackgroundWorkersLibModule, dwsSynapseLibModule, dwsCryptoLibModule,
   dwsEncodingLibModule, dwsComConnector;

type

   TProgramList = TSimpleList<IdwsProgram>;

   TCompiledProgram = record
      Name : String;
      Prog : IdwsProgram;
   end;

   TDependenciesHash = class (TSimpleNameObjectHash<TProgramList>)
      procedure RegisterProg(const cp : TCompiledProgram);
      procedure DeregisterProg(const prog : IdwsProgram);
      procedure AddName(const name : String; const prog : IdwsProgram);
   end;

   TCompiledProgramHash = class (TSimpleHash<TCompiledProgram>)
      protected
         function SameItem(const item1, item2 : TCompiledProgram) : Boolean; override;
         function GetItemHashCode(const item1 : TCompiledProgram) : Integer; override;
   end;

   TDWSHandlingOption = (optWorker);
   TDWSHandlingOptions = set of TDWSHandlingOption;

   PExecutingScript = ^TExecutingScript;
   TExecutingScript = record
      ID : Integer;
      Exec : IdwsProgramExecution;
      Start : Int64;
      Options : TDWSHandlingOptions;
      Prev, Next : PExecutingScript;
   end;

   TLoadSourceCodeEvent = function (const fileName : String) : String of object;

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
      FWebEnv : TdwsWebLib;
      FDataBase : TdwsDatabaseLib;
      FJSON : TdwsJSONLibModule;
      FSynapse : TdwsSynapseLib;
      FWebServerLib : TdwsWebServerLib;
      FBkgndWorkers : TdwsBackgroundWorkersLib;

      FCompiledPrograms : TCompiledProgramHash;
      FCompiledProgramsLock : TFixedCriticalSection;
      FDependenciesHash : TDependenciesHash;

      FCompilerLock : TFixedCriticalSection;
      FCodeGenLock : TFixedCriticalSection;
      FEnableJIT : Boolean;

      FExecutingID : Integer;
      FExecutingScripts : PExecutingScript;
      FExecutingScriptsLock : TMultiReadSingleWrite;

      FFlushProgList : TProgramList;

      FOnLoadSourceCode : TLoadSourceCodeEvent;

      FStartupScriptName : String;
      FShutdownScriptName : String;
      FBackgroundFileSystem : IdwsFileSystem;

      FErrorLogDirectory : String;

   protected
      {$ifdef EnablePas2Js}
      FJSCompiler : TDelphiWebScript;
      FJSFilter : TdwsJSFilter;
      {$endif}

      procedure TryAcquireDWS(const fileName : String; var prog : IdwsProgram);
      procedure CompileDWS(const fileName : String; var prog : IdwsProgram;
                           typ : TFileAccessType);

      procedure LogCompileErrors(const fileName : String; const msgs : TdwsMessageList);

      function AddNotMatching(const cp : TCompiledProgram) : TSimpleHashAction;

      procedure SetCPUUsageLimit(const val : Integer);
      procedure SetCPUAffinity(const val : Cardinal);

      function WaitForCPULimit : Boolean;

      function DoLoadSourceCode(const fileName : String) : String;

      procedure DoBackgroundWork(const request : TWebRequest);

      class procedure Handle500(response : TWebResponse; msgs : TdwsMessageList); static;
      class procedure Handle503(response : TWebResponse); static;

   public
      procedure Initialize(const serverInfo : IWebServerInfo);
      procedure Finalize;

      procedure HandleDWS(const fileName : String; typ : TFileAccessType;
                          request : TWebRequest; response : TWebResponse;
                          const options : TDWSHandlingOptions);
      procedure StopDWS;

      {$ifdef EnablePas2Js}
      procedure HandleP2JS(const fileName : String; request : TWebRequest; response : TWebResponse);
      {$endif}

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

      property OnLoadSourceCode : TLoadSourceCodeEvent read FOnLoadSourceCode write FOnLoadSourceCode;

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
         // HTML Filter patterns
         +'"PatternOpen": "<?pas",'
         +'"PatternEval": "=",'
         +'"PatternClose": "?>",'
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

procedure TSimpleDWScript.DataModuleCreate(Sender: TObject);
var
   cryptoLib : TdwsCryptoLib;
begin
   // attempt to minimize probability of ID collisions between server restarts
   FExecutingID:=SimpleIntegerHash(GetTickCount);

   FPathVariables:=TFastCompareTextList.Create;

   FSystemInfo:=TdwsSystemInfoLibModule.Create(Self);
   FSystemInfo.dwsSystemInfo.Script:=DelphiWebScript;

   FDataBase:=TdwsDatabaseLib.Create(Self);
   FDataBase.dwsDatabase.Script:=DelphiWebScript;
   TdwsDataBase.OnApplyPathVariables:=ApplyPathVariables;

   FWebEnv:=TdwsWebLib.Create(Self);
   FWebEnv.dwsWeb.Script:=DelphiWebScript;

   FJSON:=TdwsJSONLibModule.Create(Self);
   FJSON.Script:=DelphiWebScript;

   FSynapse:=TdwsSynapseLib.Create(Self);
   FSynapse.Script:=DelphiWebScript;

   cryptoLib:=TdwsCryptoLib.Create(Self);
   cryptoLib.dwsCrypto.Script:=DelphiWebScript;
   cryptoLib.UseTemporaryStorageForNonces;

   TdwsEncodingLib.Create(Self).dwsEncoding.Script:=DelphiWebScript;

   TdwsZipLib.Create(Self).dwsZip.Script:=DelphiWebScript;

   FCompiledPrograms:=TCompiledProgramHash.Create;
   FCompiledProgramsLock:=TFixedCriticalSection.Create;
   FCompilerLock:=TFixedCriticalSection.Create;
   FCodeGenLock:=TFixedCriticalSection.Create;
   FDependenciesHash:=TDependenciesHash.Create;

   FExecutingScriptsLock:=TMultiReadSingleWrite.Create;

   FScriptTimeoutMilliseconds:=3000;
   FWorkerTimeoutMilliseconds:=30000;

{$ifdef EnablePas2Js}
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
{$endif}
end;

procedure TSimpleDWScript.DataModuleDestroy(Sender: TObject);
begin
   FlushDWSCache;

   FCodeGenLock.Free;
   FCompilerLock.Free;
   FCompiledProgramsLock.Free;
   FCompiledPrograms.Free;
   FDependenciesHash.Free;
   FPathVariables.Free;

   FExecutingScriptsLock.Free;
end;

// HandleDWS
//
procedure TSimpleDWScript.HandleDWS(const fileName : String; typ : TFileAccessType;
                                    request : TWebRequest; response : TWebResponse;
                                    const options : TDWSHandlingOptions);

   procedure HandleScriptResult(response : TWebResponse; scriptResult : TdwsResult);
   begin
      if (response.ContentEncoding='') and StrBeginsWithA(response.ContentType, 'text/') then
         response.ContentData:=scriptResult.ToUTF8String
      else response.ContentData:=scriptResult.ToDataString;
   end;

var
   prog : IdwsProgram;
   webenv : TWebEnvironment;
   executing : TExecutingScript;
begin
   if (CPUUsageLimit>0) and not WaitForCPULimit then begin
      Handle503(response);
      Exit;
   end;

   TryAcquireDWS(fileName, prog);
   if prog=nil then
      CompileDWS(fileName, prog, typ);

   if prog.Msgs.HasErrors then
      Handle500(response, prog.Msgs)
   else begin
      executing.Start:=GetSystemMilliseconds;
      executing.Exec:=prog.CreateNewExecution;
      executing.Options:=options;
      executing.ID:=InterlockedIncrement(FExecutingID);

      webenv:=TWebEnvironment.Create;
      webenv.WebRequest:=request;
      webenv.WebResponse:=response;
      executing.Exec.Environment:=webenv;
      try
         executing.Exec.BeginProgram;

         // insert into executing queue
         executing.Prev:=nil;
         FExecutingScriptsLock.BeginWrite;
         executing.Next:=FExecutingScripts;
         if FExecutingScripts<>nil then
            FExecutingScripts.Prev:=@executing;
         FExecutingScripts:=@executing;
         FExecutingScriptsLock.EndWrite;

         if optWorker in options then
            executing.Exec.RunProgram(WorkerTimeoutMilliseconds)
         else executing.Exec.RunProgram(ScriptTimeoutMilliseconds);

         // extract from executing queue
         FExecutingScriptsLock.BeginWrite;
         if executing.Prev<>nil then
            executing.Prev.Next:=executing.Next
         else FExecutingScripts:=executing.Next;
         if executing.Next<>nil then
            executing.Next.Prev:=executing.Prev;
         FExecutingScriptsLock.EndWrite;

         executing.Exec.EndProgram;
      finally
         executing.Exec.Environment:=nil;
      end;

      if executing.Exec.Msgs.Count>0 then
         Handle500(response, executing.Exec.Msgs)
      else if (response<>nil) and (response.ContentData='') then
         HandleScriptResult(response, executing.Exec.Result);
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
      p^.Exec.Stop;
      p:=p.Next;
   end;
   FExecutingScriptsLock.EndRead;
end;

// HandleP2JS
//
{$ifdef EnablePas2Js}
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
   FCodeGenLock.Enter;
   try
      if prog=nil then begin
         code:=DoLoadSourceCode(fileName);
         js:=FJSFilter.CompileToJS(prog, code);
      end else begin
         js:=FJSFilter.CompileToJS(prog, '');
      end;
   finally
      FCodeGenLock.Leave;
   end;
   if (prog<>nil) and prog.Msgs.HasErrors then
      Handle500(response, prog.Msgs)
   else begin
      response.ContentData:='(function(){'#13#10+UTF8Encode(js)+'})();'#13#10;
      response.ContentType:='text/javascript; charset=UTF-8';
   end;
end;
{$endif}

// FlushDWSCache
//
procedure TSimpleDWScript.FlushDWSCache(const fileName : String = '');
var
   i : Integer;
   oldHash : TCompiledProgramHash;
   unitName : String;
begin
   // ignore .bak files
   if StrEndsWith(fileName, '.bak') then exit;
   // ignore .db folder
   if Pos('/.db/', fileName)>0 then exit;

   FCompiledProgramsLock.Enter;
   try
      if fileName='' then begin
         FDependenciesHash.Clean;
         FCompiledPrograms.Clear;
      end else begin
         unitName:=LowerCase(ExtractFileName(fileName));
         FFlushProgList:=FDependenciesHash.Objects[unitName];
         if FFlushProgList=nil then
            FFlushProgList:=FDependenciesHash.Objects[ChangeFileExt(unitName, '')];
         if (FFlushProgList<>nil) and (FFlushProgList.Count>0) then begin
            oldHash:=FCompiledPrograms;
            try
               FCompiledPrograms:=TCompiledProgramHash.Create;
               oldHash.Enumerate(AddNotMatching);
            finally
               oldHash.Free;
            end;
            for i:=0 to FFlushProgList.Count-1 do
               FDependenciesHash.DeregisterProg(FFlushProgList[i]);
            FFlushProgList.Clear;
         end;
      end;
   finally
      FCompiledProgramsLock.Leave;
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
begin
   dws:=TdwsJSONValue.ParseString(cDefaultDWScriptOptions);
   try
      dws.Extend(options);

      ScriptTimeoutMilliseconds:=dws['TimeoutMSec'].AsInteger;
      WorkerTimeoutMilliseconds:=dws['WorkerTimeoutMSec'].AsInteger;

      DelphiWebScript.Config.MaxDataSize:=dws['StackMaxSize'].AsInteger;
      DelphiWebScript.Config.MaxRecursionDepth:=dws['MaxRecursionDepth'].AsInteger;

      dwsCompileSystem.Paths.Clear;
      ApplyPathsVariables(dws['LibraryPaths'], dwsCompileSystem.Paths);

      dwsRuntimeFileSystem.Paths.Clear;
      ApplyPathsVariables(dws['WorkPaths'], dwsRuntimeFileSystem.Paths);

      dwsHtmlFilter.PatternOpen:=dws['PatternOpen'].AsString;
      dwsHtmlFilter.PatternClose:=dws['PatternClose'].AsString;
      dwsHtmlFilter.PatternEval:=dws['PatternEval'].AsString;

      FEnableJIT:=dws['JIT'].AsBoolean;

      FStartupScriptName:=ApplyPathVariables(dws['Startup'].AsString);
      FShutdownScriptName:=ApplyPathVariables(dws['Shutdown'].AsString);

      if dws['COM'].AsBoolean then
         dwsComConnector.Script:=DelphiWebScript;
   finally
      dws.Free;
   end;
end;

// TryAcquireDWS
//
procedure TSimpleDWScript.TryAcquireDWS(const fileName : String; var prog : IdwsProgram);
var
   cp : TCompiledProgram;
begin
   cp.Name:=fileName;
   FCompiledProgramsLock.Enter;
   try
      if FCompiledPrograms.Match(cp) then
         prog:=cp.Prog;
   finally
      FCompiledProgramsLock.Leave;
   end;
end;

// CompileDWS
//
procedure TSimpleDWScript.CompileDWS(const fileName : String; var prog : IdwsProgram;
                                     typ : TFileAccessType);
var
   code : String;
   cp : TCompiledProgram;
   jit : TdwsJITx86;
begin
   code:=DoLoadSourceCode(fileName);

   FCompilerLock.Enter;
   try
      // check after compiler lock in case of simultaneous requests
      TryAcquireDWS(fileName, prog);
      if prog<>nil then Exit;

      FHotPath:=ExtractFilePath(fileName);

      if typ=fatDWS then
         DelphiWebScript.Config.Filter:=dwsHtmlFilter
      else DelphiWebScript.Config.Filter:=nil;

      prog:=DelphiWebScript.Compile(code, fileName);

      if not prog.Msgs.HasErrors then begin
         if FEnableJIT and (typ in [fatDWS, fatPAS]) then begin
            jit:=TdwsJITx86.Create;
            try
               jit.Options:=[jitoDoStep, jitoRangeCheck];
               jit.GreedyJIT(prog.ProgramObject);
            finally
               jit.Free;
            end;
         end;
      end else if ErrorLogDirectory<>'' then begin
         LogCompileErrors(fileName, prog.Msgs);
      end;

      cp.Name:=fileName;
      cp.Prog:=prog;

      FCompiledProgramsLock.Enter;
      try
         FCompiledPrograms.Add(cp);
         FDependenciesHash.RegisterProg(cp);
      finally
         FCompiledProgramsLock.Leave;
      end;
   finally
      FCompilerLock.Leave;
   end;
end;

// LogError
//
procedure TSimpleDWScript.LogError(const msg : String);
var
   buf : String;
begin
   buf := #13#10+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now)
         +' '+msg+#13#10;
   AppendTextToUTF8File(ErrorLogDirectory+'error.log', UTF8Encode(buf));
end;

// LogCompileErrors
//
procedure TSimpleDWScript.LogCompileErrors(const fileName : String; const msgs : TdwsMessageList);
begin
   LogError('in '+fileName+#13#10+msgs.AsInfo);
end;

// AddNotMatching
//
function TSimpleDWScript.AddNotMatching(const cp : TCompiledProgram) : TSimpleHashAction;
var
   i : Integer;
begin
   Result:=shaNone;
   if cp.Prog.Msgs.HasErrors then Exit;

   for i:=0 to FFlushProgList.Count-1 do
      if FFlushProgList[i]=cp.Prog then
         Exit;

   FCompiledPrograms.Add(cp);
end;

// DoInclude
//
procedure TSimpleDWScript.DoInclude(const scriptName: String; var scriptSource: String);
begin
   if FHotPath<>'' then
      scriptSource:=DoLoadSourceCode(FHotPath+scriptName);
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

// DoLoadSourceCode
//
function TSimpleDWScript.DoLoadSourceCode(const fileName : String) : String;
begin
   if Assigned(FOnLoadSourceCode) then
      Result:=FOnLoadSourceCode(fileName)
   else Result:=LoadTextFromFile(fileName);
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
   response.StatusCode:=503;
   response.ContentData:='CPU Usage limit reached, please try again later';
   response.ContentType:='text/plain';
end;

// Initialize
//
procedure TSimpleDWScript.Initialize(const serverInfo : IWebServerInfo);
begin
   FWebServerLib:=TdwsWebServerLib.Create(Self);
   FWebServerLib.Server:=serverInfo;
   FWebServerLib.dwsWebServer.Script:=DelphiWebScript;

   FBkgndWorkers:=TdwsBackgroundWorkersLib.Create(Self);
   FBkgndWorkers.dwsBackgroundWorkers.Script:=DelphiWebScript;
   FBkgndWorkers.OnBackgroundWork:=DoBackgroundWork;
end;

// Finalize
//
procedure TSimpleDWScript.Finalize;
begin
   FreeAndNil(FBkgndWorkers);
   FreeAndNil(FWebServerLib);
end;

// ApplyPathVariables
//
function TSimpleDWScript.ApplyPathVariables(const aPath : String) : String;
var
   p1, p2 : Integer;
begin
   Result:=aPath;
   p1:=Pos('%', Result);
   while p1>0 do begin
      p2:=PosEx('%', Result, p1+1);
      if p2<p1 then Break;
      Result:= Copy(Result, 1, p1-1)
              +FPathVariables.Values[Copy(Result, p1+1, p2-p1-1)]
              +Copy(Result, p2+1);
      p1:=PosEx('%', Result, p1);
   end;
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
begin
   FBackgroundFileSystem:=dwsRuntimeFileSystem.AllocateFileSystem;

   if StartupScriptName<>'' then
      HandleDWS(StartupScriptName, fatPAS, nil, nil, [optWorker]);
end;

// Shutdown
//
procedure TSimpleDWScript.Shutdown;
begin
   FBackgroundFileSystem:=nil;

   StopDWS;

   if ShutdownScriptName<>'' then
      HandleDWS(ShutdownScriptName, fatPAS, nil, nil, [optWorker]);
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
            webEnv := exec.Exec.Environment.GetSelf as TWebEnvironment;
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
            wr.WriteName('sleeping').WriteBoolean(exec.Exec.Sleeping);
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
function TCompiledProgramHash.GetItemHashCode(const item1 : TCompiledProgram) : Integer;
begin
   Result:=SimpleStringHash(item1.Name);
end;

// ------------------
// ------------------ TDependenciesHash ------------------
// ------------------

// RegisterProg
//
procedure TDependenciesHash.RegisterProg(const cp : TCompiledProgram);
var
   i : Integer;
   list : TScriptSourceList;
   item : TScriptSourceItem;
begin
   AddName(cp.Name, cp.Prog);
   list:=cp.Prog.SourceList;
   for i:=0 to list.Count-1 do begin
      item:=list[i];
      if not StrBeginsWith(item.NameReference, '*') then
         AddName(item.NameReference, cp.Prog);
   end;
end;

// DeregisterProg
//
procedure TDependenciesHash.DeregisterProg(const prog : IdwsProgram);
var
   i, j : Integer;
   list : TProgramList;
begin
   for i:=0 to HighIndex-1 do begin
      list:=BucketObject[i];
      if list<>nil then begin
         for j:=list.Count-1 downto 0 do
            list.Extract(j);
      end;
   end;
end;

// AddName
//
procedure TDependenciesHash.AddName(const name : String; const prog : IdwsProgram);
var
   lcName : String;
   list : TProgramList;
begin
   lcName:=LowerCase(ExtractFileName(name));
   list:=Objects[lcName];
   if list=nil then begin
      list:=TProgramList.Create;
      Objects[lcName]:=list;
   end;
   list.Add(prog);
end;

end.
