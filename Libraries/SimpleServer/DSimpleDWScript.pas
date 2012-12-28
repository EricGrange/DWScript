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

uses
   Windows, SysUtils, Classes, dwsFileSystem, dwsGlobalVarsFunctions,
   dwsCompiler, dwsHtmlFilter, dwsComp, dwsExprs, dwsUtils,
   dwsWebEnvironment, dwsSystemInfoLibModule, dwsCPUUsage, dwsWebLibModule,
   dwsJSON, dwsErrors, dwsFunctions;

type

   TCompiledProgram = record
      Name : String;
      Prog : IdwsProgram;
   end;

   TCompiledProgramHash = class (TSimpleHash<TCompiledProgram>)
      protected
         function SameItem(const item1, item2 : TCompiledProgram) : Boolean; override;
         function GetItemHashCode(const item1 : TCompiledProgram) : Integer; override;
   end;

   TSynDWScript = class(TDataModule)
      DelphiWebScript: TDelphiWebScript;
      dwsHtmlFilter: TdwsHtmlFilter;
      dwsGlobalVarsFunctions: TdwsGlobalVarsFunctions;
      procedure DataModuleCreate(Sender: TObject);
      procedure DataModuleDestroy(Sender: TObject);

   private
      FLibraryPaths : TStrings;
      FScriptTimeoutMilliseconds : Integer;
      FCPUUsageLimit : Integer;
      FCPUAffinity : Cardinal;

      FSystemInfo : TdwsSystemInfoLibModule;
      FFileSystem : TdwsCustomFileSystem;
      FWebEnv : TdwsWebLib;

      FCompiledPrograms : TCompiledProgramHash;
      FCompiledProgramsLock : TFixedCriticalSection;

      FCompilerLock : TFixedCriticalSection;

      FFlushMatching : String;

   protected
      function GetFileSystem : TdwsCustomFileSystem;
      procedure SetFileSystem(const val : TdwsCustomFileSystem);

      procedure TryAcquireDWS(const fileName : String; var prog : IdwsProgram);
      procedure CompileDWS(const fileName : String; var prog : IdwsProgram);

      procedure AddNotMatching(const cp : TCompiledProgram);

      function  DoNeedUnit(const unitName : String; var unitSource : String) : IdwsUnit;

      procedure SetCPUUsageLimit(const val : Integer);
      procedure SetCPUAffinity(const val : Cardinal);

      function WaitForCPULimit : Boolean;

   public
      procedure HandleDWS(const fileName : String; request : TWebRequest; response : TWebResponse);

      // flush the cache of fileName that begin by matchBegin
      procedure FlushDWSCache(const matchingBegin : String = '');

      procedure LoadCPUOptions(options : TdwsJSONValue);
      procedure LoadDWScriptOptions(options : TdwsJSONValue);

      property ScriptTimeoutMilliseconds : Integer read FScriptTimeoutMilliseconds write FScriptTimeoutMilliseconds;

      property CPUUsageLimit : Integer read FCPUUsageLimit write SetCPUUsageLimit;
      property CPUAffinity : Cardinal read FCPUAffinity write SetCPUAffinity;

      property FileSystem : TdwsCustomFileSystem read GetFileSystem write SetFileSystem;
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
         // Library paths (outside of www folder)
         // If missing, assumes a lib subfolder of the folder where the exe is
         +'"LibraryPaths": []'
      +'}';

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$R *.dfm}

procedure TSynDWScript.DataModuleCreate(Sender: TObject);
begin
   FSystemInfo:=TdwsSystemInfoLibModule.Create(Self);
   FSystemInfo.dwsSystemInfo.Script:=DelphiWebScript;

   FWebEnv:=TdwsWebLib.Create(Self);
   FWebEnv.dwsWeb.Script:=DelphiWebScript;

   FCompiledPrograms:=TCompiledProgramHash.Create;
   FCompiledProgramsLock:=TFixedCriticalSection.Create;
   FCompilerLock:=TFixedCriticalSection.Create;

   FScriptTimeoutMilliseconds:=3000;

   FLibraryPaths:=TStringList.Create;
   DelphiWebScript.OnNeedUnit:=DoNeedUnit;
end;

procedure TSynDWScript.DataModuleDestroy(Sender: TObject);
begin
   FlushDWSCache;

   FLibraryPaths.Free;
   FCompilerLock.Free;
   FCompiledProgramsLock.Free;
   FCompiledPrograms.Free;
end;

// GetFileSystem
//
function TSynDWScript.GetFileSystem : TdwsCustomFileSystem;
begin
   Result:=FFileSystem;
end;

// SetFileSystem
//
procedure TSynDWScript.SetFileSystem(const val : TdwsCustomFileSystem);
begin
   FFileSystem:=val;
   DelphiWebScript.Config.CompileFileSystem:=val;
   DelphiWebScript.Config.RuntimeFileSystem:=val;
end;

// HandleDWS
//
procedure TSynDWScript.HandleDWS(const fileName : String; request : TWebRequest; response : TWebResponse);

   procedure Handle503(response : TWebResponse);
   begin
      response.StatusCode:=503;
      response.ContentData:='CPU Usage limit reached, please try again later';
      response.ContentType:='text/plain';
   end;

   procedure Handle400(response : TWebResponse; msgs : TdwsMessageList);
   begin
      response.StatusCode:=400;
      response.ContentText['plain']:=msgs.AsInfo;
   end;

   procedure HandleScriptResult(response : TWebResponse; scriptResult : TdwsResult);
   begin
      if StrBeginsWithA(response.ContentType, 'text/') then
         response.ContentData:=scriptResult.ToUTF8String
      else response.ContentData:=scriptResult.ToDataString;
   end;

var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   webenv : TWebEnvironment;
begin
   if (CPUUsageLimit>0) and not WaitForCPULimit then begin
      Handle503(response);
      Exit;
   end;

   TryAcquireDWS(fileName, prog);
   if prog=nil then
      CompileDWS(fileName, prog);

   if prog.Msgs.HasErrors then
      Handle400(response, prog.Msgs)
   else begin
      exec:=prog.CreateNewExecution;

      webenv:=TWebEnvironment.Create;
      webenv.WebRequest:=request;
      webenv.WebResponse:=response;
      exec.Environment:=webenv;
      try
         exec.BeginProgram;
         exec.RunProgram(ScriptTimeoutMilliseconds);
         exec.EndProgram;
      finally
         exec.Environment:=nil;
      end;

      if exec.Msgs.Count>0 then
         Handle400(response, exec.Msgs)
      else if response.ContentData='' then
         HandleScriptResult(response, exec.Result);
   end;
end;

// FlushDWSCache
//
procedure TSynDWScript.FlushDWSCache(const matchingBegin : String = '');
var
   oldHash : TCompiledProgramHash;
begin
   FCompiledProgramsLock.Enter;
   try
      if matchingBegin='' then
         FCompiledPrograms.Clear
      else begin
         FFlushMatching:=matchingBegin;
         oldHash:=FCompiledPrograms;
         try
            FCompiledPrograms:=TCompiledProgramHash.Create;
            oldHash.Enumerate(AddNotMatching);
         finally
            oldHash.Free;
         end;
      end;
   finally
      FCompiledProgramsLock.Leave;
   end;
end;

// LoadCPUOptions
//
procedure TSynDWScript.LoadCPUOptions(options : TdwsJSONValue);
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
procedure TSynDWScript.LoadDWScriptOptions(options : TdwsJSONValue);
var
   dws, libs : TdwsJSONValue;
   i : Integer;
   path : String;
begin
   dws:=TdwsJSONValue.ParseString(cDefaultDWScriptOptions);
   try
      dws.Extend(options);

      ScriptTimeoutMilliseconds:=dws['TimeoutMSec'].AsInteger;

      libs:=dws['LibraryPaths'];
      FLibraryPaths.Clear;
      for i:=0 to libs.ElementCount-1 do begin
         path:=libs.Elements[i].AsString;
         if path<>'' then
            FLibraryPaths.Add(IncludeTrailingPathDelimiter(path));
      end;

   finally
      dws.Free;
   end;
end;

// TryAcquireDWS
//
procedure TSynDWScript.TryAcquireDWS(const fileName : String; var prog : IdwsProgram);
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
procedure TSynDWScript.CompileDWS(const fileName : String; var prog : IdwsProgram);
var
   sl : TStringList;
   code : String;
   cp : TCompiledProgram;
begin
   sl:=TStringList.Create;
   try
      sl.LoadFromFile(fileName);
      code:=sl.Text;
   finally
      sl.Free;
   end;

   FCompilerLock.Enter;
   try
      // check after compiler lock in case of simultaneous requests
      TryAcquireDWS(fileName, prog);
      if prog<>nil then Exit;

      prog:=DelphiWebScript.Compile(code);

      cp.Name:=fileName;
      cp.Prog:=prog;

      FCompiledProgramsLock.Enter;
      try
         FCompiledPrograms.Add(cp);
      finally
         FCompiledProgramsLock.Leave;
      end;
   finally
      FCompilerLock.Leave;
   end;
end;

// AddNotMatching
//
procedure TSynDWScript.AddNotMatching(const cp : TCompiledProgram);
begin
   if cp.Prog.Msgs.HasErrors or not StrBeginsWith(cp.Name, FFlushMatching) then
      FCompiledPrograms.Add(cp);
end;

// DoNeedUnit
//
function TSynDWScript.DoNeedUnit(const unitName : String; var unitSource : String) : IdwsUnit;
var
   i : Integer;
   fileName : String;
begin
   for i:=0 to FLibraryPaths.Count-1 do begin
      fileName:=FLibraryPaths[i]+unitName+'.pas';
      if not FileExists(fileName) then begin
         fileName:=FLibraryPaths[i]+unitName+'.inc';
         if not FileExists(fileName) then
            continue;
      end;
      unitSource:=LoadTextFromFile(fileName);
      break;
   end;
   Result:=nil;
end;

// SetCPUUsageLimit
//
procedure TSynDWScript.SetCPUUsageLimit(const val : Integer);
begin
   FCPUUsageLimit:=val;
   if FCPUUsageLimit>0 then
      SystemCPU.Track;
end;

// SetCPUAffinity
//
procedure TSynDWScript.SetCPUAffinity(const val : Cardinal);
var
   hProcess : THandle;
   procMask, systemMask : Cardinal;
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
function TSynDWScript.WaitForCPULimit : Boolean;
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

end.
