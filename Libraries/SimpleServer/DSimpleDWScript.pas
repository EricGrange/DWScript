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
   SysUtils, Classes, Rtti, dwsFileSystem, dwsGlobalVarsFunctions,
   dwsCompiler, dwsHtmlFilter, dwsComp, dwsExprs, dwsRTTIConnector, dwsUtils,
   dwsWebEnvironment, dwsSystemInfoLibModule, dwsCPUUsage;

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
      dwsRTTIConnector: TdwsRTTIConnector;
      procedure DataModuleCreate(Sender: TObject);
      procedure DataModuleDestroy(Sender: TObject);

   private
      FScriptTimeoutMilliseconds : Integer;
      FCPUUsageLimit : Integer;

      FSystemInfo : TdwsSystemInfoLibModule;
      FFileSystem : TdwsCustomFileSystem;

      FCompiledPrograms : TCompiledProgramHash;
      FCompiledProgramsLock : TFixedCriticalSection;

      FCompilerEnvironment : TRTTIEnvironment;

      FCompilerLock : TFixedCriticalSection;

      FFlushMatching : String;

   protected
      function GetFileSystem : TdwsCustomFileSystem;
      procedure SetFileSystem(const val : TdwsCustomFileSystem);

      procedure TryAcquireDWS(const fileName : String; var prog : IdwsProgram);
      procedure CompileDWS(const fileName : String; var prog : IdwsProgram);

      procedure AddNotMatching(const cp : TCompiledProgram);

      procedure SetCPUUsageLimit(const val : Integer);
      function WaitForCPULimit : Boolean;

   public
      procedure HandleDWS(const fileName : String; request : TWebRequest; response : TWebResponse);

      // flush the cache of fileName that begin by matchBegin
      procedure FlushDWSCache(const matchingBegin : String = '');

      property ScriptTimeoutMilliseconds : Integer read FScriptTimeoutMilliseconds write FScriptTimeoutMilliseconds;
      property CPUUsageLimit : Integer read FCPUUsageLimit write SetCPUUsageLimit;

      property FileSystem : TdwsCustomFileSystem read GetFileSystem write SetFileSystem;
  end;

implementation

{$R *.dfm}

function TWebRequest_GetHeaders(instance: Pointer; const args: array of TValue): TValue;
begin
   Result:=TWebRequest(instance).Header(args[0].AsString);
end;

function TWebRequest_GetCookies(instance: Pointer; const args: array of TValue): TValue;
begin
   Result:=TWebRequest(instance).Cookies.Values[args[0].AsString];
end;

function TWebRequest_GetQueryFields(instance: Pointer; const args: array of TValue): TValue;
begin
   Result:=TWebRequest(instance).QueryFields.Values[args[0].AsString];
end;

procedure TSynDWScript.DataModuleCreate(Sender: TObject);
begin
   FSystemInfo:=TdwsSystemInfoLibModule.Create(Self);
   FSystemInfo.dwsSystemInfo.Script:=DelphiWebScript;

   FCompiledPrograms:=TCompiledProgramHash.Create;
   FCompiledProgramsLock:=TFixedCriticalSection.Create;
   FCompilerLock:=TFixedCriticalSection.Create;

   FCompilerEnvironment:=TRTTIEnvironment.Create;
   FCompilerEnvironment.SetForClass(TWebEnvironment);

   DelphiWebScript.Extensions.Add(FCompilerEnvironment);

   FScriptTimeoutMilliseconds:=30000;
end;

procedure TSynDWScript.DataModuleDestroy(Sender: TObject);
begin
   DelphiWebScript.Extensions.Remove(FCompilerEnvironment);

   FlushDWSCache;

   FCompilerEnvironment.Free;

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
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   webenv : TWebEnvironment;
begin
   if (CPUUsageLimit>0) and not WaitForCPULimit then begin

      response.StatusCode:=503;
      response.ContentData:='CPU Usage limit reached, please try again later';
      response.ContentType:='text/plain';
      Exit;

   end;

   TryAcquireDWS(fileName, prog);
   if prog=nil then
      CompileDWS(fileName, prog);

   if prog.Msgs.HasErrors then begin

      response.StatusCode:=200;
      response.ContentData:=UTF8Encode(prog.Msgs.AsInfo);
      response.ContentType:='text/plain; charset=utf-8';

   end else begin

      webenv:=TWebEnvironment.Create;
      webenv.Request:=request;
      webenv.response:=response;
      try
         exec:=prog.CreateNewExecution;
         exec.Environment:=TRTTIRuntimeEnvironment.Create(webenv);
         exec.BeginProgram;
         exec.RunProgram(ScriptTimeoutMilliseconds);
         exec.EndProgram;
         exec.Environment:=nil;
      finally
         webenv.Free;
      end;

      if exec.Msgs.Count>0 then begin
         response.StatusCode:=400;
         response.ContentData:=UTF8Encode(exec.Msgs.AsInfo);
      end else begin
         if response.ContentData='' then
            response.ContentData:=UTF8Encode(exec.Result.ToString);
      end;
      response.AllowCORS:='*';
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

// SetCPUUsageLimit
//
procedure TSynDWScript.SetCPUUsageLimit(const val : Integer);
begin
   FCPUUsageLimit:=val;
   if FCPUUsageLimit>0 then
      SystemCPU.Track;
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
      if i=10 then Exit(False);
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

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  RegisterRTTIIndexedProperty(TWebRequest, 'Headers', True, varUString, TWebRequest_GetHeaders, nil);
  RegisterRTTIIndexedProperty(TWebRequest, 'Cookies', True, varUString, TWebRequest_GetCookies, nil);
  RegisterRTTIIndexedProperty(TWebRequest, 'QueryFields', True, varUString, TWebRequest_GetQueryFields, nil);

end.
