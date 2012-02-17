unit DSimpleDWScript;

interface

uses
  SysUtils, Classes, dwsFileSystem, dwsGlobalVarsFunctions, dwsCompiler,
  dwsHtmlFilter, dwsComp, SyncObjs, Generics.Collections, dwsExprs, HTTPApp,
  dwsRTTIConnector, dwsUtils;

type
   {$M+}
   TWebEnvironment = class
      published
         Request : TWebRequest;
         Response : TWebResponse;
   end;
   {$M-}

  TSimpleDWScript = class(TDataModule)
      DelphiWebScript: TDelphiWebScript;
      dwsHtmlFilter: TdwsHtmlFilter;
      dwsGlobalVarsFunctions: TdwsGlobalVarsFunctions;
      dwsRTTIConnector: TdwsRTTIConnector;
      procedure DataModuleCreate(Sender: TObject);
      procedure DataModuleDestroy(Sender: TObject);

   private
      FScriptTimeoutMilliseconds : Integer;

      FFileSystem : TdwsCustomFileSystem;

      FCompiledPrograms : TDictionary<String,IdwsProgram>;
      FCompiledProgramsLock : TFixedCriticalSection;

      FCompilerEnvironment : TRTTIEnvironment;

      FCompilerLock : TFixedCriticalSection;

   protected
      function GetFileSystem : TdwsCustomFileSystem;
      procedure SetFileSystem(const val : TdwsCustomFileSystem);

      procedure TryAcquireDWS(const fileName : String; var prog : IdwsProgram);
      procedure CompileDWS(const fileName : String; var prog : IdwsProgram);

   public
      procedure HandleDWS(const fileName : String; request : TWebRequest; response : TWebResponse);

      procedure FlushDWSCache;

      property ScriptTimeoutMilliseconds : Integer read FScriptTimeoutMilliseconds write FScriptTimeoutMilliseconds;
      property FileSystem : TdwsCustomFileSystem read GetFileSystem write SetFileSystem;
  end;

var
  SimpleDWScript: TSimpleDWScript;

implementation

{$R *.dfm}

procedure TSimpleDWScript.DataModuleCreate(Sender: TObject);
begin
   FCompiledPrograms:=TDictionary<String,IdwsProgram>.Create;
   FCompiledProgramsLock:=TFixedCriticalSection.Create;
   FCompilerLock:=TFixedCriticalSection.Create;

   FCompilerEnvironment:=TRTTIEnvironment.Create;
   FCompilerEnvironment.SetForClass(TWebEnvironment);

   DelphiWebScript.Extensions.Add(FCompilerEnvironment);

   FScriptTimeoutMilliseconds:=3000;
end;

procedure TSimpleDWScript.DataModuleDestroy(Sender: TObject);
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
function TSimpleDWScript.GetFileSystem : TdwsCustomFileSystem;
begin
   Result:=DelphiWebScript.Config.CompileFileSystem;
end;

// SetFileSystem
//
procedure TSimpleDWScript.SetFileSystem(const val : TdwsCustomFileSystem);
begin
   DelphiWebScript.Config.CompileFileSystem:=val;
   DelphiWebScript.Config.RuntimeFileSystem:=val;
end;

// HandleDWS
//
procedure TSimpleDWScript.HandleDWS(const fileName : String; Request: TWebRequest; Response: TWebResponse);
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   webenv : TWebEnvironment;
begin
   TryAcquireDWS(fileName, prog);
   if prog=nil then
      CompileDWS(fileName, prog);

   if prog.Msgs.HasErrors then begin

      Response.StatusCode:=400;
      Response.Content:=prog.Msgs.AsInfo;

   end else begin

      webenv:=TWebEnvironment.Create;
      webenv.Request:=request;
      webenv.Response:=response;
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
         Response.StatusCode:=400;
         Response.Content:=prog.Msgs.AsInfo;
      end else begin
         Response.Content:=exec.Result.ToString;
      end;
   end;
end;

// FlushDWSCache
//
procedure TSimpleDWScript.FlushDWSCache;
begin
   FCompiledProgramsLock.Enter;
   try
      FCompiledPrograms.Clear;
   finally
      FCompiledProgramsLock.Leave;
   end;
end;

// TryAcquireDWS
//
procedure TSimpleDWScript.TryAcquireDWS(const fileName : String; var prog : IdwsProgram);
begin
   FCompiledProgramsLock.Enter;
   try
      FCompiledPrograms.TryGetValue(fileName, prog);
   finally
      FCompiledProgramsLock.Leave;
   end;
end;

// CompileDWS
//
procedure TSimpleDWScript.CompileDWS(const fileName : String; var prog : IdwsProgram);
var
   sl : TStringList;
   code : String;
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

      FCompiledProgramsLock.Enter;
      try
         FCompiledPrograms.Add(fileName, prog);
      finally
         FCompiledProgramsLock.Leave;
      end;
   finally
      FCompilerLock.Leave;
   end;
end;

end.
