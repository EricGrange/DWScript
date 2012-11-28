unit DSimpleDWScript;

interface

uses
  SysUtils, Classes, dwsFileSystem, dwsGlobalVarsFunctions, dwsCompiler,
  dwsHtmlFilter, dwsComp, SyncObjs, dwsExprs, dwsRTTIConnector, dwsUtils,
  dwsWebEnvironment, Rtti;

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

      FFileSystem : TdwsCustomFileSystem;

      FCompiledPrograms : TCompiledProgramHash;
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
   FCompiledPrograms:=TCompiledProgramHash.Create;
   FCompiledProgramsLock:=TFixedCriticalSection.Create;
   FCompilerLock:=TFixedCriticalSection.Create;

   FCompilerEnvironment:=TRTTIEnvironment.Create;
   FCompilerEnvironment.SetForClass(TWebEnvironment);

   DelphiWebScript.Extensions.Add(FCompilerEnvironment);

   FScriptTimeoutMilliseconds:=3000;
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
   TryAcquireDWS(fileName, prog);
   if prog=nil then
      CompileDWS(fileName, prog);

   if prog.Msgs.HasErrors then begin

      response.StatusCode:=400;
      response.ContentData:=UTF8Encode(prog.Msgs.AsInfo);

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
         response.ContentData:=UTF8Encode(prog.Msgs.AsInfo);
      end else begin
         if response.ContentData='' then
            response.ContentData:=UTF8Encode(exec.Result.ToString);
      end;
   end;
end;

// FlushDWSCache
//
procedure TSynDWScript.FlushDWSCache;
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
