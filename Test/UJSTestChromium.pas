unit UJSTestChromium;

interface

uses
   Forms, Classes, SysUtils, TestFrameWork, Windows, Messages,
   uCEFChromium, uCEFChromiumWindow, uCEFInterfaces, uCEFTypes, uCEFApplication,
   uCEFSentinel, uCEFConstants;

type
   ITestChromium = interface
      function Initialized : Boolean;

      procedure LoadURL(const url : String);
      procedure StopLoad;

      procedure ExecuteJavaScript(const js, url : String);

      procedure ExecuteAndWait(const js, url : String);
      procedure LoadAndWait(const html, url : String);

      function GetLastJSResult : String;
      procedure SetLastJSResult(const v : String);
      property LastJSResult : String read GetLastJSResult write SetLastJSResult;

      function GetConsole : String;
      procedure SetConsole(const v : String);
      property Console : String read GetConsole write SetConsole;
   end;

function CreateTestChromium : ITestChromium;

implementation

uses dwsUtils, dwsJSON;

type
   TTestChromium = class;

   TTestChromiumForm = class (TForm)
      public
         FLastJSResult : String;
         FConsole : String;
         FChromium : TChromiumWindow;
         FSentinel : TCEFSentinel;
         FOwner : TTestChromium;
         destructor Destroy; override;
         procedure FormCloseQuery(Sender : TObject; var canClose : Boolean);
         procedure SentinelClose(Sender: TObject);
         procedure DoJSDialog(
            Sender: TObject; const browser: ICefBrowser; const originUrl: ustring;
            dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring;
            const callback: ICefJsDialogCallback; out suppressMessage: Boolean; out Result: Boolean);
         procedure DoConsoleMessage(Sender: TObject; const browser: ICefBrowser;
            level: TCefLogSeverity; const message, source: ustring; line: Integer; out Result: Boolean);
   end;

   TTestChromium = class (TInterfacedObject, ITestChromium)
      FForm : TTestChromiumForm;
      constructor Create;
      destructor Destroy; override;
      function Initialized : Boolean;
      procedure LoadURL(const url : String);
      procedure StopLoad;
      procedure ExecuteJavaScript(const js, url : String);
      procedure ExecuteAndWait(const js, url : String);
      procedure LoadAndWait(const html, url : String);
      function GetLastJSResult : String;
      procedure SetLastJSResult(const v : String);
      function GetConsole : String;
      procedure SetConsole(const v : String);
   end;

procedure TTestChromiumForm.DoJSDialog(
            Sender: TObject; const browser: ICefBrowser; const originUrl: ustring;
            dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring;
            const callback: ICefJsDialogCallback; out suppressMessage: Boolean; out Result: Boolean);
begin
   FLastJSResult := messageText;
   Result := True;
end;

procedure TTestChromiumForm.DoConsoleMessage(Sender: TObject; const browser: ICefBrowser;
            level: TCefLogSeverity; const message, source: ustring; line: Integer; out Result: Boolean);
begin
   FConsole := FConsole + message;
   Result := True;
end;

procedure TTestChromiumForm.FormCloseQuery(Sender : TObject; var canClose : Boolean);
begin
   if FChromium <> nil then begin
      canClose := False;
      if FSentinel.Status = ssIdle then begin
         FChromium.CloseBrowser(True);
         FSentinel.Start;
      end;
   end else begin
      canClose := True;
      Release;
   end;
end;

procedure TTestChromiumForm.SentinelClose(Sender: TObject);
begin
   FreeAndNil(FChromium);
   Release;
end;

// Destroy
//
destructor TTestChromiumForm.Destroy;
begin
   FOwner.FForm := nil;
   inherited;
end;

// ------------------
// ------------------ TTestChromium ------------------
// ------------------

// Create
//
constructor TTestChromium.Create;
begin
   inherited;
   Assert(GlobalCEFApp.GlobalContextInitialized);

   FForm := TTestChromiumForm.CreateNew(nil);
   FForm.OnCloseQuery := FForm.FormCloseQuery;

   FForm.FSentinel := TCEFSentinel.Create(FForm);
   FForm.FSentinel.OnClose := FForm.SentinelClose;

   FForm.FChromium := TChromiumWindow.Create(FForm);
   FForm.FOwner := Self;
   FForm.FChromium.ChromiumBrowser.OnJsdialog := FForm.DoJSDialog;
   FForm.FChromium.ChromiumBrowser.OnConsoleMessage := FForm.DoConsoleMessage;
   FForm.FChromium.Parent := FForm;
   FForm.FChromium.CreateBrowser;
   FForm.FChromium.ChromiumBrowser.LoadURL('about:blank');
end;

// Destroy
//
destructor TTestChromium.Destroy;
begin
   inherited;
   if FForm <> nil then begin
      FForm.Close;
      while FForm <> nil do begin
         Sleep(20);
         Application.ProcessMessages;
      end;
   end;
end;

// Initialized
//
function TTestChromium.Initialized : Boolean;
begin
   Result := (FForm <> nil) and (FForm.FChromium <> nil) and FForm.FChromium.Initialized;
end;

// LoadURL
//
procedure TTestChromium.LoadURL(const url : String);
begin
   FForm.FChromium.LoadURL(url);
end;

// StopLoad
//
procedure TTestChromium.StopLoad;
begin
   FForm.FChromium.ChromiumBrowser.StopLoad;
end;

// ExecuteJavaScript
//
procedure TTestChromium.ExecuteJavaScript(const js, url : String);
begin
   FForm.FChromium.ChromiumBrowser.ExecuteJavaScript(js, url);
end;

// ExecuteAndWait
//
procedure TTestChromium.ExecuteAndWait(const js, url : String);
var
   prevResult, prevConsole : String;
   k : Integer;
begin
   prevResult := GetLastJSResult;
   prevConsole := GetConsole;
   ExecuteJavaScript(js, url);
   for k := 1 to 300 do begin
      if (GetLastJSResult <> prevResult) or (GetConsole <> prevConsole) then break;
      Application.ProcessMessages;
      case k of
         0..99 : Sleep(1);
         100..199 : Sleep(10);
      else
         Sleep(20);
      end;
   end;
end;

// LoadAndWait
//
procedure TTestChromium.LoadAndWait(const html, url : String);
var
   wobs : TWriteOnlyBlockStream;
begin
   wobs := TWriteOnlyBlockStream.AllocFromPool;
   try
      wobs.WriteString('document.open();document.write(');
      WriteJavaScriptString(wobs, html);
      wobs.WriteString(');document.close();');
      ExecuteAndWait(wobs.ToString, url);
   finally
      wobs.ReturnToPool;
   end;
end;

// GetLastJSResult
//
function TTestChromium.GetLastJSResult : String;
begin
   Result := FForm.FLastJSResult;
end;

// SetLastJSResult
//
procedure TTestChromium.SetLastJSResult(const v : String);
begin
   FForm.FLastJSResult := v;
end;

// GetConsole
//
function TTestChromium.GetConsole : String;
begin
   Result := FForm.FConsole;
end;

// SetConsole
//
procedure TTestChromium.SetConsole(const v : String);
begin
   FForm.FConsole := v;
end;

// CreateTestChromium
//
function CreateTestChromium : ITestChromium;
begin
   Result := TTestChromium.Create;
end;

end.
