unit dwsWebServerLibModule;

interface

uses
   SysUtils, Classes, SynCommons,
   dwsComp, dwsExprs, dwsSymbols, dwsUtils,
   dwsWebServerInfo, dwsWebEnvironment;

type
  TdwsWebServerLib = class(TDataModule)
    dwsWebServer: TdwsUnit;
    procedure dwsWebServerClassesWebServerMethodsNameEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebServerClassesWebServerMethodsHttpPortEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebServerClassesWebServerMethodsHttpsPortEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebServerClassesWebServerMethodsAuthenticationsEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebServerClassesWebServerMethodsFlushCompiledProgramsEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebServerClassesWebServerMethodsLiveQueriesEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebServerClassesWebServerSentEventsMethodsPostRawEventEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebServerClassesWebServerSentEventsMethodsCloseEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebServerClassesWebServerSentEventsMethodsConnectionsEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebServerClassesWebServerSentEventsMethodsSourceNamesEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebServerClassesWebServerSentEventMethodsPostEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebServerClassesWebServerSentEventMethodsToRawDataEval(
      Info: TProgramInfo; ExtObject: TObject);
  private
    { Private declarations }
    FServer :  IWebServerInfo;
  public
    { Public declaration }
    property Server : IWebServerInfo read FServer write FServer;
  end;

implementation

{$R *.dfm}

function WebServerSentEventToRawData(const obj : TScriptObjInstance) : RawByteString;
var
   i : Integer;
   dyn : TScriptDynamicStringArray;
   buf : String;
begin
   buf := obj.AsString[obj.FieldAddress('ID')];
   if buf <> '' then
      Result := 'id: ' + StringToUTF8(buf) + #10;
   buf := obj.AsString[obj.FieldAddress('Name')];
   if buf <> '' then
      Result := Result + 'event: ' + StringToUTF8(buf) + #10;
   i := obj.AsInteger[obj.FieldAddress('Retry')];
   if i > 0 then
      Result := Result + 'retry: ' + ScriptStringToRawByteString(IntToStr(i)) + #10;
   dyn := (obj.AsInterface[obj.FieldAddress('Data')] as IScriptDynArray).GetSelf as TScriptDynamicStringArray;
   for i := 0 to dyn.ArrayLength-1 do
      Result := Result + 'data: ' + StringToUTF8(dyn.AsString[i]) + #10;
   Result := Result + #10;
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerMethodsAuthenticationsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=0;
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerMethodsFlushCompiledProgramsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   FServer.FlushCompiledPrograms;
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerMethodsHttpPortEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=FServer.HttpPort;
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerMethodsHttpsPortEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=FServer.HttpsPort;
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerMethodsLiveQueriesEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=FServer.LiveQueries;
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerMethodsNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=FServer.Name;
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerSentEventMethodsPostEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   obj : TScriptObjInstance;
begin
   obj := Info.ScriptObj.GetSelf as TScriptObjInstance;
   FServer.EventSourcePost(
      Info.ParamAsString[0],
      obj.AsString[obj.FieldAddress('ID')],
      obj.AsString[obj.FieldAddress('name')],
      WebServerSentEventToRawData(obj));
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerSentEventMethodsToRawDataEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   obj : TScriptObjInstance;
begin
   obj := Info.ScriptObj.GetSelf as TScriptObjInstance;
   Info.ResultAsDataString := WebServerSentEventToRawData(obj);
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerSentEventsMethodsCloseEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   FServer.EventSourceClose(Info.ParamAsString[0]);
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerSentEventsMethodsConnectionsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsStringArray := FServer.EventSourceConnections(Info.ParamAsString[0]);
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerSentEventsMethodsPostRawEventEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   FServer.EventSourcePost(Info.ParamAsString[0], Info.ParamAsString[1], Info.ParamAsString[2],
                           Info.ParamAsDataString[3]);
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerSentEventsMethodsSourceNamesEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsStringArray := FServer.EventSourceList;
end;

end.
