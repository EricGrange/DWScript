unit dwsSynapseLibModule;

interface

uses
  SysUtils, Classes, Masks,
  dwsStrings, dwsUtils, dwsExprList, dwsXPlatform,
  dwsComp, dwsExprs, dwsSymbols, dwsStack, dwsDatabase, dwsJSON, dwsErrors,
  dwsWebUtils,
  smtpsend, httpsend;

type

  TdwsSynapseLib = class(TDataModule)
    dwsSynapse: TdwsUnit;
    procedure dwsSynapseClassesSMTPMailMethodsSendEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsSynapseClassesHttpQueryMethodsGetTextEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsSynapseClassesHttpQueryMethodsGetDataEval(Info: TProgramInfo;
      ExtObject: TObject);
  private
    { Private declarations }
    procedure SetScript(aScript : TDelphiWebScript);
    procedure RaiseSynapseException(Info: TProgramInfo; const msg : String);
    procedure RaiseSMTPException(Info: TProgramInfo; smtp : TSMTPSend; const action : String);

  public
    { Public declarations }
    property Script : TDelphiWebScript write SetScript;
  end;

implementation

{$R *.dfm}

// SetScript
//
procedure TdwsSynapseLib.SetScript(aScript : TDelphiWebScript);
begin
   dwsSynapse.Script:=aScript;
end;

// RaiseSynapseException
//
procedure TdwsSynapseLib.RaiseSynapseException(Info: TProgramInfo; const msg : String);
var
   exceptObj : IScriptObj;
begin
   exceptObj:=Info.Vars['ESynapseException'].Method[SYS_TOBJECT_CREATE].Call([msg]).ScriptObj;
   (exceptObj.ExternalObject as TdwsExceptionContext).Skip(1); // temporary constructor expression
   Info.RaiseExceptObj(msg, exceptObj);
end;

// RaiseSMTPException
//
procedure TdwsSynapseLib.RaiseSMTPException(Info: TProgramInfo; smtp : TSMTPSend; const action : String);
begin
   RaiseSynapseException(Info, 'SMTP, '+action+': '+smtp.EnhCodeString+','+smtp.ResultString);
end;

// SynapseHttpQuery
//
function SynapseHttpQuery(const url : String; var data : String; asText : Boolean) : Integer;
var
   query : THTTPSend;
   n : Integer;
   buf : RawByteString;
begin
   query := THTTPSend.Create;
   try
      if query.HTTPMethod('GET', url) then begin
         n := query.Document.Size;
         SetLength(buf, n);
         System.Move(query.Document.Memory^, Pointer(buf)^, n);
         if asText and StrIEndsWith(query.MimeType, 'charset=utf-8') then
            data := UTF8ToUnicodeString(buf)
         else RawByteStringToScriptString(buf, data);
         Result := query.ResultCode;
      end else begin
         Result := 0;
      end;
  finally
      query.Free;
  end;
end;

procedure TdwsSynapseLib.dwsSynapseClassesHttpQueryMethodsGetDataEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   buf : String;
begin
   Info.ResultAsInteger := SynapseHttpQuery(Info.ParamAsString[0], buf, False);
   Info.ParamAsString[1] := buf;
end;

procedure TdwsSynapseLib.dwsSynapseClassesHttpQueryMethodsGetTextEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   buf : String;
begin
   Info.ResultAsInteger := SynapseHttpQuery(Info.ParamAsString[0], buf, True);
   Info.ParamAsString[1] := buf;
end;

procedure TdwsSynapseLib.dwsSynapseClassesSMTPMailMethodsSendEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   smtp : TSMTPSend;
   msg : TStringList;
   from, sendTo, subject, body, headers : String;
begin
   from:=Info.ParamAsString[0];
   sendTo:=Info.ParamAsString[1];
   subject:=Info.ParamAsString[2];
   body:=Info.ParamAsString[3];
   headers:=Info.ParamAsString[4];

   msg:=TStringList.Create;
   smtp:=TSMTPSend.Create;
   try
      smtp.TargetHost:='127.0.0.1';

      if headers<>'' then begin
         if not StrEndsWith(headers, #13#10) then
            headers:=headers+#13#10;
         msg.Text:=headers+#13#10+body
      end;

      msg.Insert(0, 'From: '+from);
      msg.Insert(1, 'To: '+sendTo);
      msg.Insert(2, 'Subject: '+WebUtils.EncodeEncodedWord(subject));

      if not smtp.Login() then
         RaiseSMTPException(Info, smtp, 'Login');

      if not smtp.MailFrom(from, Length(from)) then
         RaiseSMTPException(Info, smtp, 'MailFrom');

      if not smtp.MailTo(sendTo) then
         RaiseSMTPException(Info, smtp, 'MailTo');

      if not smtp.MailData(msg) then
         RaiseSMTPException(Info, smtp, 'MailData');

      if not smtp.Logout() then
         RaiseSMTPException(Info, smtp, 'Logout');
   finally
      msg.Free;
      smtp.Free;
   end;
end;

end.
