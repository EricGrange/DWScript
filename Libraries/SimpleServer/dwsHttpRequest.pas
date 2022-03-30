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
unit dwsHttpRequest;

interface

uses
   System.Classes, System.Types, System.SysUtils,
   SynCrtSock, SynCommons, SynWinSock,
   dwsCustomData, dwsXPlatform, dwsWinHTTP, dwsExprs;

const
   cWinHttpConnection : TGUID = '{0B47FA19-7BE9-41AE-A3BD-2C686117D669}';

   cWinHttpCredentials : TGUID = '{FB60EB3D-1085-4A88-9923-DE895B5CAB76}';
   cWinHttpIgnoreSSLCertificateErrors : TGUID = '{42AC8563-761B-4E3D-9767-A21F8F32201C}';
   cWinHttpKeepAlive : TGUID = '{6081C40E-EED1-4421-A7B4-15E4D1942A15}';
   cWinHttpProxyName : TGUID = '{2449F585-D6C6-4FDC-8D86-0266E01CA99C}';
   cWinHttpConnectTimeout : TGUID = '{8D322334-D1DD-4EBF-945F-193CFCA001FB}';
   cWinHttpSendTimeout : TGUID = '{1DE21769-65B5-4039-BB66-62D405FB00B7}';
   cWinHttpReceiveTimeout : TGUID = '{0D14B470-4F8A-48AE-BAD2-426E15FE4E03}';
   cWinHttpCustomHeaders : TGUID = '{FD05B54E-FBF2-498A-BD1F-0B1F18F27A1E}';

   cWinHttpSynchronousRequest : TGUID = '{7D0B442B-0D52-4D05-95A1-3964FAB588CA}';

   cWinHttpDefaultKeepAlive = True;

type
   THttpRequest = class
      Method, URL : RawByteString;
      RequestData, RequestContentType : RawByteString;
      ResponseData : SockString;
      RawResponseHeaders : SockString;
      FResponseHeaders : TStrings;
      CustomStates : TdwsCustomStates;
      CurrentSize, ContentLength : DWORD;
      StatusCode : Integer;
      Error : String;
      CertificateInfo : TdwsHttpCertificateInfo;
      Released : Boolean;

      constructor Create;
      destructor Destroy; override;

      procedure PrepareResponseHeaders;
      function GetResponseHeader(const name : String) : String;
      procedure Execute(exec : TdwsProgramExecution);
      procedure DoProgress(Sender: TWinHttpAPI; CurrentSize, ContentLength: DWORD);
   end;

   THttpRequestContainer = class
      public
         function Request : THttpRequest; virtual; abstract;
         function Wait : THttpRequest; virtual; abstract;
         procedure Release; virtual; abstract;
         function Completed : Boolean; virtual; abstract;

         class function CreateAsync(aRequest : THttpRequest) : THttpRequestContainer;
         class function CreateSynchronous(exec : TdwsProgramExecution; aRequest : THttpRequest) : THttpRequestContainer;
   end;

function HttpQuery(exec : TdwsProgramExecution;
                   const method, url : RawByteString;
                   const requestData, requestContentType : RawByteString;
                   var replyHeaders, replyData : SockString;
                   onProgress : TWinHttpProgress = nil;
                   customStates : TdwsCustomStates = nil;
                   certificateInfo : TdwsHttpCertificateInfo = nil) : Integer;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsUtils, System.StrUtils;

type
   THttpRequestThread = class (TThread)
      Request : THttpRequest;
      ReleaseLock : TMultiReadSingleWrite;
      Completed : Boolean;
      constructor CreateQuery(request : THttpRequest);
      destructor Destroy; override;
      procedure Execute; override;
      procedure Release;
      function Wait : THttpRequest;
   end;

   THttpRequestAsyncContainer = class (THttpRequestContainer)
      FThread : THttpRequestThread;
      constructor Create(aRequest : THttpRequest);
      destructor Destroy; override;
      function Request : THttpRequest; override;
      function Wait : THttpRequest; override;
      procedure Release; override;
      function Completed : Boolean; override;
   end;

   THttpRequestSynchronousContainer = class (THttpRequestContainer)
      FRequest : THttpRequest;
      constructor Create(exec : TdwsProgramExecution; aRequest : THttpRequest);
      destructor Destroy; override;
      function Request : THttpRequest; override;
      function Wait : THttpRequest; override;
      procedure Release; override;
      function Completed : Boolean; override;
   end;

// HttpQuery
//
function HttpQuery(exec : TdwsProgramExecution;
                   const method, url : RawByteString;
                   const requestData, requestContentType : RawByteString;
                   var replyHeaders, replyData : SockString;
                   onProgress : TWinHttpProgress = nil;
                   customStates : TdwsCustomStates = nil;
                   certificateInfo : TdwsHttpCertificateInfo = nil) : Integer;
var
   uri : TURI;
   conn : TdwsWinHttpConnection;
   iconn : IGetSelf;
   unassignedVariant : Variant;
   keepAlive : Boolean;
begin
   if not uri.From(url) then
      raise Exception.CreateFmt('Invalid url "%s"', [url]);

   if exec <> nil then
      iconn := exec.CustomInterfaces[cWinHttpConnection] as IGetSelf;
   if iconn = nil then begin
      iconn := IGetSelf(TdwsWinHttpConnection.Create);
      if exec <> nil then
         exec.CustomInterfaces[cWinHttpConnection] := iconn;
   end;

   try
      if (customStates = nil) and (exec <> nil) then begin
         if exec.HasCustomStates then
            customStates := exec.CustomStates;
      end;

      conn := iconn.GetSelf as TdwsWinHttpConnection;
      if customStates <> nil then begin
         conn.ConnectServer(uri, customStates.StringStateDef(cWinHttpProxyName, ''),
                            customStates.IntegerStateDef(cWinHttpConnectTimeout, HTTP_DEFAULT_CONNECTTIMEOUT),
                            customStates.IntegerStateDef(cWinHttpSendTimeout, HTTP_DEFAULT_SENDTIMEOUT),
                            customStates.IntegerStateDef(cWinHttpReceiveTimeout, HTTP_DEFAULT_RECEIVETIMEOUT));
         conn.SetIgnoreSSLErrors(customStates[cWinHttpIgnoreSSLCertificateErrors]);
         conn.SetCredentials(customStates[cWinHttpCredentials]);
         conn.SetCustomHeaders(customStates[cWinHttpCustomHeaders]);
         keepAlive := customStates.BooleanStateDef(cWinHttpKeepAlive, cWinHttpDefaultKeepAlive);
      end else begin
         conn.ConnectServer(uri, '', HTTP_DEFAULT_CONNECTTIMEOUT, HTTP_DEFAULT_SENDTIMEOUT, HTTP_DEFAULT_RECEIVETIMEOUT);
         conn.SetIgnoreSSLErrors(unassignedVariant);
         conn.SetCredentials(unassignedVariant);
         conn.SetCustomHeaders(unassignedVariant);
         keepAlive := cWinHttpDefaultKeepAlive;
      end;
      conn.SetOnProgress(onProgress);
      conn.FWinHttp.CertificateInfo := certificateInfo;

      Result := conn.Request(uri, method, Ord(keepAlive), '', requestData, requestContentType, replyHeaders, replyData);

   except
      on EWinHTTP do begin
         if exec <> nil then
            exec.CustomInterfaces[cWinHttpConnection] := nil;
         raise;
      end;
   end;
end;

// ------------------
// ------------------ THttpRequest ------------------
// ------------------

// Create
//
constructor THttpRequest.Create;
begin
   inherited Create;
   CertificateInfo := TdwsHttpCertificateInfo.Create;
end;

// Destroy
//
destructor THttpRequest.Destroy;
begin
   inherited;
   FResponseHeaders.Free;
   CustomStates.Free;
   CertificateInfo.Free;
end;

// PrepareResponse
//
procedure THttpRequest.PrepareResponseHeaders;

   procedure AddHeader(const s : String);
   var
      k : Integer;
   begin
      k := Pos(':', s);
      if k > 0 then begin
         FResponseHeaders.Add(
              System.SysUtils.TrimRight(Copy(s, 1, k-1)
            + '='
            + System.SysUtils.Trim(Copy(s, k+1)))
         );
      end;
   end;

var
   h : String;
   p, pn : Integer;
begin
   FResponseHeaders := TFastCompareTextList.Create;
   if (RawResponseHeaders <> '') and (not Released) then begin
      RawByteStringToScriptString(RawResponseHeaders, h);

      p := 1;
      while True do begin
         pn := System.StrUtils.PosEx(#13#10, h, p);
         if pn > 0 then begin
            AddHeader(Copy(h, p, pn-p));
            p := pn + 2;
         end else break;
      end;
      AddHeader(Copy(h, p));
   end;
end;

// GetResponse
//
function THttpRequest.GetResponseHeader(const name : String) : String;
begin
   if FResponseHeaders = nil then
      PrepareResponseHeaders;
   Result := FResponseHeaders.Values[name];
end;

// Execute
//
procedure THttpRequest.Execute(exec : TdwsProgramExecution);
begin
   if not Released then try
      StatusCode := HttpQuery(exec, Method, URL, RequestData, RequestContentType,
                              RawResponseHeaders, ResponseData,
                              DoProgress, CustomStates, CertificateInfo);
      FreeAndNil(CustomStates);
      RequestData := '';
      RequestContentType := '';
      ContentLength := Length(ResponseData);
      CurrentSize := ContentLength;
   except
      on E: Exception do
         Error := E.Message;
   end;
end;

// DoProgress
//
procedure THttpRequest.DoProgress(Sender: TWinHttpAPI; CurrentSize, ContentLength: DWORD);
begin
   Self.CurrentSize := CurrentSize;
   Self.ContentLength := ContentLength;
end;

// ------------------
// ------------------ THttpRequestThread ------------------
// ------------------

// CreateQuery
//
constructor THttpRequestThread.CreateQuery(request : THttpRequest);
begin
   inherited Create;
   Self.Request := request;
   FreeOnTerminate := False;
   ReleaseLock := TMultiReadSingleWrite.Create;
end;

// Destroy
//
destructor THttpRequestThread.Destroy;
begin
   inherited;
   ReleaseLock.Free;
   Request.Free;
end;

// Execute
//
procedure THttpRequestThread.Execute;
begin
   Request.Execute(nil);

   ReleaseLock.BeginWrite;
   Completed := True;
   if Request.Released then
      FreeOnTerminate := True;
   ReleaseLock.EndWrite;
end;

// Release
//
procedure THttpRequestThread.Release;
begin
   ReleaseLock.BeginWrite;
   if Completed then begin
      if not Finished then
         WaitFor;
      Free;
   end else begin
      Request.Released := True;
      ReleaseLock.EndWrite;
   end;
end;

// Wait
//
function THttpRequestThread.Wait : THttpRequest;
begin
   if not Finished then WaitFor;
   Result := Request;
end;

// ------------------
// ------------------ THttpRequestAsyncContainer ------------------
// ------------------

// Create
//
constructor THttpRequestAsyncContainer.Create(aRequest : THttpRequest);
begin
   FThread := THttpRequestThread.CreateQuery(aRequest);
end;

// Destroy
//
destructor THttpRequestAsyncContainer.Destroy;
begin
   inherited;
   if FThread <> nil then
      FThread.Release;
end;

// Request
//
function THttpRequestAsyncContainer.Request : THttpRequest;
begin
   Result := FThread.Request;
end;

// Wait
//
function THttpRequestAsyncContainer.Wait : THttpRequest;
begin
   Result := FThread.Wait;
end;

// Release
//
procedure THttpRequestAsyncContainer.Release;
begin
   Destroy;
end;

// Completed
//
function THttpRequestAsyncContainer.Completed : Boolean;
begin
   Result := FThread.Completed;
end;

// ------------------
// ------------------ THttpRequestSynchronousContainer ------------------
// ------------------

// Create
//
constructor THttpRequestSynchronousContainer.Create(exec : TdwsProgramExecution; aRequest : THttpRequest);
begin
   FRequest := aRequest;
   FRequest.Execute(exec);
end;

// Destroy
//
destructor THttpRequestSynchronousContainer.Destroy;
begin
   inherited;
   FRequest.Free;
end;

// Request
//
function THttpRequestSynchronousContainer.Request : THttpRequest;
begin
   Result := FRequest;
end;

// Wait
//
function THttpRequestSynchronousContainer.Wait : THttpRequest;
begin
   Result := FRequest;
end;

// Release
//
procedure THttpRequestSynchronousContainer.Release;
begin
   Destroy;
end;

// Completed
//
function THttpRequestSynchronousContainer.Completed : Boolean;
begin
   Result := True;
end;

// ------------------
// ------------------ THttpRequestContainer ------------------
// ------------------

// CreateAsync
//
class function THttpRequestContainer.CreateAsync(aRequest : THttpRequest) : THttpRequestContainer;
begin
   Result := THttpRequestAsyncContainer.Create(aRequest);
end;

// CreateSynchronous
//
class function THttpRequestContainer.CreateSynchronous(exec : TdwsProgramExecution; aRequest : THttpRequest) : THttpRequestContainer;
begin
   Result := THttpRequestSynchronousContainer.Create(exec, aRequest);
end;

end.
