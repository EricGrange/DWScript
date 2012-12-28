unit USynopseSimpleWebServer;

interface

uses
  Windows, SysUtils,
  SynCommons, SynZip,
  dwsHTTPSysServer, dwsHTTPSysAPI,
  dwsUtils, dwsWebEnvironment, dwsSynopseWebEnv, dwsFileSystem,
  dwsDirectoryNotifier, dwsJSON, dwsXPlatform,
  DSimpleDWScript;

type

   TSynopseSimpleServer = class
      protected
         FPath : TFileName;
         FServer : THttpApi2Server;
         FFileSystem : TdwsRestrictedFileSystem;
         FDWS : TSynDWScript;
         FNotifier : TdwsDirectoryNotifier;
         FPort : Integer;
         FSSLPort : Integer;

         procedure DirectoryChanged(sender : TdwsDirectoryNotifier);

      public
         constructor Create(const basePath : TFileName; options : TdwsJSONValue);
         destructor Destroy; override;

         function Process(const inRequest : TSynHttpServerRequest;
                          var outResponse : TSynHttpServerResponse) : cardinal;

         function FindDirectoryIndex(var pathFileName : String) : Boolean;

         property Port : Integer read FPort;
         property SSLPort : Integer read FSSLPort;
  end;

const
   cDefaultServerOptions =
      '{'
         // http server port, if zero, no http port is opened
         +'"Port": 888,'
         // https server port, if zero, no https port is opened
         +'"SSLPort": 0,'
         // Base path for served files
         // If not defined, assumes a www subfolder of the folder where the exe is
         +'"WWWPath": "",'
         // Directory for log files (NCSA)
         // If empty, logs are not active
         +'"LogDirectory": "",'
         // Maximum bandwidth in bytes per second
         // Zero and negative values mean "infinite"
         +'"MaxBandwidth": "",'
         // Maximum input http request length in bytes
         // If zero or negative, defaults to 10 Megabytes
         // requests larger than this value will get canceled
         +'"MaxInputLength": 0'
      +'}';

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TSynopseSimpleServer ------------------
// ------------------

constructor TSynopseSimpleServer.Create(const basePath : TFileName; options : TdwsJSONValue);
var
   logPath : TdwsJSONValue;
   serverOptions : TdwsJSONValue;
begin
   FPath:=IncludeTrailingPathDelimiter(ExpandFileName(basePath));

   FFileSystem:=TdwsRestrictedFileSystem.Create(nil);
   FFileSystem.Paths.Add(FPath);

   serverOptions:=TdwsJSONValue.ParseString(cDefaultServerOptions);
   try
      serverOptions.Extend(options['Server']);

      FServer:=THttpApi2Server.Create(False);
      FPort:=serverOptions['Port'].AsInteger;
      if FPort<>0 then
         FServer.AddUrl('', FPort, False, '+');
      FSSLPort:=serverOptions['SSLPort'].AsInteger;
      if FSSLPort<>0 then begin
         FServer.AddUrl('', FSSLPort, True, '+');
      end;
      FServer.RegisterCompress(CompressDeflate);
      FServer.OnRequest:=Process;

      logPath:=serverOptions['LogDirectory'];
      if (logPath.ValueType=jvtString) and (logPath.AsString<>'') then begin
         FServer.LogDirectory:=IncludeTrailingPathDelimiter(logPath.AsString);
         FServer.LogRolloverSize:=1024*1024;
         FServer.Logging:=True;
      end;

      FServer.MaxBandwidth:=serverOptions['MaxBandwidth'].AsInteger;

      FServer.MaxInputCountLength:=serverOptions['MaxInputLength'].AsInteger;
   finally
      serverOptions.Free;
   end;

   FDWS:=TSynDWScript.Create(nil);
   FDWS.FileSystem:=FFileSystem;
   FDWS.LoadCPUOptions(options['CPU']);
   FDWS.LoadDWScriptOptions(options['DWScript']);

   FNotifier:=TdwsDirectoryNotifier.Create(FPath, dnoDirectoryAndSubTree);
   FNotifier.OnDirectoryChanged:=DirectoryChanged;

   FServer.Clone(7);
end;

destructor TSynopseSimpleServer.Destroy;
begin
   FNotifier.Free;
   FServer.Free;
   FDWS.Free;
   FFileSystem.Free;
   inherited;
end;

{$WARN SYMBOL_PLATFORM OFF}

function TSynopseSimpleServer.Process(
      const inRequest : TSynHttpServerRequest;
      var outResponse : TSynHttpServerResponse) : cardinal;
var
   pathFileName : String;
   rawUrl : RawUTF8;
   params : String;
   p : Integer;
   request : TSynopseWebRequest;
   response : TSynopseWebResponse;
   fileAttribs : Cardinal;
begin
   rawUrl:=StringReplaceChars(UrlDecode(copy(inRequest.InURL,2,maxInt)), '/', '\');
   while (rawUrl<>'') and (rawUrl[1]='\') do
      delete(rawUrl,1,1);
   while (rawUrl<>'') and (rawUrl[length(rawUrl)]='\') do
      delete(rawUrl,length(rawUrl),1);
   pathFileName:=FPath+UTF8ToString(rawUrl);

   p:=Pos('?', pathFileName);
   if p>0 then begin
      params:=Copy(pathFileName, p+1);
      SetLength(pathFileName, p-1);
   end else params:='';

   pathFileName:=ExpandFileName(pathFileName);

   if Pos('\.', pathFileName)>0 then

      // Directories or files beginning with a '.' are invisible
      fileAttribs:=INVALID_FILE_ATTRIBUTES

   else if not StrBeginsWith(pathFileName, FPath) then begin

      // request is outside base path
      outResponse.OutContent:='<h1>Not authorized</h1>';
      outResponse.OutContentType:=HTML_CONTENT_TYPE;
      Result:=401;
      Exit;

   end else begin

      fileAttribs:=GetFileAttributes(Pointer(pathFileName));
      if fileAttribs<>INVALID_FILE_ATTRIBUTES then begin
         if (fileAttribs and faHidden)<>0 then
            fileAttribs:=INVALID_FILE_ATTRIBUTES
         else if (fileAttribs and faDirectory)<>0 then begin
            if not FindDirectoryIndex(pathFileName) then
               fileAttribs:=INVALID_FILE_ATTRIBUTES;
         end;
      end;

   end;

   if fileAttribs=INVALID_FILE_ATTRIBUTES then begin

      outResponse.OutContent:='<h1>Not found</h1>';
      outResponse.OutContentType:=HTML_CONTENT_TYPE;
      Result:=404;
      Exit;

   end else if StrEndsWith(pathFileName, '.dws') then begin

      request:=TSynopseWebRequest.Create;
      response:=TSynopseWebResponse.Create;
      try
         request.InURL:=inRequest.InURL;
         request.InMethod:=inRequest.InMethod;
         request.InHeaders:=inRequest.InHeaders;
         request.InContent:=inRequest.InContent;
         request.InContentType:=inRequest.InContentType;

         response.StatusCode:=200;
         response.ContentType:=HTML_CONTENT_TYPE;

         FDWS.HandleDWS(pathFileName, request, response);

         outResponse.OutContent:=response.ContentData;
         outResponse.OutContentType:=response.ContentType;
         if response.HasHeaders then
            outResponse.OutCustomHeader:=outResponse.OutCustomHeader+response.CompiledHeaders;

         Result:=response.StatusCode;
      finally
         request.Free;
         response.Free;
      end;

   end else begin

      // http.sys will send the specified file from kernel mode
      outResponse.OutContent:=StringToUTF8(pathFileName);
      outResponse.OutContentType:=HTTP_RESP_STATICFILE;
      Result:=200; // THttpApiServer.Execute will return 404 if not found

   end;
end;

// FindDirectoryIndex
//
function TSynopseSimpleServer.FindDirectoryIndex(var pathFileName : String) : Boolean;
begin
   Result:=True;
   if FileExists(pathFileName+'\index.dws') then
      pathFileName:=pathFileName+'\index.dws'
   else if FileExists(pathFileName+'\index.htm') then
      pathFileName:=pathFileName+'\index.htm'
   else if FileExists(pathFileName+'\index.html') then
      pathFileName:=pathFileName+'\index.html'
   else Result:=False;
end;

// DirectoryChanged
//
procedure TSynopseSimpleServer.DirectoryChanged(sender : TdwsDirectoryNotifier);
begin
   FDWS.FlushDWSCache;
end;

end.
