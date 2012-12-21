unit USynopseSimpleWebServer;

interface

uses
  SysUtils,
  SynCommons, SynZip, SynCrtSock,
  dwsUtils, dwsWebEnvironment, dwsSynopseWebEnv, dwsFileSystem, dwsDirectoryNotifier,
  DSimpleDWScript;

type

   TSynopseSimpleServer = class
      protected
         FPath : TFileName;
         FServer : THttpApiServer;
         FFileSystem : TdwsRestrictedFileSystem;
         FDWS : TSynDWScript;
         FNotifier : TdwsDirectoryNotifier;

         procedure DirectoryChanged(sender : TdwsDirectoryNotifier);

      public
         constructor Create(const basePath : TFileName);
         destructor Destroy; override;

         function Process(const inRequest : TSynHttpServerRequest;
                          var outResponse : TSynHttpServerResponse) : cardinal;

         function DirectoryListing(FN : RawByteString; const fileName : TFileName) : RawByteString;
  end;

implementation

{ TSynopseSimpleServer }

constructor TSynopseSimpleServer.Create(const basePath : TFileName);
begin
   FPath:=IncludeTrailingPathDelimiter(ExpandFileName(basePath));

   FFileSystem:=TdwsRestrictedFileSystem.Create(nil);
   FFileSystem.Paths.Add(FPath);

   FDWS:=TSynDWScript.Create(nil);
   FDWS.FileSystem:=FFileSystem;
   FDWS.CPUUsageLimit:=10;

   FServer:=THttpApiServer.Create(false);
   FServer.AddUrl('', '888', false,'+');
   FServer.RegisterCompress(CompressDeflate); // our server will deflate html :)
   FServer.OnRequest:=Process;

   FNotifier:=TdwsDirectoryNotifier.Create(FPath, dnoDirectoryAndSubTree);
   FNotifier.OnDirectoryChanged:=DirectoryChanged;

   FServer.Clone(8);
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
   pathFileName : TFileName;
   rawUrl : RawUTF8;
   params : String;
   p : Integer;
   request : TSynopseWebRequest;
   response : TSynopseWebResponse;
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

   if not StrBeginsWith(pathFileName, FPath) then begin

      // request is outside base path
      outResponse.OutContent:='Not authorized';
      outResponse.OutContentType:=TEXT_CONTENT_TYPE;
      Result:=401;

   end else if DirectoryExists(pathFileName) then begin

      outResponse.OutContent:=DirectoryListing(rawURL, pathFileName);
      outResponse.OutContentType:=HTML_CONTENT_TYPE;
      Result:=200;

   end else if ExtractFileExt(pathFileName)='.dws' then begin

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
         if response.AllowCORS<>'' then
            outResponse.OutCustomHeader:=outResponse.OutCustomHeader
               +'Access-Control-Allow-Origin: '+response.AllowCORS+#13#10;
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

// DirectoryListing
//
function TSynopseSimpleServer.DirectoryListing(FN : RawByteString; const fileName : TFileName) : RawByteString;
var
   W : TTextWriter;
   SRName, href: RawUTF8;
   i : integer;
   SR : TSearchRec;

   procedure hrefCompute;
   begin
      SRName := StringToUTF8(SR.Name);
      href := FN+StringReplaceChars(SRName,'\','/');
   end;

begin
   // reply directory listing as html
   W := TTextWriter.CreateOwnedStream;
   try
      W.Add( '<html><body style="font-family: Arial">'
            +'<h3>%</h3><p><table>',[FN]);
      FN := StringReplaceChars(FN,'\','/');
      if FN<>'' then
         FN := FN+'/';
      if FindFirst(FileName+'\*.*',faDirectory,SR)=0 then begin
         repeat
            if (SR.Attr and faDirectory<>0) and (SR.Name<>'.') then begin
               hrefCompute;
               if SRName='..' then begin
                  i := length(FN);
                  while (i>0) and (FN[i]='/') do dec(i);
                  while (i>0) and (FN[i]<>'/') do dec(i);
                  href := copy(FN,1,i);
               end;
               W.Add('<tr><td><b><a href="/%">[%]</a></b></td></tr>', [href, SRName]);
            end;
         until FindNext(SR)<>0;
         FindClose(SR);
      end;
      if FindFirst(FileName+'\*.*',faAnyFile-faDirectory-faHidden,SR)=0 then begin
         repeat
            hrefCompute;
            if SR.Attr and faDirectory=0 then
               W.Add('<tr><td><b><a href="/%">%</a></b></td><td>%</td><td>%</td></td></tr>',
                     [href, SRName,KB(SR.Size), DateTimeToStr(SR.TimeStamp)]);
         until FindNext(SR)<>0;
         FindClose(SR);
      end;
      W.AddString('</table></p><p><i>Powered by <strong>THttpApiServer</strong></i> - '+
                  'see <a href=http://synopse.info>http://synopse.info</a></p></body></html>');
      Result:=W.Text;
   finally
      W.Free;
   end;
end;

// DirectoryChanged
//
procedure TSynopseSimpleServer.DirectoryChanged(sender : TdwsDirectoryNotifier);
begin
   FDWS.FlushDWSCache;
end;

end.
