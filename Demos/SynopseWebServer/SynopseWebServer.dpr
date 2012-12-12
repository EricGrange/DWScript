{
    Will serve static content and DWS dynamic content via http.sys
    kernel mode high-performance HTTP server (available since XP SP2).
    See http://blog.synopse.info/post/2011/03/11/HTTP-server-using-fast-http.sys-kernel-mode-server
    WARNING: you need to first register the server URI and port to the http.sys stack.
    That is, run the application at least once as administrator.

    Sample based on official mORMot's sample
    "SQLite3\Samples\09 - HttpApi web server\HttpApiServer.dpr"

    Synopse mORMot framework. Copyright (C) 2012 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

    Original tri-license: MPL 1.1/GPL 2.0/LGPL 2.1

    You would need at least the following files from mORMot framework
    to be available in your project path:
    - SynCommons.pas
    - Synopse.inc
    - SynLZ.pas
    - SynZip.pas
    - SynCrtSock.pas
    - SynWinWock.pas
    http://synopse.info/fossil/wiki?name=Downloads

}
program SynopseWebServer;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  SynCommons,
  SynZip,
  SynCrtSock,
  DSimpleDWScript,
  dwsUtils,
  dwsWebEnvironment,
  dwsSynopseWebEnv,
  dwsFileSystem,
  dwsDirectoryNotifier;

type

   TTestServer = class
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

         function Process(const InURL, InMethod, InHeaders, InContent, InContentType: RawByteString;
                          out OutContent, OutContentType, OutCustomHeader: RawByteString) : cardinal;

         function DirectoryListing(FN : RawByteString; const fileName : TFileName) : RawByteString;
  end;

{ TTestServer }

constructor TTestServer.Create(const basePath : TFileName);
begin
   FPath:=IncludeTrailingPathDelimiter(ExpandFileName(basePath));

   FFileSystem:=TdwsRestrictedFileSystem.Create(nil);
   FFileSystem.Paths.Add(FPath);

   FDWS:=TSynDWScript.Create(nil);
   FDWS.FileSystem:=FFileSystem;

   FServer:=THttpApiServer.Create(false);
   FServer.AddUrl('', '888', false,'+');
   // FServer.RegisterCompress(CompressDeflate); // our server will deflate html :)
   FServer.OnRequest:=Process;

   FNotifier:=TdwsDirectoryNotifier.Create(FPath, dnoDirectoryAndSubTree);
   FNotifier.OnDirectoryChanged:=DirectoryChanged;
end;

destructor TTestServer.Destroy;
begin
   FNotifier.Free;
   FServer.Free;
   FDWS.Free;
   FFileSystem.Free;
   inherited;
end;

{$WARN SYMBOL_PLATFORM OFF}

function TTestServer.Process(
      const InURL, InMethod, InHeaders, InContent, InContentType: RawByteString;
      out OutContent, OutContentType, OutCustomHeader: RawByteString): cardinal;
var
   pathFileName : TFileName;
   rawUrl : RawUTF8;
   params : String;
   p : Integer;
   request : TSynopseWebRequest;
   response : TSynopseWebResponse;
begin
   rawUrl:=StringReplaceChars(UrlDecode(copy(InURL,2,maxInt)), '/', '\');
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
      OutContent:='Not authorized';
      OutContentType:=TEXT_CONTENT_TYPE;
      Result:=401;

   end else if DirectoryExists(pathFileName) then begin

      OutContent:=DirectoryListing(rawURL, pathFileName);
      OutContentType:=HTML_CONTENT_TYPE;
      Result:=200;

   end else if ExtractFileExt(pathFileName)='.dws' then begin

      request:=TSynopseWebRequest.Create;
      response:=TSynopseWebResponse.Create;
      try
         request.InURL:=InURL;
         request.InMethod:=InMethod;
         request.InHeaders:=InHeaders;
         request.InContent:=InContent;
         request.InContentType:=InContentType;

         response.StatusCode:=200;
         response.ContentType:=HTML_CONTENT_TYPE;

         FDWS.HandleDWS(pathFileName, request, response);

         OutContent:=response.ContentData;
         OutContentType:=response.ContentType;
         Result:=response.StatusCode;
      finally
         request.Free;
         response.Free;
      end;

   end else begin
      // http.sys will send the specified file from kernel mode
      OutContent:=StringToUTF8(pathFileName);
      OutContentType:=HTTP_RESP_STATICFILE;
      Result:=200; // THttpApiServer.Execute will return 404 if not found
   end;
end;

// DirectoryListing
//
function TTestServer.DirectoryListing(FN : RawByteString; const fileName : TFileName) : RawByteString;
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
procedure TTestServer.DirectoryChanged(sender : TdwsDirectoryNotifier);
begin
   FDWS.FlushDWSCache;
end;

var
   basePath : String;
begin
   basePath:=ExtractFilePath(ParamStr(0));
   if DirectoryExists(basePath+'www') then
      basePath:=basePath+'www' // subfolder 'ww' of where the exe is placed
   else if FileExists(ChangeFileExt(ParamStr(0), '.dpr')) then
      basePath:=basePath+'..\Data\www' // if compiled alongside dpr
   else basePath:=basePath+'..\..\..\Data\www'; // assume compiled in platform/target
   with TTestServer.Create(basePath) do try
      write('Server is now running on http://localhost:888/'#13#10#13#10+
            'Press [Enter] to quit');
      readln;
   finally
      Free;
  end;
end.
