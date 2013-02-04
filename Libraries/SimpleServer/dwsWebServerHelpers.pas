// dwsWebServerHelpers
{: egg<p>

   Helper class and utilities for web servers

   <b>Historique : </b><font size=-1><ul>
      <li>31/12/12 - egg - Creation
   </ul></font>
}
unit dwsWebServerHelpers;

interface

uses
   Windows, Classes, SysUtils, Registry,
   dwsUtils;

type
   TDirectoryIndexInfo = class(TRefCountedObject)
      private
         FIndexFileName : String;

      public
         property IndexFileName : String read FIndexFileName write FIndexFileName;
   end;

   TDirectoryIndexCache = class
      private
         FLock : TFixedCriticalSection;
         FHash : TSimpleNameObjectHash<TDirectoryIndexInfo>;
         FIndexFileNames : TStrings;

      protected
         function CreateIndexInfo(const directory : String) : TDirectoryIndexInfo;

      public
         constructor Create;
         destructor Destroy; override;

         function IndexFileForDirectory(var path : String) : Boolean;

         procedure Flush;

         property IndexFileNames : TStrings read FIndexFileNames;
   end;

   TFileAccessInfo = class(TRefCountedObject)
      public
         CookedPathName : String;
         FileAttribs : Cardinal;
         DWScript : Boolean;
   end;

   // this class is not thread safe, use from a single thread
   TFileAccessInfoCache = class
      private
         FHash : TSimpleNameObjectHash<TFileAccessInfo>;
         FMaxSize, FSize : Integer;
         FCacheCounter : Cardinal;

      public
         constructor Create(const aMaxSize : Integer);
         destructor Destroy; override;

         function FileAccessInfo(const pathInfo : String) : TFileAccessInfo; inline;
         function CreateFileAccessInfo(const pathInfo : String) : TFileAccessInfo;

         procedure Flush;

         property CacheCounter : Cardinal read FCacheCounter write FCacheCounter;
   end;

   TMIMETypeInfo = class (TRefCountedObject)
      MIMEType : RawByteString;
      constructor Create(const ext : String);
   end;

   TMIMETypeInfos = TSimpleNameObjectHash<TMIMETypeInfo>;

   TMIMETypeCache = class
      private
         FList : TMIMETypeInfos;

      public
         constructor Create;
         destructor Destroy; override;

         function MIMEType(const fileName : String) : RawByteString;
   end;

// Decodes an http request URL and splits path & params
// Skips initial '/'
// Normalizes '/' to '\' for the pathInfo
procedure HttpRequestUrlDecode(const s : RawByteString; var pathInfo, params : String);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

procedure HttpRequestUrlDecode(const s : RawByteString; var pathInfo, params : String);
var
   n, c : Integer;
   decodedBuffer : UTF8String;
   pIn : PAnsiChar;
   pOut : PAnsiChar;
   paramsOffset : PAnsiChar;
begin
   n:=Length(s);
   if n=0 then begin
      pathInfo:='';
      params:='';
      Exit;
   end;
   SetLength(decodedBuffer, n);

   c:=0; // workaround for spurious compiler warning
   paramsOffset:=nil;
   pIn:=Pointer(s);
   if pIn^='/' then
      Inc(pIn);
   pOut:=Pointer(decodedBuffer);
   while True do begin
      case pIn^ of
         #0 : break;
         '%' : begin
            Inc(pIn);
            case pIn^ of
               '0'..'9' : c:=Ord(pIn^)-Ord('0');
               'a'..'f' : c:=Ord(pIn^)+(10-Ord('a'));
               'A'..'F' : c:=Ord(pIn^)+(10-Ord('A'));
            else
               break;  // invalid url
            end;
            Inc(pIn);
            case pIn^ of
               '0'..'9' : c:=(c shl 4)+Ord(pIn^)-Ord('0');
               'a'..'f' : c:=(c shl 4)+Ord(pIn^)+(10-Ord('a'));
               'A'..'F' : c:=(c shl 4)+Ord(pIn^)+(10-Ord('A'));
            else
               break;  // invalid url
            end;
            pOut^:=AnsiChar(c);
         end;
         '+' : pOut^:=' ';
         '?' : begin
            pOut^:='?';
            if paramsOffset=nil then
               paramsOffset:=pOut;
         end;
         '/' : begin
            if paramsOffset=nil then
               pOut^:='\'
            else pOut^:='/';
         end;
      else
         pOut^:=pIn^;
      end;
      Inc(pIn);
      Inc(pOut);
   end;

   if paramsOffset=nil then begin

      params:='';
      n:=UInt64(pOut)-UInt64(Pointer(decodedBuffer));
      SetLength(pathInfo, n);
      n:=MultiByteToWideChar(CP_UTF8, 0, Pointer(decodedBuffer), n, Pointer(pathInfo), n);
      SetLength(pathInfo, n);

   end else begin

      n:=UInt64(paramsOffset)-UInt64(Pointer(decodedBuffer));
      SetLength(pathInfo, n);
      n:=MultiByteToWideChar(CP_UTF8, 0, Pointer(decodedBuffer), n, Pointer(pathInfo), n);
      SetLength(pathInfo, n);

      n:=UInt64(pOut)-UInt64(paramsOffset);
      SetLength(params, n);
      n:=MultiByteToWideChar(CP_UTF8, 0, Pointer(paramsOffset), n, Pointer(params), n);
      SetLength(params, n);

   end;
end;

// ------------------
// ------------------ TDirectoryIndexCache ------------------
// ------------------

// Create
//
constructor TDirectoryIndexCache.Create;
begin
   inherited;
   FLock:=TFixedCriticalSection.Create;
   FHash:=TSimpleNameObjectHash<TDirectoryIndexInfo>.Create;
   FIndexFileNames:=TStringList.Create;
end;

// Destroy
//
destructor TDirectoryIndexCache.Destroy;
begin
   inherited;
   FHash.Free;
   FLock.Free;
   FIndexFileNames.Free;
end;

// IndexFileForDirectory
//
function TDirectoryIndexCache.IndexFileForDirectory(var path : String) : Boolean;
var
   indexInfo : TDirectoryIndexInfo;
begin
   if not StrEndsWith(path, PathDelim) then
      path:=path+PathDelim;

   FLock.Enter;
   try
      indexInfo:=FHash.Objects[path];
      if indexInfo=nil then begin
         indexInfo:=CreateIndexInfo(path);
         FHash.Objects[path]:=indexInfo;
      end;
      if indexInfo.IndexFileName<>'' then begin
         path:=indexInfo.IndexFileName;
         Result:=True;
      end else Result:=False;
   finally
      FLock.Leave;
   end;
end;

// Flush
//
procedure TDirectoryIndexCache.Flush;
begin
   FLock.Enter;
   try
      FHash.Free;
      FHash:=TSimpleNameObjectHash<TDirectoryIndexInfo>.Create;
   finally
      FLock.Leave;
   end;
end;

// CreateIndexInfo
//
function TDirectoryIndexCache.CreateIndexInfo(const directory : String) : TDirectoryIndexInfo;
var
   i : Integer;
   path, fileName : String;
begin
   Result:=TDirectoryIndexInfo.Create;

   path:=IncludeTrailingPathDelimiter(directory);

   for i:=0 to IndexFileNames.Count-1 do begin
      fileName:=path+IndexFileNames[i];
      if FileExists(fileName) then begin
         Result.IndexFileName:=fileName;
         Break;
      end;
   end;
end;

// ------------------
// ------------------ TFileAccessInfoCache ------------------
// ------------------

// Create
//
constructor TFileAccessInfoCache.Create(const aMaxSize : Integer);
begin
   inherited Create;
   FHash:=TSimpleNameObjectHash<TFileAccessInfo>.Create;
   FMaxSize:=aMaxSize;
end;

// Destroy
//
destructor TFileAccessInfoCache.Destroy;
begin
   FHash.Free;
   inherited;
end;

// FileAccessInfo
//
function TFileAccessInfoCache.FileAccessInfo(const pathInfo : String) : TFileAccessInfo;
begin
   Result:=FHash.Objects[pathInfo];
end;

// CreateFileAccessInfo
//
function TFileAccessInfoCache.CreateFileAccessInfo(const pathInfo : String) : TFileAccessInfo;
begin
   if FSize=FMaxSize then
      Flush;
   Result:=TFileAccessInfo.Create;
   Result.CookedPathName:=pathInfo;
   FHash.AddObject(pathInfo, Result);
   Inc(FSize);
end;

// Flush
//
procedure TFileAccessInfoCache.Flush;
begin
   FHash.Free;
   FHash:=TSimpleNameObjectHash<TFileAccessInfo>.Create;
   FSize:=0;
end;

// ------------------
// ------------------ TMIMETypeInfo ------------------
// ------------------

// Create
//
constructor TMIMETypeInfo.Create(const ext : String);
var
   reg : TRegistry;
begin
   reg:=TRegistry.Create;
   try
      reg.RootKey:=HKEY_CLASSES_ROOT;
      if     reg.OpenKeyReadOnly(ext)
         and reg.ValueExists('Content Type') then
         MIMEType:=ScriptStringToRawByteString(reg.ReadString('Content Type'));
   finally
      reg.Free;
   end;
end;

// ------------------
// ------------------ TMIMETypeCache ------------------
// ------------------

// Create
//
constructor TMIMETypeCache.Create;
begin
   inherited;
   FList:=TMIMETypeInfos.Create;
end;

// Destroy
//
destructor TMIMETypeCache.Destroy;
begin
   inherited;
   FList.Free;
end;

// MIMEType
//
function TMIMETypeCache.MIMEType(const fileName : String) : RawByteString;
var
   ext : String;
   info : TMIMETypeInfo;
begin
   ext:=ExtractFileExt(fileName);
   info:=FList.Objects[ext];
   if info=nil then begin
      info:=TMIMETypeInfo.Create(ext);
      FList.Objects[ext]:=info;
   end;
   Result:=info.MIMEType;
end;

end.
