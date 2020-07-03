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
unit dwsWebServerHelpers;

{$I dws.inc}

interface

uses
   Windows, Classes, SysUtils, Registry,
   dwsUtils, dwsXPlatform, dwsXXHash;

type
   TDirectoryIndexInfo = class(TRefCountedObject)
      private
         FIndexFileName : String;

      public
         property IndexFileName : String read FIndexFileName write FIndexFileName;
   end;

   TDirectoryIndexCache = class
      private
         FLock : TMultiReadSingleWrite;
         FHash : TNameObjectHash;
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

   TFileAccessType = (fatRAW, fatDWS, fatPAS, fatP2JS);

   TFileAccessInfo = class(TRefCountedObject)
      public
         CookedPathName : String;
         DefaultMimeType : RawByteString;
         FileAttribs : Cardinal;
         Typ : TFileAccessType;
         NextCheck : Int64;
   end;

   // this class is not thread safe, use from a single thread
   TFileAccessInfoCache = class
      private
         FHash : TSimpleNameObjectHash<TFileAccessInfo>;
         FMaxSize, FSize : Integer;

      public
         constructor Create(const aMaxSize : Integer);
         destructor Destroy; override;

         function FileAccessInfo(const pathInfo : String) : TFileAccessInfo; inline;
         function CreateFileAccessInfo(const pathInfo : String) : TFileAccessInfo;
         function Count : Integer; inline;

         procedure Flush;
   end;

   TMIMETypeInfo = class (TRefCountedObject)
      MIMEType : RawByteString;
      constructor CreateAuto(const ext : String);
   end;

   TMIMETypeInfos = TSimpleNameObjectHash<TMIMETypeInfo>;

   TMIMETypeCache = class
      private
         FList : TMIMETypeInfos;
         FLock : TMultiReadSingleWrite;

         procedure Prime(const ext : String; const mimeType : RawByteString);

      public
         constructor Create;
         destructor Destroy; override;

         function MIMEType(const fileName : String) : RawByteString;
   end;

   TFastCompareStringList = class (TStringList)
      function CompareStrings(const S1, S2: UnicodeString): Integer; override;
      function IndexOfName(const name : UnicodeString): Integer; override;
   end;

// Decodes an http request URL and splits path & params
// Skips initial '/'
// Normalizes '/' to '\' for the pathInfo
procedure HttpRequestUrlDecode(const s : RawByteString; var pathInfo, params : String);

function MIMETypeCache : TMIMETypeCache;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cUnknownMIMEType = 'application/unknown';

var
   vMIMETypeCache : TMIMETypeCache;
function MIMETypeCache : TMIMETypeCache;
begin
   Result := vMIMETypeCache;
end;

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
   FLock := TMultiReadSingleWrite.Create;
   FHash := TNameObjectHash.Create;
   FIndexFileNames := TStringList.Create;
end;

// Destroy
//
destructor TDirectoryIndexCache.Destroy;
begin
   inherited;
   FHash.Clean;
   FHash.Free;
   FLock.Free;
   FIndexFileNames.Free;
end;

// IndexFileForDirectory
//
function TDirectoryIndexCache.IndexFileForDirectory(var path : String) : Boolean;
var
   h : Cardinal;
   indexInfo : TDirectoryIndexInfo;
begin
   if not StrEndsWith(path, PathDelim) then
      path := path + PathDelim;

   h := FHash.HashName(path);

   FLock.BeginRead;
   try
      indexInfo := TDirectoryIndexInfo(FHash.HashedObjects[path, h]);
      if indexInfo <> nil then begin
         if indexInfo.IndexFileName <> '' then begin
            path := indexInfo.IndexFileName;
            Exit(True);
         end;
      end;
   finally
      FLock.EndRead;
   end;

   FLock.BeginWrite;
   try
      indexInfo := TDirectoryIndexInfo(FHash.HashedObjects[path, h]);
      if indexInfo = nil then begin
         indexInfo := CreateIndexInfo(path);
         FHash.HashedObjects[path, h] := indexInfo;
      end;
      if indexInfo.IndexFileName <> '' then begin
         path := indexInfo.IndexFileName;
         Result := True;
      end else Result := False;
   finally
      FLock.EndWrite;
   end;
end;

// Flush
//
procedure TDirectoryIndexCache.Flush;
begin
   FLock.BeginWrite;
   try
      FHash.Clean;
   finally
      FLock.EndWrite;
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
   FHash.Clean;
   FHash.Free;
   inherited;
end;

// FileAccessInfo
//
function TFileAccessInfoCache.FileAccessInfo(const pathInfo : String) : TFileAccessInfo;
begin
   Result := FHash.Objects[pathInfo];
end;

// CreateFileAccessInfo
//
function TFileAccessInfoCache.CreateFileAccessInfo(const pathInfo : String) : TFileAccessInfo;
begin
   if FSize = FMaxSize then
      Flush;
   Result := TFileAccessInfo.Create;
   Result.CookedPathName := pathInfo;
   FHash.AddObject(pathInfo, Result);
   Inc(FSize);
end;

// Count
//
function TFileAccessInfoCache.Count : Integer;
begin
   Result := FHash.Count;
end;

// Flush
//
procedure TFileAccessInfoCache.Flush;
begin
   FHash.Clean;
   FHash.Free;
   FHash:=TSimpleNameObjectHash<TFileAccessInfo>.Create;
   FSize:=0;
end;

// ------------------
// ------------------ TMIMETypeInfo ------------------
// ------------------

// CreateAuto
//
constructor TMIMETypeInfo.CreateAuto(const ext : String);
var
   reg : TRegistry;
begin
   reg:=TRegistry.Create;
   try
      reg.RootKey:=HKEY_CLASSES_ROOT;
      if     reg.OpenKeyReadOnly(ext)
         and reg.ValueExists('Content Type') then
         MIMEType:=ScriptStringToRawByteString(reg.ReadString('Content Type'));
      if MIMEType = '' then
         MIMEType := cUnknownMIMEType;
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
   FList := TMIMETypeInfos.Create;

   // prime the cache with common extensions

   Prime('.txt', 'text/plain');
   Prime('.htm', 'text/html');
   Prime('.html', 'text/html');
   Prime('.js',  'text/javascript');
   Prime('.css', 'text/css');
   Prime('.dws', 'text/html');

   Prime('.png', 'image/png');
   Prime('.jpg', 'image/jpeg');
   Prime('.gif', 'image/gif');
   Prime('.svg', 'image/svg+xml');

   Prime('.pdf', 'application/pdf');
   Prime('.xml', 'application/xml');
   Prime('.zip', 'application/zip');

   FLock := TMultiReadSingleWrite.Create;
end;

// Destroy
//
destructor TMIMETypeCache.Destroy;
begin
   inherited;
   FList.Clean;
   FList.Free;
   FLock.Free;
end;

// MIMEType
//
function TMIMETypeCache.MIMEType(const fileName : String) : RawByteString;

   function NewFileInfo(const ext : String) : TMIMETypeInfo;
   begin
      Result := TMIMETypeInfo.CreateAuto(ext);
      FLock.BeginWrite;
      FList.Objects[ext] := Result;
      FLock.EndWrite;
   end;

var
   ext : String;
   info : TMIMETypeInfo;
   i : Integer;
begin
   for i := Length(fileName) downto 1 do begin
      case fileName[i] of
         '.' : begin
            ext := Copy(fileName, i);
            break;
         end;
         PathDelim, DriveDelim :
            break;
      end;
   end;
   if ext = '' then Exit('');

   FLock.BeginRead;
   info := FList.Objects[ext];
   FLock.EndRead;

   if info = nil then
      info := NewFileInfo(ext);

   Result := info.MIMEType;
end;

// Prime
//
procedure TMIMETypeCache.Prime(const ext : String; const mimeType : RawByteString);
var
   info : TMIMETypeInfo;
begin
   info := TMIMETypeInfo.Create;
   info.MIMEType := mimeType;
   FList.Objects[ext] := info;
end;

// ------------------
// ------------------ TFastCompareStringList ------------------
// ------------------

type
   {$IF CompilerVersion > 22}
   TStringListList = TStringItemList;
   {$ELSE}
   TStringListList = PStringItemList;
   {$IFEND}

   TStringListCracker = class (TStrings)
      private
         FList : TStringListList;
   end;

// CompareStrings
//
function TFastCompareStringList.CompareStrings(const S1, S2: UnicodeString): Integer;
begin
   Result:=CompareStr(S1, S2);
end;

// IndexOfName
//
function TFastCompareStringList.IndexOfName(const name : UnicodeString): Integer;
var
   n, nc : Integer;
   nvs : WideChar;
   list : TStringListList;
begin
   nvs := NameValueSeparator;
   n := Length(name);
   list := TStringListCracker(Self).FList;
   for Result := 0 to Count-1 do begin
      nc := Length(list[Result].FString);
      if     (nc > n) and (list[Result].FString[n+1] = nvs)
         and CompareMem(PWideChar(Pointer(name)),
                        PWideChar(Pointer(list[Result].FString)), n*SizeOf(Char)) then Exit;
   end;
   Result:=-1;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   vMIMETypeCache := TMIMETypeCache.Create;

finalization

   FreeAndNil(vMIMETypeCache);

end.
