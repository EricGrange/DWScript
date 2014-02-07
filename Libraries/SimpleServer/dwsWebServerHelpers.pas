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
   dwsUtils, dwsXPlatform;

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
      constructor CreateAuto(const ext : String);
   end;

   TMIMETypeInfos = TSimpleNameObjectHash<TMIMETypeInfo>;

   TMIMETypeCache = class
      private
         FList : TMIMETypeInfos;

         procedure Prime(const ext : String; const mimeType : RawByteString);

      public
         constructor Create;
         destructor Destroy; override;

         function MIMEType(const fileName : String) : RawByteString;
   end;

// Decodes an http request URL and splits path & params
// Skips initial '/'
// Normalizes '/' to '\' for the pathInfo
procedure HttpRequestUrlDecode(const s : RawByteString; var pathInfo, params : String);

function DateTimeToRFC822(const dt : TDateTime) : String;
function RFC822ToDateTime(const str : String) : TDateTime;

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

const
   cRFC822Months : array[1..12] of String = (
      'Jan','Feb','Mar','Apr', 'May','Jun','Jul','Aug', 'Sep','Oct','Nov','Dec'
   );
   cRFC822Days : array[1..7] of String = (
      'Sun','Mon','Tue','Wed','Thu', 'Fri','Sat'
   );

// DateTimeToRFC822
//
function DateTimeToRFC822(const dt : TDateTime) : String;
var
   a, m, j, hh, mn, ss, ms : Word;
   dow : Integer;
begin
   DecodeDate(dt, a, m, j);
   DecodeTime(dt, hh, mn, ss, ms);
   dow:=DayOfWeek(dt);
   Result:=Format('%s, %.2d %s %.4d %.2d:%.2d:%.2d GMT',
                  [cRFC822Days[dow], j, cRFC822Months[m], a, hh, mn, ss]);
end;

// RFC822ToDateTime
//
function RFC822ToDateTime(const str : String) : TDateTime;
const
   cMaxItems = 6;
type
   TStringArray = array of String;
var
   list : array [0..cMaxItems+1] of String;
   count : Integer;
   y, mo, d : Word;
   h, mi, s : Word;
   deltaHours, deltaDays, p : Integer;
   deltaTime : TDateTime;

   procedure SplitStr(const str : String; const delim : Char; start : Integer);
   var
      lookup : integer;
   begin
      count:=0;
      if str='' then Exit;
      lookup:=start;
      while lookup<=Length(str) do begin
         if str[lookup]=delim then begin
            if lookup>start then begin
               list[count]:=Copy(str, start, lookup-start);
               Inc(count);
               if count>=cMaxItems then break;
            end;
            start:=lookup+1;
         end;
         Inc(lookup);
      end;
      if lookup>start then begin
         list[count]:=Copy(str, start, lookup-start);
         Inc(count);
      end;
   end;

   function ParseTwoDigits(p : PChar; offset : Integer) : Integer; inline;
   begin
      Result:=Ord(p[offset])*10+Ord(p[offset+1])-11*Ord('0')
   end;

   function ParseFourDigits(p : PChar; offset : Integer) : Integer; inline;
   begin
      Result:=ParseTwoDigits(p, 0)*100+ParseTwoDigits(p, 2);
   end;

   procedure ParseHMS(const str : String);
   var
      p : PChar;
   begin
      p:=PChar(Pointer(str));
      h:=65535;
      case Length(str) of
         5 : begin // hh:nn
            if p[2]<>':' then exit;
            h:=ParseTwoDigits(p, 0);
            mi:=ParseTwoDigits(p, 3);
            s:=0;
         end;
         8 : begin // hh:nn:ss
            if p[2]<>':' then exit;
            if p[5]<>':' then exit;
            h:=ParseTwoDigits(p, 0);
            mi:=ParseTwoDigits(p, 3);
            s:=ParseTwoDigits(p, 6);
         end;
      end;
   end;

   procedure ParseYear(const str : String);
   begin
      case Length(str) of
         2 : y:=ParseTwoDigits(Pointer(str), 0)+2000;
         4 : y:=ParseFourDigits(Pointer(str), 0);
      else
         y:=65535;
      end;
   end;

   procedure ParseMonth(const str : String);
   begin
      mo:=1;
      while (mo<=12) and not SameText(str, cRFC822Months[mo]) do
         Inc(mo);
   end;

begin
   Result:=0;
   if str='' then Exit;

   p:=Pos(',', str);
   if p>0 then
      SplitStr(str, ' ', p+1)
   else SplitStr(str, ' ', 1);
   if count<5 then // invalid date
      Exit;
   if (count>5) and (Pos(':', list[4])>0) and StrBeginsWith(list[5], 'GMT+') then begin
      // Thu Oct 08 2009 00:00:00 GMT+0200 (Romance Daylight Time)
      ParseMonth(list[1]);
      if Length(list[2])=2 then
         d:=ParseTwoDigits(Pointer(list[2]), 0)
      else d:=0;
      ParseYear(list[3]);
      ParseHMS(list[4]);
      deltaHours:=0;
      deltaDays:=0;
   end else begin
      // Thu, 08 Oct 2009 00:00:00 GMT
      if Length(list[0])=2 then
         d:=ParseTwoDigits(Pointer(list[0]), 0)
      else d:=0;
      ParseMonth(list[1]);
      ParseYear(list[2]);
      ParseHMS(list[3]);
      deltaHours:=StrToIntDef(list[4], 0);
      deltaDays:=0;
      while h>=24 do begin
         Dec(h, 24);
         Inc(deltaDays);
      end;
   end;
   if not TryEncodeDate(y, mo, d, Result) then
      Result:=0
   else if TryEncodeTime(h, mi, s, 0, deltaTime) then
      Result:=Result+deltaTime-deltaHours*(1/100/24)+deltaDays
   else Result:=0;
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
   FHash.Clean;
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
      FHash.Clean;
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
   FHash.Clean;
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
      if MIMEType='' then
         MIMEType:='application/unknown';
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

   // prime the cache with common extensions

   Prime('.txt', 'text/plain');
   Prime('.htm', 'text/html');
   Prime('.html', 'text/html');
   Prime('.js', 'text/javascript');
   Prime('.css', 'text/css');
   Prime('.png', 'image/png');
   Prime('.jpg', 'image/jpeg');
   Prime('.gif', 'image/gif');
end;

// Destroy
//
destructor TMIMETypeCache.Destroy;
begin
   inherited;
   FList.Clean;
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
      info:=TMIMETypeInfo.CreateAuto(ext);
      FList.Objects[ext]:=info;
   end;
   Result:=info.MIMEType;
end;

// Prime
//
procedure TMIMETypeCache.Prime(const ext : String; const mimeType : RawByteString);
var
   info : TMIMETypeInfo;
begin
   info:=TMIMETypeInfo.Create;
   info.MIMEType:=mimeType;
   FList.Objects[ext]:=info;
end;

end.
