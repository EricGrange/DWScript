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
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. For other initial contributors, see contributors.txt   }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsXPlatform;

{$I dws.inc}

//
// This unit should concentrate all non-UI cross-platform aspects,
// cross-Delphi versions, ifdefs and other conditionals
//
// no ifdefs in the main code.

{$WARN SYMBOL_PLATFORM OFF}

{$IFDEF FPC}
   {$DEFINE VER200}
{$ENDIF}

interface

uses Windows, Classes, SysUtils
   {$IFNDEF VER200}, IOUtils{$ENDIF}
   ;

const
{$IFDEF UNIX}
   cLineTerminator  = #10;
{$ELSE}
   cLineTerminator  = #13#10;
{$ENDIF}

procedure SetDecimalSeparator(c : Char);
function GetDecimalSeparator : Char;

procedure CollectFiles(const directory, fileMask : String; list : TStrings);

type
   {$IFNDEF FPC}
   {$IF CompilerVersion<22.0}
   // NativeUInt broken in D2009, and PNativeInt is missing in D2010
   // http://qc.embarcadero.com/wc/qcmain.aspx?d=71292
   NativeInt = Integer;
   PNativeInt = ^NativeInt;
   NativeUInt = Cardinal;
   PNativeUInt = ^NativeUInt;
   {$IFEND}
   {$ENDIF}

   {$IF compilerversion = 20}
   NativeInt = Integer; // D2009 workaround
   {$IFEND}

   {$IFDEF FPC}
   TBytes = array of Byte;

   RawByteString = String;

   PNativeInt = ^NativeInt;
   PUInt64 = ^UInt64;
   {$ENDIF}

   TPath = class
      class function GetTempFileName : String; static;
   end;

   TFile = class
      class function ReadAllBytes(const filename : String) : TBytes; static;
   end;

   TdwsThread = class (TThread)
      {$IFNDEF FPC}
      {$IFDEF VER200}
      procedure Start;
      {$ENDIF}
      {$ENDIF}
   end;

function GetSystemMilliseconds : Cardinal;
function UTCDateTime : TDateTime;

function AnsiCompareText(const S1, S2 : String) : Integer;
function AnsiCompareStr(const S1, S2 : String) : Integer;
function UnicodeComparePChars(p1 : PChar; n1 : Integer; p2 : PChar; n2 : Integer) : Integer; overload;
function UnicodeComparePChars(p1, p2 : PChar; n : Integer) : Integer; overload;

function InterlockedIncrement(var val : Integer) : Integer;
function InterlockedDecrement(var val : Integer) : Integer;

procedure SetThreadName(const threadName : PAnsiChar; threadID : Cardinal = Cardinal(-1));

procedure OutputDebugString(const msg : String);

procedure WriteToOSEventLog(const logName, logCaption, logDetails : String;
                            const logRawData : RawByteString = ''); overload;

function TryTextToFloat(const s : PChar; var value : Extended;
                        const formatSettings : TFormatSettings) : Boolean; {$ifndef FPC} inline; {$endif}

{$ifdef FPC}
procedure VarCopy(out dest : Variant; const src : Variant); inline;
{$endif}

function LoadTextFromBuffer(const buf : TBytes) : String;
function LoadTextFromStream(aStream : TStream) : String;
function LoadTextFromFile(const fileName : String) : String;
function OpenFileForSequentialReadOnly(const fileName : String) : THandle;
procedure CloseFileHandle(hFile : THandle);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// GetSystemMilliseconds
//
function GetSystemMilliseconds : Cardinal;
begin
   Result:=GetTickCount;
end;

// UTCDateTime
//
function UTCDateTime : TDateTime;
var
   systemTime : TSystemTime;
begin
   FillChar(systemTime, SizeOf(systemTime), 0);
   GetSystemTime(systemTime);
   with systemTime do
      Result:= EncodeDate(wYear, wMonth, wDay)
              +EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
end;

// AnsiCompareText
//
function AnsiCompareText(const S1, S2: String) : Integer;
begin
   Result:=SysUtils.AnsiCompareText(S1, S2);
end;

// AnsiCompareStr
//
function AnsiCompareStr(const S1, S2: String) : Integer;
begin
   Result:=SysUtils.AnsiCompareStr(S1, S2);
end;

// UnicodeComparePChars
//
function UnicodeComparePChars(p1 : PChar; n1 : Integer; p2 : PChar; n2 : Integer) : Integer;
const
   CSTR_EQUAL = 2;
begin
   {$ifdef FPC}
   Result:=CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE, p1, n1, p2, n2)-CSTR_EQUAL;
   {$else}
   Result:=CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, p1, n1, p2, n2)-CSTR_EQUAL;
   {$endif}
end;

// UnicodeComparePChars
//
function UnicodeComparePChars(p1, p2 : PChar; n : Integer) : Integer; overload;
const
   CSTR_EQUAL = 2;
begin
   {$ifdef FPC}
   Result:=CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE, p1, n, p2, n)-CSTR_EQUAL;
   {$else}
   Result:=CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, p1, n, p2, n)-CSTR_EQUAL;
   {$endif}
end;

// InterlockedIncrement
//
function InterlockedIncrement(var val : Integer) : Integer;
begin
   Result:=Windows.InterlockedIncrement(val);
end;

// InterlockedDecrement
//
function InterlockedDecrement(var val : Integer) : Integer;
begin
   Result:=Windows.InterlockedDecrement(val);
end;

// SetThreadName
//
function IsDebuggerPresent : BOOL; stdcall; external kernel32 name 'IsDebuggerPresent';
procedure SetThreadName(const threadName : PAnsiChar; threadID : Cardinal = Cardinal(-1));
// http://www.codeproject.com/Articles/8549/Name-your-threads-in-the-VC-debugger-thread-list
type
   TThreadNameInfo = record
      dwType : Cardinal;      // must be 0x1000
      szName : PAnsiChar;     // pointer to name (in user addr space)
      dwThreadID : Cardinal;  // thread ID (-1=caller thread)
      dwFlags : Cardinal;     // reserved for future use, must be zero
   end;
var
   info : TThreadNameInfo;
begin
   if not IsDebuggerPresent then Exit;

   info.dwType:=$1000;
   info.szName:=threadName;
   info.dwThreadID:=threadID;
   info.dwFlags:=0;
   {$ifndef FPC}
   try
      RaiseException($406D1388, 0, SizeOf(info) div SizeOf(Cardinal), @info);
   except
   end;
   {$endif}
end;

// OutputDebugString
//
procedure OutputDebugString(const msg : String);
begin
   Windows.OutputDebugString(PChar(msg));
end;

// WriteToOSEventLog
//
procedure WriteToOSEventLog(const logName, logCaption, logDetails : String;
                            const logRawData : RawByteString = '');
var
  eventSource : THandle;
  detailsPtr : array [0..1] of PChar;
begin
   if logName<>'' then
      eventSource:=RegisterEventSource(nil, PChar(logName))
   else eventSource:=RegisterEventSource(nil, PChar(ChangeFileExt(ExtractFileName(ParamStr(0)), '')));
   if eventSource>0 then begin
      try
         detailsPtr[0]:=PChar(logCaption);
         detailsPtr[1]:=PChar(logDetails);
         ReportEvent(eventSource, EVENTLOG_INFORMATION_TYPE, 0, 0, nil,
                     2, Length(logRawData),
                     @detailsPtr, Pointer(logRawData));
      finally
         DeregisterEventSource(eventSource);
      end;
   end;
end;

// SetDecimalSeparator
//
procedure SetDecimalSeparator(c : Char);
begin
   {$IFDEF FPC}
      FormatSettings.DecimalSeparator:=c;
   {$ELSE}
      {$IF CompilerVersion >= 22.0}
      FormatSettings.DecimalSeparator:=c;
      {$ELSE}
      DecimalSeparator:=c;
      {$IFEND}
   {$ENDIF}
end;

// GetDecimalSeparator
//
function GetDecimalSeparator : Char;
begin
   {$IFDEF FPC}
      Result:=FormatSettings.DecimalSeparator;
   {$ELSE}
      {$IF CompilerVersion >= 22.0}
      Result:=FormatSettings.DecimalSeparator;
      {$ELSE}
      Result:=DecimalSeparator;
      {$IFEND}
   {$ENDIF}
end;

// CollectFiles
//
procedure CollectFiles(const directory, fileMask : String; list : TStrings);
var
   searchRec : TSearchRec;
   found : Integer;
begin
   found:=FindFirst(directory+fileMask, faArchive or faReadOnly or faHidden, searchRec);
   while found=0 do begin
      if (searchRec.Attr and faDirectory)=0 then begin
         list.Add(directory+searchRec.Name);
      end;
      found:=FindNext(searchRec);
   end;
   FindClose(searchRec);
end;

{$ifdef FPC}
// VarCopy
//
procedure VarCopy(out dest : Variant; const src : Variant);
begin
   dest:=src;
end;
{$endif}

// TryTextToFloat
//
function TryTextToFloat(const s : PChar; var value : Extended; const formatSettings : TFormatSettings) : Boolean;
{$ifdef FPC}
var
   cw : Word;
begin
   cw:=Get8087CW;;
   Set8087CW($133F);
   if TryStrToFloat(s, value, formatSettings) then
      Result:=(value>-1.7e308) and (value<1.7e308);
   if not Result then
      value:=0;
   asm fclex end;
   Set8087CW(cw);
{$else}
begin
   Result:=TextToFloat(s, value, fvExtended, formatSettings)
{$endif}
end;

// LoadTextFromBuffer
//
function LoadTextFromBuffer(const buf : TBytes) : String;
var
   n : Integer;
   encoding : TEncoding;
begin
   encoding:=nil;
   n:=TEncoding.GetBufferEncoding(buf, encoding);
   if not Assigned(encoding) then
      encoding:=TEncoding.UTF8;
   Result:=encoding.GetString(buf, n, Length(buf)-n);
end;

// LoadTextFromStream
//
function LoadTextFromStream(aStream : TStream) : String;
var
   n : Integer;
   buf : TBytes;
begin
   n:=aStream.Size-aStream.Position;
   SetLength(buf, n);
   aStream.Read(buf[0], n);
   Result:=LoadTextFromBuffer(buf);
end;

// LoadTextFromFile
//
function LoadTextFromFile(const fileName : String) : String;
var
   hFile : THandle;
   n, nRead : Cardinal;
   buf : TBytes;
begin
   hFile:=OpenFileForSequentialReadOnly(fileName);
   if hFile=INVALID_HANDLE_VALUE then
      Exit('');
   try
      n:=GetFileSize(hFile, nil);
      if n=INVALID_FILE_SIZE then
         RaiseLastOSError;
      SetLength(buf, n);
      if not ReadFile(hFile, buf[0], n, nRead, nil) then
         RaiseLastOSError;
      Result:=LoadTextFromBuffer(buf);
   finally
      FileClose(hFile);
   end;
end;

// OpenFileForSequentialReadOnly
//
function OpenFileForSequentialReadOnly(const fileName : String) : THandle;
begin
   Result:=CreateFile(PChar(fileName), GENERIC_READ, FILE_SHARE_READ+FILE_SHARE_WRITE,
                      nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
   if Result=INVALID_HANDLE_VALUE then begin
      if GetLastError<>ERROR_FILE_NOT_FOUND then
         RaiseLastOSError;
      Exit;
   end;
end;

// CloseFileHandle
//
procedure CloseFileHandle(hFile : THandle);
begin
   CloseHandle(hFile);
end;

// ------------------
// ------------------ TPath ------------------
// ------------------

// GetTempFileName
//
class function TPath.GetTempFileName : String;
{$IFDEF VER200} // Delphi 2009
var
   tempPath, tempFileName : array [0..MAX_PATH] of WideChar; // Buf sizes are MAX_PATH+1
begin
   if Windows.GetTempPath(MAX_PATH, @tempPath[0])=0 then begin
      tempPath[1]:='.'; // Current directory
      tempPath[2]:=#0;
   end;
   if Windows.GetTempFileNameW(@tempPath[0], 'DWS', 0, tempFileName)=0 then
      RaiseLastOSError; // should never happen
   Result:=tempFileName;
{$ELSE}
begin
   Result:=IOUTils.TPath.GetTempFileName;
{$ENDIF}
end;

// ------------------
// ------------------ TFile ------------------
// ------------------

// ReadAllBytes
//
class function TFile.ReadAllBytes(const filename : String) : TBytes;
{$IFDEF VER200} // Delphi 2009
var
   fileStream : TFileStream;
   n : Integer;
begin
   fileStream:=TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
   try
      n:=fileStream.Size;
      SetLength(Result, n);
      if n>0 then
         fileStream.ReadBuffer(Result[0], n);
   finally
      fileStream.Free;
   end;
{$ELSE}
begin
   Result:=IOUTils.TFile.ReadAllBytes(filename);
{$ENDIF}
end;

// ------------------
// ------------------ TdwsThread ------------------
// ------------------

{$IFNDEF FPC}
{$IFDEF VER200}

// Start
//
procedure TdwsThread.Start;
begin
   Resume;
end;

{$ENDIF}
{$ENDIF}

end.
