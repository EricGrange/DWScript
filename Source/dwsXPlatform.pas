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
   {$DEFINE VER200}  // FPC compatibility = D2009
{$ENDIF}

interface

uses
   Classes, SysUtils, Types, Masks, Registry, SyncObjs, Variants, StrUtils,
   {$IFDEF FPC}
      {$IFDEF Windows}
         Windows
      {$ELSE}
         LCLIntf
      {$ENDIF}
   {$ELSE}
      Windows
      {$IFNDEF VER200}, IOUtils{$ENDIF}
   {$ENDIF}
   ;

const
{$IFDEF UNIX}
   cLineTerminator  = #10;
{$ELSE}
   cLineTerminator  = #13#10;
{$ENDIF}

   // following is missing from D2010
   INVALID_HANDLE_VALUE = NativeUInt(-1);

   {$ifdef FPC}
   // FreePascal RTL declares this constant, but does not support it,
   // so it just leads to runtime crashes, this attempts to trigger compile-time crashes instead
   varUString = 'varUString is not supported by FreePascal';
   {$endif}

type

   // see http://delphitools.info/2011/11/30/fixing-tcriticalsection/
   {$HINTS OFF}
   {$ifdef UNIX}
   TdwsCriticalSection = class (TCriticalSection);
   {$else}
   TdwsCriticalSection = class
      private
         FDummy : array [0..95-SizeOf(TRTLCRiticalSection)-2*SizeOf(Pointer)] of Byte;
         FCS : TRTLCriticalSection;

      public
         constructor Create;
         destructor Destroy; override;

         procedure Enter;
         procedure Leave;

         function TryEnter : Boolean;
   end;
   {$endif}

   IMultiReadSingleWrite = interface
      procedure BeginRead;
      function  TryBeginRead : Boolean;
      procedure EndRead;

      procedure BeginWrite;
      function  TryBeginWrite : Boolean;
      procedure EndWrite;
   end;

   TMultiReadSingleWriteState = (mrswUnlocked, mrswReadLock, mrswWriteLock);

   {$ifdef UNIX}{$define SRW_FALLBACK}{$endif}

   TMultiReadSingleWrite = class (TInterfacedObject, IMultiReadSingleWrite)
      private
         {$ifndef SRW_FALLBACK}
         FSRWLock : Pointer;
         FDummy : array [0..95-4*SizeOf(Pointer)] of Byte; // padding
         {$else}
         FLock : TdwsCriticalSection;
         {$endif}

      public
         {$ifdef SRW_FALLBACK}
         constructor Create;
         destructor Destroy; override;
         {$endif}

         procedure BeginRead; inline;
         function  TryBeginRead : Boolean; inline;
         procedure EndRead; inline;

         procedure BeginWrite; inline;
         function  TryBeginWrite : Boolean; inline;
         procedure EndWrite; inline;

         // use for diagnostic only
         function State : TMultiReadSingleWriteState;
   end;

   {$HINTS ON}

procedure SetDecimalSeparator(c : Char);
function GetDecimalSeparator : Char;

type
   TCollectFileProgressEvent = procedure (const directory : TFileName; var skipScan : Boolean) of object;

procedure CollectFiles(const directory, fileMask : TFileName;
                       list : TStrings; recurseSubdirectories: Boolean = False;
                       onProgress : TCollectFileProgressEvent = nil);
procedure CollectSubDirs(const directory : TFileName; list : TStrings);

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

   {$IFDEF FPC}
   TBytes = array of Byte;

   RawByteString = String;

   PNativeInt = ^NativeInt;
   PUInt64 = ^UInt64;
   {$ENDIF}

   TPath = class
      class function GetTempPath : String; static;
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

   // Wrap in a record so it is not assignment compatible without explicit casts
   // Internal representation is UnixTime in milliseconds (same as JavaScript)
   TdwsDateTime = record
      private
         FValue : Int64;

         function GetAsUnixTime : Int64;
         procedure SetAsUnixTime(const val : Int64);

         function GetAsFileTime : TFileTime;
         procedure SetAsFileTime(const val : TFileTime);
         function GetAsDosDateTime : Integer;

         function GetAsLocalDateTime : TDateTime;
         procedure SetAsLocalDateTime(const val : TDateTime);
         function GetAsUTCDateTime : TDateTime;
         procedure SetAsUTCDateTime(const val : TDateTime);

      public
         class function Now : TdwsDateTime; static;
         class function FromLocalDateTime(const dt : TDateTime) : TdwsDateTime; static;

         procedure Clear; inline;
         function IsZero : Boolean; inline;

         class operator Equal(const a, b : TdwsDateTime) : Boolean; static; inline;
         class operator NotEqual(const a, b : TdwsDateTime) : Boolean; static; inline;
         class operator GreaterThan(const a, b : TdwsDateTime) : Boolean; static; inline;
         class operator GreaterThanOrEqual(const a, b : TdwsDateTime) : Boolean; static; inline;
         class operator LessThan(const a, b : TdwsDateTime) : Boolean; static; inline;
         class operator LessThanOrEqual(const a, b : TdwsDateTime) : Boolean; static; inline;

         function MillisecondsAheadOf(const d : TdwsDateTime) : Int64; inline;
         procedure IncMilliseconds(const msec : Int64); inline;

         property Value : Int64 read FValue write FValue;

         property AsUnixTime : Int64 read GetAsUnixTime write SetAsUnixTime;
         property AsFileTime : TFileTime read GetAsFileTime write SetAsFileTime;
         property AsDosDateTime : Integer read GetAsDosDateTime;

         property AsLocalDateTime : TDateTime read GetAsLocalDateTime write SetAsLocalDateTime;
         property AsUTCDateTime : TDateTime read GetAsUTCDateTime write SetAsUTCDateTime;
   end;

// 64bit system clock reference in milliseconds since boot
function GetSystemMilliseconds : Int64;
function UTCDateTime : TDateTime;
function UnixTime : Int64;

function LocalDateTimeToUTCDateTime(t : TDateTime) : TDateTime;
function UTCDateTimeToLocalDateTime(t : TDateTime) : TDateTime;

function SystemMillisecondsToUnixTime(t : Int64) : Int64;
function UnixTimeToSystemMilliseconds(ut : Int64) : Int64;

procedure SystemSleep(msec : Integer);

function FirstWideCharOfString(const s : String; const default : WideChar = #0) : WideChar; inline;
procedure CodePointToUnicodeString(c : Integer; var result : UnicodeString);
procedure CodePointToString(const c : Integer; var result : String); inline;

{$ifndef FPC}
function UnicodeCompareStr(const S1, S2 : String) : Integer; inline;
function UnicodeStringReplace(const s, oldPattern, newPattern: String; flags: TReplaceFlags) : String; inline;
{$endif}

function UnicodeCompareP(p1 : PWideChar; n1 : Integer; p2 : PWideChar; n2 : Integer) : Integer; overload;
function UnicodeCompareP(p1, p2 : PWideChar; n : Integer) : Integer; overload;

function UnicodeLowerCase(const s : UnicodeString) : UnicodeString; overload;
function UnicodeUpperCase(const s : UnicodeString) : UnicodeString; overload;

{$ifdef FPC}
function UnicodeLowerCase(const s : String) : String; overload;
function UnicodeUpperCase(const s : String) : String; overload;
{$endif}

function ASCIICompareText(const s1, s2 : String) : Integer; inline;
function ASCIISameText(const s1, s2 : String) : Boolean; inline;

function NormalizeString(const s, form : String) : String;
function StripAccents(const s : String) : String;

function InterlockedIncrement(var val : Integer) : Integer; overload; {$IFDEF PUREPASCAL} inline; {$endif}
function InterlockedDecrement(var val : Integer) : Integer; {$IFDEF PUREPASCAL} inline; {$endif}

procedure FastInterlockedIncrement(var val : Integer); {$IFDEF PUREPASCAL} inline; {$endif}
procedure FastInterlockedDecrement(var val : Integer); {$IFDEF PUREPASCAL} inline; {$endif}

function InterlockedExchangePointer(var target : Pointer; val : Pointer) : Pointer; {$IFDEF PUREPASCAL} inline; {$endif}

function InterlockedCompareExchangePointer(var destination : Pointer; exchange, comparand : Pointer) : Pointer; {$IFDEF PUREPASCAL} inline; {$endif}

procedure SetThreadName(const threadName : PAnsiChar; threadID : Cardinal = Cardinal(-1));

procedure OutputDebugString(const msg : String);

procedure WriteToOSEventLog(const logName, logCaption, logDetails : String;
                            const logRawData : RawByteString = ''); overload;

{$ifdef FPC}
procedure VarCopy(out dest : Variant; const src : Variant); inline;
{$else}
function VarToUnicodeStr(const v : Variant) : String; inline;
{$endif}

{$ifdef FPC}
function Utf8ToUnicodeString(const buf : RawByteString) : UnicodeString; inline;
{$endif}

function RawByteStringToBytes(const buf : RawByteString) : TBytes;
function BytesToRawByteString(const buf : TBytes; startIndex : Integer = 0) : RawByteString; overload;
function BytesToRawByteString(p : Pointer; size : Integer) : RawByteString; overload;

function LoadDataFromFile(const fileName : TFileName) : TBytes;
procedure SaveDataToFile(const fileName : TFileName; const data : TBytes);

function LoadRawBytesFromFile(const fileName : TFileName) : RawByteString;
function SaveRawBytesToFile(const fileName : TFileName; const data : RawByteString) : Integer;

procedure LoadRawBytesAsScriptStringFromFile(const fileName : TFileName; var result : String);

function LoadTextFromBuffer(const buf : TBytes) : UnicodeString;
function LoadTextFromRawBytes(const buf : RawByteString) : UnicodeString;
function LoadTextFromStream(aStream : TStream) : UnicodeString;
function LoadTextFromFile(const fileName : TFileName) : UnicodeString;
procedure SaveTextToUTF8File(const fileName : TFileName; const text : UTF8String);
procedure AppendTextToUTF8File(const fileName : TFileName; const text : UTF8String);
function OpenFileForSequentialReadOnly(const fileName : TFileName) : THandle;
function OpenFileForSequentialWriteOnly(const fileName : TFileName) : THandle;
procedure CloseFileHandle(hFile : THandle);
function FileWrite(hFile : THandle; buffer : Pointer; byteCount : Integer) : Cardinal;
function FileFlushBuffers(hFile : THandle) : Boolean;
function FileCopy(const existing, new : TFileName; failIfExists : Boolean) : Boolean;
function FileMove(const existing, new : TFileName) : Boolean;
function FileDelete(const fileName : TFileName) : Boolean;
function FileRename(const oldName, newName : TFileName) : Boolean;
function FileSize(const name : TFileName) : Int64;
function FileDateTime(const name : TFileName; lastAccess : Boolean = False) : TdwsDateTime;
procedure FileSetDateTime(hFile : THandle; const aDateTime : TdwsDateTime);
function DeleteDirectory(const path : String) : Boolean;

function DirectSet8087CW(newValue : Word) : Word; register;
function DirectSetMXCSR(newValue : Word) : Word; register;

function SwapBytes(v : Cardinal) : Cardinal;
procedure SwapInt64(src, dest : PInt64);

function RDTSC : UInt64;

function GetCurrentUserName : String;

{$ifndef FPC}
// Generics helper functions to handle Delphi 2009 issues - HV
function TtoObject(const T): TObject; inline;
function TtoPointer(const T): Pointer; inline;
procedure GetMemForT(var T; Size: integer); inline;
{$endif}

procedure InitializeWithDefaultFormatSettings(var fmt : TFormatSettings);

type
   TTimerEvent = procedure of object;

   ITimer = interface
      procedure Cancel;
   end;

   TTimerTimeout = class (TInterfacedObject, ITimer)
      private
         FTimer : THandle;
         FOnTimer : TTimerEvent;

      public
         class function Create(delayMSec : Cardinal; onTimer : TTimerEvent) : ITimer;
         destructor Destroy; override;

         procedure Cancel;
   end;

{$ifndef SRW_FALLBACK}
procedure AcquireSRWLockExclusive(var SRWLock : Pointer); stdcall; external 'kernel32.dll';
function TryAcquireSRWLockExclusive(var SRWLock : Pointer) : BOOL; stdcall; external 'kernel32.dll';
procedure ReleaseSRWLockExclusive(var SRWLock : Pointer); stdcall; external 'kernel32.dll';

procedure AcquireSRWLockShared(var SRWLock : Pointer); stdcall; external 'kernel32.dll';
function TryAcquireSRWLockShared(var SRWLock : Pointer) : BOOL; stdcall; external 'kernel32.dll';
procedure ReleaseSRWLockShared(var SRWLock : Pointer); stdcall; external 'kernel32.dll';
{$endif}

type
   TModuleVersion = record
      Major, Minor : Word;
      Release, Build : Word;
      function AsString : String;
   end;

function GetModuleVersion(instance : THandle; var version : TModuleVersion) : Boolean;
function GetApplicationVersion(var version : TModuleVersion) : Boolean;
function ApplicationVersion : String;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$ifdef FPC}
type
   TFindExInfoLevels = FINDEX_INFO_LEVELS;
{$endif}

// GetSystemTimeMilliseconds
//
function GetSystemTimeMilliseconds : Int64; stdcall;
begin
{$IFDEF WINDOWS}
   Result := TdwsDateTime.Now.Value;
{$ELSE}
   Not yet implemented!
{$ENDIF}
end;

// GetSystemMilliseconds
//
var
   vGetSystemMilliseconds : function : Int64; stdcall;
function GetSystemMilliseconds : Int64;
{$ifdef WIN32_ASM}
asm
   jmp [vGetSystemMilliseconds]
{$else}
begin
   Result:=vGetSystemMilliseconds;
{$endif}
end;

// InitializeGetSystemMilliseconds
//
procedure InitializeGetSystemMilliseconds;
var
   h : THandle;
begin
   {$IFDEF WINDOWS}
   h:=LoadLibrary('kernel32.dll');
   vGetSystemMilliseconds:=GetProcAddress(h, 'GetTickCount64');
   {$ENDIF}
   if not Assigned(vGetSystemMilliseconds) then
      vGetSystemMilliseconds:=@GetSystemTimeMilliseconds;
end;

// UTCDateTime
//
function UTCDateTime : TDateTime;
var
   systemTime : TSystemTime;
begin
{$IFDEF Windows}
   FillChar(systemTime, SizeOf(systemTime), 0);
   GetSystemTime(systemTime);
   with systemTime do
      Result:= EncodeDate(wYear, wMonth, wDay)
              +EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
{$ELSE}
   Not yet implemented!
{$ENDIF}
end;

// UnixTime
//
function UnixTime : Int64;
begin
   Result:=Trunc(UTCDateTime*86400)-Int64(25569)*86400;
end;

type
   TDynamicTimeZoneInformation = record
      Bias : Longint;
      StandardName : array[0..31] of WCHAR;
      StandardDate : TSystemTime;
      StandardBias : Longint;
      DaylightName : array[0..31] of WCHAR;
      DaylightDate : TSystemTime;
      DaylightBias : Longint;
      TimeZoneKeyName : array[0..127] of WCHAR;
      DynamicDaylightTimeDisabled : Boolean;
   end;
   PDynamicTimeZoneInformation = ^TDynamicTimeZoneInformation;

function GetDynamicTimeZoneInformation(
      var pTimeZoneInformation: TDynamicTimeZoneInformation): DWORD; stdcall; external 'kernel32' {$ifndef FPC}delayed{$endif};
function GetTimeZoneInformationForYear(wYear: USHORT; lpDynamicTimeZoneInformation: PDynamicTimeZoneInformation;
      var lpTimeZoneInformation: TTimeZoneInformation): BOOL; stdcall; external 'kernel32' {$ifndef FPC}delayed{$endif};
function TzSpecificLocalTimeToSystemTime(lpTimeZoneInformation: PTimeZoneInformation;
      var lpLocalTime, lpUniversalTime: TSystemTime): BOOL; stdcall; external 'kernel32' {$ifndef FPC}delayed{$endif};

// LocalDateTimeToUTCDateTime
//
function LocalDateTimeToUTCDateTime(t : TDateTime) : TDateTime;
var
   localSystemTime, universalSystemTime : TSystemTime;
   tzDynInfo : TDynamicTimeZoneInformation;
   tzInfo : TTimeZoneInformation;
   y, m, d : Word;
begin
   DateTimeToSystemTime(t, localSystemTime);
   if GetDynamicTimeZoneInformation(tzDynInfo) = TIME_ZONE_ID_INVALID then
      RaiseLastOSError;
   DecodeDate(t, y, m, d);
   if not GetTimeZoneInformationForYear(y, @tzDynInfo, tzInfo) then
      RaiseLastOSError;
   if not TzSpecificLocalTimeToSystemTime(@tzInfo, localSystemTime, universalSystemTime) then
      RaiseLastOSError;
   Result := SystemTimeToDateTime(universalSystemTime);
end;

// UTCDateTimeToLocalDateTime
//
function UTCDateTimeToLocalDateTime(t : TDateTime) : TDateTime;
var
   tzDynInfo : TDynamicTimeZoneInformation;
   tzInfo : TTimeZoneInformation;
   localSystemTime, universalSystemTime : TSystemTime;
begin
   DateTimeToSystemTime(t, universalSystemTime);
   if GetDynamicTimeZoneInformation(tzDynInfo) = TIME_ZONE_ID_INVALID then
      RaiseLastOSError;
   if not GetTimeZoneInformationForYear(universalSystemTime.wYear, @tzDynInfo, tzInfo) then
      RaiseLastOSError;
   if not SystemTimeToTzSpecificLocalTime(@tzInfo, universalSystemTime, localSystemTime) then
      RaiseLastOSError;
   Result := SystemTimeToDateTime(localSystemTime);
end;

// SystemMillisecondsToUnixTime
//
function SystemMillisecondsToUnixTime(t : Int64) : Int64;
begin
   Result := UnixTime - (GetSystemTimeMilliseconds-t) div 1000;
end;

// UnixTimeToSystemMilliseconds
//
function UnixTimeToSystemMilliseconds(ut : Int64) : Int64;
begin
   Result := GetSystemTimeMilliseconds - (UnixTime-ut)*1000;
end;

// SystemSleep
//
procedure SystemSleep(msec : Integer);
begin
   if msec>=0 then
      Windows.Sleep(msec);
end;

// FirstWideCharOfString
//
function FirstWideCharOfString(const s : String; const default : WideChar = #0) : WideChar;
begin
   {$ifdef FPC}
   if s <> '' then
      Result := PWideChar(String(s))^
   else Result := default;
   {$else}
   if s <> '' then
      Result := PWideChar(Pointer(s))^
   else Result := default;
   {$endif}
end;

// CodePointToUnicodeString
//
procedure CodePointToUnicodeString(c : Integer; var result : UnicodeString);
begin
   case c of
      0..$FFFF :
         Result := WideChar(c);
      $10000..$10FFFF : begin
         c := c-$10000;
         Result := WideChar($D800+(c shr 10))+WideChar($DC00+(c and $3FF));
      end;
   else
      raise EConvertError.CreateFmt('Invalid codepoint: %d', [c]);
   end;
end;

// CodePointToString
//
procedure CodePointToString(const c : Integer; var result : String); inline;
{$ifdef FPC}
var
   buf : UnicodeString;
begin
   CodePointToUnicodeString(c, buf);
   result := String(buf);
{$else}
begin
   CodePointToUnicodeString(c, result);
{$endif}
end;

// UnicodeCompareStr
//
{$ifndef FPC}
function UnicodeCompareStr(const S1, S2 : String) : Integer;
begin
   Result:=CompareStr(S1, S2);
end;
{$endif}

// UnicodeStringReplace
//
function UnicodeStringReplace(const s, oldPattern, newPattern: String; flags: TReplaceFlags) : String;
begin
   Result := SysUtils.StringReplace(s, oldPattern, newPattern, flags);
end;

function CompareStringEx(
   lpLocaleName: LPCWSTR; dwCmpFlags: DWORD;
   lpString1: LPCWSTR; cchCount1: Integer;
   lpString2: LPCWSTR; cchCount2: Integer;
   lpVersionInformation: Pointer; lpReserved: LPVOID;
   lParam: LPARAM): Integer; stdcall; external 'kernel32.dll';

// UnicodeCompareP
//
function UnicodeCompareP(p1 : PWideChar; n1 : Integer; p2 : PWideChar; n2 : Integer) : Integer;
const
   CSTR_EQUAL = 2;
begin
   Result := CompareStringEx(nil, NORM_IGNORECASE, p1, n1, p2, n2, nil, nil, 0)-CSTR_EQUAL;
end;

// UnicodeCompareP
//
function UnicodeCompareP(p1, p2 : PWideChar; n : Integer) : Integer; overload;
const
   CSTR_EQUAL = 2;
begin
   Result := CompareStringEx(nil, NORM_IGNORECASE, p1, n, p2, n, nil, nil, 0) - CSTR_EQUAL;
end;

// UnicodeLowerCase
//
function UnicodeLowerCase(const s : UnicodeString) : UnicodeString;
begin
   if s<>'' then begin
      Result:=s;
      UniqueString(Result);
      Windows.CharLowerBuffW(PWideChar(Pointer(Result)), Length(Result));
   end else Result:=s;
end;

// UnicodeUpperCase
//
function UnicodeUpperCase(const s : UnicodeString) : UnicodeString;
begin
   if s<>'' then begin
      Result:=s;
      UniqueString(Result);
      Windows.CharUpperBuffW(PWideChar(Pointer(Result)), Length(Result));
   end else Result:=s;
end;

{$ifdef FPC}
// UnicodeLowerCase
//
function UnicodeLowerCase(const s : String) : String;
begin
   Result := String(UnicodeLowerCase(UnicodeString(s)));
end;

// UnicodeUpperCase
//
function UnicodeUpperCase(const s : String) : String;
begin
   Result := String(UnicodeUpperCase(UnicodeString(s)));
end;
{$endif}

// ASCIICompareText
//
function ASCIICompareText(const s1, s2 : String) : Integer; inline;
begin
   {$ifdef FPC}
   Result:=CompareText(UTF8Encode(s1), UTF8Encode(s2));
   {$else}
   Result:=CompareText(s1, s2);
   {$endif}
end;

// ASCIISameText
//
function ASCIISameText(const s1, s2 : String) : Boolean; inline;
begin
   {$ifdef FPC}
   Result:=(ASCIICompareText(s1, s2)=0);
   {$else}
   Result:=SameText(s1, s2);
   {$endif}
end;

// NormalizeString
//
function APINormalizeString(normForm : Integer; lpSrcString : LPCWSTR; cwSrcLength : Integer;
                            lpDstString : LPWSTR; cwDstLength : Integer) : Integer;
                            stdcall; external 'Normaliz.dll' name 'NormalizeString' {$ifndef FPC}delayed{$endif};
function NormalizeString(const s, form : String) : String;
var
   nf, len, n : Integer;
begin
   if s = '' then Exit('');
   if (form = '') or (form = 'NFC') then
      nf := 1
   else if form = 'NFD' then
      nf := 2
   else if form = 'NFKC' then
      nf := 5
   else if form = 'NFKD' then
      nf := 6
   else raise Exception.CreateFmt('Unsupported normalization form "%s"', [form]);
   n := 10;
   len := APINormalizeString(nf, Pointer(s), Length(s), nil, 0);
   repeat
      SetLength(Result, len);
      len := APINormalizeString(nf, PWideChar(s), Length(s), Pointer(Result), len);
      if len <= 0 then begin
         if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
            RaiseLastOSError;
         Dec(n);
         if n <= 0 then
            RaiseLastOSError;
         len := -len;
         len := len + (len div 4); // extra margin since estimation failed
         continue;
      end;
   until True;
   SetLength(Result, len);
end;

// StripAccents
//
function StripAccents(const s : String) : String;
var
   i : Integer;
   pSrc, pDest : PWideChar;
begin
   Result := NormalizeString(s, 'NFD');
   pSrc := Pointer(Result);
   pDest := pSrc;
   for i := 1 to Length(Result) do begin
      case Ord(pSrc^) of
         $300..$36F : ; // diacritic range
      else
         pDest^ := pSrc^;
         Inc(pDest);
      end;
      Inc(pSrc);
   end;
   SetLength(Result, (NativeUInt(pDest)-NativeUInt(Pointer(Result))) div 2);
end;

// InterlockedIncrement
//
function InterlockedIncrement(var val : Integer) : Integer;
{$ifndef WIN32_ASM}
begin
   Result:=Windows.InterlockedIncrement(val);
{$else}
asm
   mov   ecx,  eax
   mov   eax,  1
   lock  xadd [ecx], eax
   inc   eax
{$endif}
end;

// InterlockedDecrement
//
function InterlockedDecrement(var val : Integer) : Integer;
{$ifndef WIN32_ASM}
begin
   Result:=Windows.InterlockedDecrement(val);
{$else}
asm
   mov   ecx,  eax
   mov   eax,  -1
   lock  xadd [ecx], eax
   dec   eax
{$endif}
end;

// FastInterlockedIncrement
//
procedure FastInterlockedIncrement(var val : Integer);
{$ifndef WIN32_ASM}
begin
   InterlockedIncrement(val);
{$else}
asm
   lock  inc [eax]
{$endif}
end;

// FastInterlockedDecrement
//
procedure FastInterlockedDecrement(var val : Integer);
{$ifndef WIN32_ASM}
begin
   InterlockedDecrement(val);
{$else}
asm
   lock  dec [eax]
{$endif}
end;

// InterlockedExchangePointer
//
function InterlockedExchangePointer(var target : Pointer; val : Pointer) : Pointer;
{$ifndef WIN32_ASM}
begin
   {$ifdef FPC}
   Result:=System.InterLockedExchange(target, val);
   {$else}
   Result:=Windows.InterlockedExchangePointer(target, val);
   {$endif}
{$else}
asm
   lock  xchg dword ptr [eax], edx
   mov   eax, edx
{$endif}
end;

// InterlockedCompareExchangePointer
//
function InterlockedCompareExchangePointer(var destination : Pointer; exchange, comparand : Pointer) : Pointer; {$IFDEF PUREPASCAL} inline; {$endif}
begin
   {$ifdef FPC}
      {$ifdef CPU64}
      Result := Pointer(System.InterlockedCompareExchange64(QWord(destination), QWord(exchange), QWord(comparand)));
      {$else}
      Result:=System.InterLockedCompareExchange(destination, exchange, comparand);
      {$endif}
   {$else}
   Result:=Windows.InterlockedCompareExchangePointer(destination, exchange, comparand);
   {$endif}
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
   Windows.OutputDebugStringW(PWideChar(msg));
end;

// WriteToOSEventLog
//
procedure WriteToOSEventLog(const logName, logCaption, logDetails : String;
                            const logRawData : RawByteString = '');
var
  eventSource : THandle;
  detailsPtr : array [0..1] of PWideChar;
begin
   if logName<>'' then
      eventSource:=RegisterEventSourceW(nil, PWideChar(logName))
   else eventSource:=RegisterEventSourceW(nil, PWideChar(ChangeFileExt(ExtractFileName(ParamStr(0)), '')));
   if eventSource>0 then begin
      try
         detailsPtr[0]:=PWideChar(logCaption);
         detailsPtr[1]:=PWideChar(logDetails);
         ReportEventW(eventSource, EVENTLOG_INFORMATION_TYPE, 0, 0, nil,
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
type
   TFindDataRec = record
      Handle : THandle;
      Data : TWin32FindDataW;
   end;

   TMasks = array of TMask;

// CollectFilesMasked
//
procedure CollectFilesMasked(const directory : TFileName;
                             const masks : TMasks; list : TStrings;
                             recurseSubdirectories: Boolean = False;
                             onProgress : TCollectFileProgressEvent = nil);
const
   // contant defined in Windows.pas is incorrect
   FindExInfoBasic = 1;
var
   searchRec : TFindDataRec;
   infoLevel : TFindexInfoLevels;
   fileName : TFileName;
   skipScan, addToList : Boolean;
   i : Integer;
begin
   // 6.1 required for FindExInfoBasic (Win 2008 R2 or Win 7)
   if ((Win32MajorVersion shl 8) or Win32MinorVersion)>=$601 then
      infoLevel:=TFindexInfoLevels(FindExInfoBasic)
   else infoLevel:=FindExInfoStandard;

   if Assigned(onProgress) then begin
      skipScan:=False;
      onProgress(directory, skipScan);
      if skipScan then exit;
   end;

   fileName:=directory+'*';
   searchRec.Handle:=FindFirstFileEx(PChar(fileName), infoLevel,
                                     @searchRec.Data, FINDEX_SEARCH_OPS.FindExSearchNameMatch,
                                     nil, 0);
   if searchRec.Handle<>INVALID_HANDLE_VALUE then begin
      repeat
         if (searchRec.Data.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)=0 then begin
            // check file against mask
            fileName:=searchRec.Data.cFileName;
            addToList := True;
            for i := 0 to High(masks) do begin
               addToList := masks[i].Matches(fileName);
               if addToList then Break;
            end;
            if addToList then begin
               fileName:=directory+fileName;
               list.Add(fileName);
            end;
         end else if recurseSubdirectories then begin
            // dive in subdirectory
            if searchRec.Data.cFileName[0]='.' then begin
               if searchRec.Data.cFileName[1]='.' then begin
                  if searchRec.Data.cFileName[2]=#0 then continue;
               end else if searchRec.Data.cFileName[1]=#0 then continue;
            end;
            // decomposed cast and concatenation to avoid implicit string variable
            fileName:=searchRec.Data.cFileName;
            fileName:=directory+fileName+PathDelim;
            CollectFilesMasked(fileName, masks, list, True, onProgress);
         end;
      until not FindNextFileW(searchRec.Handle, searchRec.Data);
      Windows.FindClose(searchRec.Handle);
   end;
end;

// CollectFiles
//
procedure CollectFiles(const directory, fileMask : TFileName; list : TStrings;
                       recurseSubdirectories: Boolean = False;
                       onProgress : TCollectFileProgressEvent = nil);
var
   masks : TMasks;
   p, pNext : Integer;
begin
   if fileMask <> '' then begin
      p := 1;
      repeat
         pNext := PosEx(';', fileMask, p);
         if pNext < p then begin
            SetLength(masks, Length(masks)+1);
            masks[High(masks)] := TMask.Create(Copy(fileMask, p));
            break;
         end;
         if pNext > p then begin
            SetLength(masks, Length(masks)+1);
            masks[High(masks)] := TMask.Create(Copy(fileMask, p, pNext-p));
         end;
         p := pNext + 1;
      until p > Length(fileMask);
   end;
   // Windows can match 3 character filters with old DOS filenames
   // Mask confirmation is necessary
   try
      CollectFilesMasked(IncludeTrailingPathDelimiter(directory), masks,
                         list, recurseSubdirectories, onProgress);
   finally
      for p := 0 to High(masks) do
         masks[p].Free;
   end;
end;

// CollectSubDirs
//
procedure CollectSubDirs(const directory : TFileName; list : TStrings);
const
   // contant defined in Windows.pas is incorrect
   FindExInfoBasic = 1;
var
   searchRec : TFindDataRec;
   infoLevel : TFindexInfoLevels;
   fileName : TFileName;
begin
   // 6.1 required for FindExInfoBasic (Win 2008 R2 or Win 7)
   if ((Win32MajorVersion shl 8) or Win32MinorVersion)>=$601 then
      infoLevel:=TFindexInfoLevels(FindExInfoBasic)
   else infoLevel:=FindExInfoStandard;

   fileName := directory+'*';
   searchRec.Handle:=FindFirstFileEx(PChar(fileName), infoLevel,
                                     @searchRec.Data, FINDEX_SEARCH_OPS.FindExSearchLimitToDirectories,
                                     nil, 0);
   if searchRec.Handle<>INVALID_HANDLE_VALUE then begin
      repeat
         if (searchRec.Data.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)<>0 then begin
            if searchRec.Data.cFileName[0]='.' then begin
               if searchRec.Data.cFileName[1]='.' then begin
                  if searchRec.Data.cFileName[2]=#0 then continue;
               end else if searchRec.Data.cFileName[1]=#0 then continue;
            end;
            // decomposed cast and concatenation to avoid implicit string variable
            fileName := searchRec.Data.cFileName;
            list.Add(fileName);
         end;
      until not FindNextFileW(searchRec.Handle, searchRec.Data);
      Windows.FindClose(searchRec.Handle);
   end;
end;

{$ifdef FPC}
// VarCopy
//
procedure VarCopy(out dest : Variant; const src : Variant);
begin
   dest:=src;
end;
{$else}
// VarToUnicodeStr
//
function VarToUnicodeStr(const v : Variant) : String; inline;
begin
   Result := VarToStr(v);
end;
{$endif FPC}

{$ifdef FPC}
// Utf8ToUnicodeString
//
function Utf8ToUnicodeString(const buf : RawByteString) : UnicodeString; inline;
begin
   Result := UTF8Decode(buf);
end;
{$endif}

// RawByteStringToBytes
//
function RawByteStringToBytes(const buf : RawByteString) : TBytes;
var
   n : Integer;
begin
   n:=Length(buf);
   SetLength(Result, n);
   if n>0 then
      System.Move(buf[1], Result[0], n);
end;

// BytesToRawByteString
//
function BytesToRawByteString(const buf : TBytes; startIndex : Integer = 0) : RawByteString;
var
   n : Integer;
begin
   n:=Length(buf)-startIndex;
   if n<=0 then
      Result:=''
   else begin
      SetLength(Result, n);
      System.Move(buf[startIndex], Pointer(Result)^, n);
   end;
end;

// BytesToRawByteString
//
function BytesToRawByteString(p : Pointer; size : Integer) : RawByteString;
begin
   SetLength(Result, size);
   System.Move(p^, Pointer(Result)^, size);
end;

// TryTextToFloat
//
function TryTextToFloat(const s : PChar; var value : Extended; const formatSettings : TFormatSettings) : Boolean;
{$ifdef FPC}
var
   cw : Word;
begin
   cw:=Get8087CW;
   Set8087CW($133F);
   if TryStrToFloat(s, value, formatSettings) then
      Result:=(value>-1.7e308) and (value<1.7e308);
   if not Result then
      value:=0;
   asm fclex end;
   Set8087CW(cw);
{$else}
begin
//   Result:=TextToFloat(s, value, fvExtended, formatSettings);
   Result := TryStrToFloat(s, value, formatSettings);
//   Result := StrToFloat(s, formatSettings);
{$endif}
end;

// TryTextToFloatW
//
function TryTextToFloatW(const s : PWideChar; var value : Extended;
                        const formatSettings : TFormatSettings) : Boolean;
{$ifdef FPC}
var
   bufU : UnicodeString;
   buf : String;
begin
   bufU := s;
   buf := String(bufU);
   Result := TryTextToFloat(PChar(buf), value, formatSettings);
{$else}
begin
   Result:=TextToFloat(s, value, fvExtended, formatSettings)
{$endif}
end;

// LoadTextFromBuffer
//
function LoadTextFromBuffer(const buf : TBytes) : UnicodeString;
var
   n, sourceLen, len : Integer;
   encoding : TEncoding;
begin
   if buf=nil then
      Result:=''
   else begin
      encoding:=nil;
      n:=TEncoding.GetBufferEncoding(buf, encoding);
      if n=0 then
         encoding:=TEncoding.UTF8;
      if encoding=TEncoding.UTF8 then begin
         // handle UTF-8 directly, encoding.GetString returns an empty string
         // whenever a non-utf-8 character is detected, the implementation below
         // will return a '?' for non-utf8 characters instead
         sourceLen := Length(buf)-n;
         SetLength(Result, sourceLen);
         len := Utf8ToUnicode(Pointer(Result), sourceLen+1, PAnsiChar(buf)+n, sourceLen)-1;
         if len>0 then begin
            if len<>sourceLen then
               SetLength(Result, len);
         end else Result:=''
      end else begin
         Result:=encoding.GetString(buf, n, Length(buf)-n);
      end;
   end;
end;

// LoadTextFromRawBytes
//
function LoadTextFromRawBytes(const buf : RawByteString) : UnicodeString;
var
   b : TBytes;
begin
   if buf='' then Exit('');
   SetLength(b, Length(buf));
   System.Move(buf[1], b[0], Length(buf));
   Result:=LoadTextFromBuffer(b);
end;

// LoadTextFromStream
//
function LoadTextFromStream(aStream : TStream) : UnicodeString;
var
   n : Integer;
   buf : TBytes;
begin
   n := aStream.Size-aStream.Position;
   SetLength(buf, n);
   aStream.Read(buf[0], n);
   Result:=LoadTextFromBuffer(buf);
end;

// LoadTextFromFile
//
function LoadTextFromFile(const fileName : TFileName) : UnicodeString;
var
   buf : TBytes;
begin
   buf:=LoadDataFromFile(fileName);
   Result:=LoadTextFromBuffer(buf);
end;

// ReadFileChunked
//
function ReadFileChunked(hFile : THandle; const buffer; size : Integer) : Integer;
const
   CHUNK_SIZE = 16384;
var
   p : PByte;
   nRemaining : Integer;
   nRead : Cardinal;
begin
   p := @buffer;
   nRemaining := size;
   repeat
      if nRemaining > CHUNK_SIZE then
         nRead := CHUNK_SIZE
      else nRead := nRemaining;
      if not ReadFile(hFile, p^, nRead, nRead, nil) then
         RaiseLastOSError
      else if nRead = 0 then begin
         // file got trimmed while we were reading
         Exit(size-nRemaining);
      end;
      Dec(nRemaining, nRead);
      Inc(p, nRead);
   until nRemaining <= 0;
   Result := size;
end;

// LoadDataFromFile
//
function LoadDataFromFile(const fileName : TFileName) : TBytes;
const
   INVALID_FILE_SIZE = DWORD($FFFFFFFF);
var
   hFile : THandle;
   n, nRead : Cardinal;
begin
   if fileName='' then Exit(nil);
   hFile:=OpenFileForSequentialReadOnly(fileName);
   if hFile=INVALID_HANDLE_VALUE then Exit(nil);
   try
      n:=GetFileSize(hFile, nil);
      if n=INVALID_FILE_SIZE then
         RaiseLastOSError;
      if n>0 then begin
         SetLength(Result, n);
         nRead := ReadFileChunked(hFile, Result[0], n);
         if nRead < n then
            SetLength(Result, nRead);
      end else Result:=nil;
   finally
      CloseHandle(hFile);
   end;
end;

// SaveDataToFile
//
procedure SaveDataToFile(const fileName : TFileName; const data : TBytes);
var
   hFile : THandle;
   n, nWrite : DWORD;
begin
   hFile:=OpenFileForSequentialWriteOnly(fileName);
   try
      n:=Length(data);
      if n>0 then
         if not WriteFile(hFile, data[0], n, nWrite, nil) then
            RaiseLastOSError;
   finally
      CloseHandle(hFile);
   end;
end;

// LoadRawBytesFromFile
//
function LoadRawBytesFromFile(const fileName : TFileName) : RawByteString;
const
   INVALID_FILE_SIZE = DWORD($FFFFFFFF);
var
   hFile : THandle;
   n, nRead : Cardinal;
begin
   if fileName='' then Exit;
   hFile := OpenFileForSequentialReadOnly(fileName);
   if hFile = INVALID_HANDLE_VALUE then Exit;
   try
      n:=GetFileSize(hFile, nil);
      if n=INVALID_FILE_SIZE then
         RaiseLastOSError;
      if n>0 then begin
         SetLength(Result, n);
         nRead := ReadFileChunked(hFile, Pointer(Result)^, n);
         if nRead < n then
            SetLength(Result, nRead);
      end;
   finally
      CloseHandle(hFile);
   end;
end;

// SaveRawBytesToFile
//
function SaveRawBytesToFile(const fileName : TFileName; const data : RawByteString) : Integer;
var
   hFile : THandle;
   nWrite : DWORD;
begin
   Result:=0;
   hFile:=OpenFileForSequentialWriteOnly(fileName);
   try
      if data<>'' then begin
         Result:=Length(data);
         if not WriteFile(hFile, data[1], Result, nWrite, nil) then
            RaiseLastOSError;
      end;
   finally
      CloseHandle(hFile);
   end;
end;

// LoadRawBytesAsScriptStringFromFile
//
procedure LoadRawBytesAsScriptStringFromFile(const fileName : TFileName; var result : String);
const
   INVALID_FILE_SIZE = DWORD($FFFFFFFF);
var
   hFile : THandle;
   n, i, nRead : Cardinal;
   pDest : PWord;
   buffer : array [0..16383] of Byte;
begin
   if fileName='' then Exit;
   hFile:=OpenFileForSequentialReadOnly(fileName);
   if hFile=INVALID_HANDLE_VALUE then Exit;
   try
      n:=GetFileSize(hFile, nil);
      if n=INVALID_FILE_SIZE then
         RaiseLastOSError;
      if n>0 then begin
         SetLength(Result, n);
         pDest := Pointer(Result);
         repeat
            if n > SizeOf(Buffer) then
               nRead := SizeOf(Buffer)
            else nRead := n;
            if not ReadFile(hFile, buffer, nRead, nRead, nil) then
               RaiseLastOSError
            else if nRead = 0 then begin
               // file got trimmed while we were reading
               SetLength(Result, Length(Result)-Integer(n));
               Break;
            end;
            for i := 1 to nRead do begin
               pDest^ := buffer[i-1];
               Inc(pDest);
            end;
            Dec(n, nRead);
         until n <= 0;
      end;
   finally
      CloseHandle(hFile);
   end;
end;

// SaveTextToUTF8File
//
procedure SaveTextToUTF8File(const fileName : TFileName; const text : UTF8String);
begin
   SaveRawBytesToFile(fileName, UTF8Encode(text));
end;

// AppendTextToUTF8File
//
procedure AppendTextToUTF8File(const fileName : TFileName; const text : UTF8String);
var
   fs : TFileStream;
begin
   if text='' then Exit;
   if FileExists(fileName) then
      fs:=TFileStream.Create(fileName, fmOpenWrite or fmShareDenyNone)
   else fs:=TFileStream.Create(fileName, fmCreate);
   try
      fs.Seek(0, soFromEnd);
      fs.Write(text[1], Length(text));
   finally
      fs.Free;
   end;
end;

// OpenFileForSequentialReadOnly
//
function OpenFileForSequentialReadOnly(const fileName : TFileName) : THandle;
begin
   Result:=CreateFile(PChar(fileName), GENERIC_READ, FILE_SHARE_READ+FILE_SHARE_WRITE,
                      nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
   if Result=INVALID_HANDLE_VALUE then begin
      if GetLastError<>ERROR_FILE_NOT_FOUND then
         RaiseLastOSError;
   end;
end;

// OpenFileForSequentialWriteOnly
//
function OpenFileForSequentialWriteOnly(const fileName : TFileName) : THandle;
begin
   Result:=CreateFile(PChar(fileName), GENERIC_WRITE, 0, nil, CREATE_ALWAYS,
                      FILE_ATTRIBUTE_NORMAL+FILE_FLAG_SEQUENTIAL_SCAN, 0);
   if Result=INVALID_HANDLE_VALUE then
      RaiseLastOSError;
end;

// CloseFileHandle
//
procedure CloseFileHandle(hFile : THandle);
begin
   CloseHandle(hFile);
end;

// FileWrite
//
function FileWrite(hFile : THandle; buffer : Pointer; byteCount : Integer) : Cardinal;
begin
   if not WriteFile(hFile, buffer^, byteCount, Result, nil) then
      RaiseLastOSError;
end;

// FileFlushBuffers
//
function FlushFileBuffers(hFile : THandle) : BOOL; stdcall; external 'kernel32.dll';
function FileFlushBuffers(hFile : THandle) : Boolean;
begin
   Result := FlushFileBuffers(hFile);
end;

// FileCopy
//
function FileCopy(const existing, new : TFileName; failIfExists : Boolean) : Boolean;
begin
   Result:=Windows.CopyFileW(PWideChar(existing), PWideChar(new), failIfExists);
end;

// FileMove
//
function FileMove(const existing, new : TFileName) : Boolean;
begin
   Result:=Windows.MoveFileW(PWideChar(existing), PWideChar(new));
end;

// FileDelete
//
function FileDelete(const fileName : TFileName) : Boolean;
begin
   Result:=SysUtils.DeleteFile(fileName);
end;

// FileRename
//
function FileRename(const oldName, newName : TFileName) : Boolean;
begin
   Result:=RenameFile(oldName, newName);
end;

// FileSize
//
function FileSize(const name : TFileName) : Int64;
var
   info : TWin32FileAttributeData;
begin
   if GetFileAttributesExW(PWideChar(Pointer(name)), GetFileExInfoStandard, @info) then
      Result:=info.nFileSizeLow or (Int64(info.nFileSizeHigh) shl 32)
   else Result:=-1;
end;

// FileDateTime
//
function FileDateTime(const name : TFileName; lastAccess : Boolean = False) : TdwsDateTime;
var
   info : TWin32FileAttributeData;
   fileTime : TFileTime;
   buf : TdwsDateTime;
begin
   if GetFileAttributesExW(PWideChar(Pointer(name)), GetFileExInfoStandard, @info) then begin
      if lastAccess then
         FileTimeToLocalFileTime(info.ftLastAccessTime, fileTime)
      else FileTimeToLocalFileTime(info.ftLastWriteTime, fileTime);
      buf.AsFileTime := fileTime;
   end else buf.Clear;
   Result := buf;
end;

// FileSetDateTime
//
procedure FileSetDateTime(hFile : THandle; const aDateTime : TdwsDateTime);
var
   doNotChange, newTimeStamp : TFileTime;
begin
   newTimeStamp := aDateTime.AsFileTime;
   doNotChange.dwLowDateTime  := Cardinal(-1);
   doNotChange.dwHighDateTime := Cardinal(-1);
   SetFileTime(hFile, @doNotChange, @newTimeStamp, @newTimeStamp);
end;

// DeleteDirectory
//
function DeleteDirectory(const path : String) : Boolean;
begin
   {$ifdef FPC}
   Result := RemoveDir(path);
   {$else}
   try
      TDirectory.Delete(path, True);
   except
      Exit(False);
   end;
   Result := not TDirectory.Exists(path);
   {$endif}
end;

// DirectSet8087CW
//
function DirectSet8087CW(newValue: Word): Word; register;
{$IFNDEF WIN32_ASM}
begin
   Result:=newValue;
{$else}
asm
   push    eax
   push    eax
   fnstcw  [esp]
   fnclex
   pop     eax
   fldcw   [esp]
   pop     edx
{$endif}
end;

// DirectSetMXCSR
//
function DirectSetMXCSR(newValue : Word) : Word; register;
{$ifdef WIN32_ASM}
asm
   and      eax, $FFC0
   push     eax
   push     eax
   stmxcsr  [esp+4]
   ldmxcsr  [esp]
   pop eax
   pop eax
{$else}
begin
   Result:=newValue;
{$endif}
end;

// SwapBytes
//
function SwapBytes(v : Cardinal) : Cardinal;
{$ifdef WIN32_ASM}
asm
   bswap eax
{$else}
type
   TCardinalBytes = array [0..3] of Byte;
begin
   TCardinalBytes(Result)[0] := TCardinalBytes(v)[3];
   TCardinalBytes(Result)[1] := TCardinalBytes(v)[2];
   TCardinalBytes(Result)[2] := TCardinalBytes(v)[1];
   TCardinalBytes(Result)[3] := TCardinalBytes(v)[0];
{$endif}
end;

// SwapInt64
//
procedure SwapInt64(src, dest : PInt64);
{$ifdef WIN32_ASM}
asm
   mov   ecx, [eax]
   mov   eax, [eax+4]
   bswap ecx
   bswap eax
   mov   [edx+4], ecx
   mov   [edx], eax
{$else}
begin
   PByteArray(dest)[0] := PByteArray(src)[7];
   PByteArray(dest)[1] := PByteArray(src)[6];
   PByteArray(dest)[2] := PByteArray(src)[5];
   PByteArray(dest)[3] := PByteArray(src)[4];
   PByteArray(dest)[4] := PByteArray(src)[3];
   PByteArray(dest)[5] := PByteArray(src)[2];
   PByteArray(dest)[6] := PByteArray(src)[1];
   PByteArray(dest)[7] := PByteArray(src)[0];
{$endif}
end;

// RDTSC
//
function RDTSC : UInt64;
asm
   RDTSC
end;

// GetCurrentUserName
//
function GetCurrentUserName : String;
var
   len : Cardinal;
begin
   len:=255;
   SetLength(Result, len);
   Windows.GetUserNameW(PWideChar(Result), len);
   SetLength(Result, len-1);
end;

{$ifndef FPC}
// Delphi 2009 is not able to cast a generic T instance to TObject or Pointer
function TtoObject(const T): TObject;
begin
// Manually inlining the code would require the IF-defs
//{$IF Compilerversion >= 21}
   Result := TObject(T);
//{$ELSE}
//   Result := PObject(@T)^;
//{$IFEND}
end;

function TtoPointer(const T): Pointer;
begin
// Manually inlining the code would require the IF-defs
//{$IF Compilerversion >= 21}
   Result := Pointer(T);
//{$ELSE}
//   Result := PPointer(@T)^;
//{$IFEND}
end;

procedure GetMemForT(var T; Size: integer); inline;
begin
  GetMem(Pointer(T), Size);
end;
{$endif}

// InitializeWithDefaultFormatSettings
//
procedure InitializeWithDefaultFormatSettings(var fmt : TFormatSettings);
begin
   {$ifdef DELPHI_XE_PLUS}
   fmt:=SysUtils.FormatSettings;
   {$else}
   fmt:=SysUtils.TFormatSettings((@CurrencyString{%H-})^);
   {$endif}
end;

// AsString
//
function TModuleVersion.AsString : String;
begin
   Result := Format('%d.%d.%d.%d', [Major, Minor, Release, Build]);
end;

// Adapted from Ian Boyd code published in
// http://stackoverflow.com/questions/10854958/how-to-get-version-of-running-executable
function GetModuleVersion(instance : THandle; var version : TModuleVersion) : Boolean;
var
   fileInformation : PVSFIXEDFILEINFO;
   verlen : Cardinal;
   rs : TResourceStream;
   m : TMemoryStream;
   resource : HRSRC;
begin
   Result:=False;

   // Workaround bug in Delphi if resource doesn't exist
   resource:=FindResource(instance, PChar(1), RT_VERSION);
   if resource=0 then Exit;

   m:=TMemoryStream.Create;
   try
      rs:=TResourceStream.CreateFromID(instance, 1, RT_VERSION);
      try
         m.CopyFrom(rs, rs.Size);
      finally
         rs.Free;
      end;

      m.Position:=0;
      if VerQueryValue(m.Memory, '\', Pointer(fileInformation), verlen) then begin
         version.Major := fileInformation.dwFileVersionMS shr 16;
         version.Minor := fileInformation.dwFileVersionMS and $FFFF;
         version.Release := fileInformation.dwFileVersionLS shr 16;
         version.Build := fileInformation.dwFileVersionLS and $FFFF;
         Result := True;
      end;
   finally
      m.Free;
   end;
end;

// GetApplicationVersion
//
var
   vApplicationVersion : TModuleVersion;
   vApplicationVersionRetrieved : Integer;
function GetApplicationVersion(var version : TModuleVersion) : Boolean;
begin
   if vApplicationVersionRetrieved = 0 then begin
      if GetModuleVersion(HInstance, vApplicationVersion) then
         vApplicationVersionRetrieved := 1
      else vApplicationVersionRetrieved := -1;
   end;
   Result := (vApplicationVersionRetrieved = 1);
   if Result then
      version := vApplicationVersion;
end;

// ApplicationVersion
//
function ApplicationVersion : String;
var
   version : TModuleVersion;
begin
   {$ifdef WIN64}
   if GetApplicationVersion(version) then
      Result := version.AsString + ' 64bit'
   else Result := '?.?.?.? 64bit';
   {$else}
   if GetApplicationVersion(version) then
      Result := version.AsString + ' 32bit'
   else Result := '?.?.?.? 32bit';
   {$endif}
end;

// ------------------
// ------------------ TdwsCriticalSection ------------------
// ------------------

// Create
//
constructor TdwsCriticalSection.Create;
begin
   InitializeCriticalSection(FCS);
end;

// Destroy
//
destructor TdwsCriticalSection.Destroy;
begin
   DeleteCriticalSection(FCS);
end;

// Enter
//
procedure TdwsCriticalSection.Enter;
begin
   EnterCriticalSection(FCS);
end;

// Leave
//
procedure TdwsCriticalSection.Leave;
begin
   LeaveCriticalSection(FCS);
end;

// TryEnter
//
function TdwsCriticalSection.TryEnter : Boolean;
begin
   Result:=TryEnterCriticalSection(FCS);
end;

// ------------------
// ------------------ TPath ------------------
// ------------------

// GetTempPath
//
class function TPath.GetTempPath : String;
{$IFDEF WINDOWS}
var
   tempPath : array [0..MAX_PATH] of WideChar; // Buf sizes are MAX_PATH+1
begin
   if Windows.GetTempPath(MAX_PATH, @tempPath[0])=0 then begin
      tempPath[1]:='.'; // Current directory
      tempPath[2]:=#0;
   end;
   Result:=tempPath;
{$ELSE}
begin
   Result:=IOUTils.TPath.GetTempPath;
{$ENDIF}
end;

// GetTempFileName
//
class function TPath.GetTempFileName : String;
{$IFDEF WINDOWS}
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

// ------------------
// ------------------ TMultiReadSingleWrite ------------------
// ------------------

{$ifndef SRW_FALLBACK}
procedure TMultiReadSingleWrite.BeginRead;
begin
   AcquireSRWLockShared(FSRWLock);
end;

function TMultiReadSingleWrite.TryBeginRead : Boolean;
begin
   Result:=TryAcquireSRWLockShared(FSRWLock);
end;

procedure TMultiReadSingleWrite.EndRead;
begin
   ReleaseSRWLockShared(FSRWLock)
end;

procedure TMultiReadSingleWrite.BeginWrite;
begin
   AcquireSRWLockExclusive(FSRWLock);
end;

function TMultiReadSingleWrite.TryBeginWrite : Boolean;
begin
   Result:=TryAcquireSRWLockExclusive(FSRWLock);
end;

procedure TMultiReadSingleWrite.EndWrite;
begin
   ReleaseSRWLockExclusive(FSRWLock)
end;

function TMultiReadSingleWrite.State : TMultiReadSingleWriteState;
begin
   // Attempt to guess the state of the lock without making assumptions
   // about implementation details
   // This is only for diagnosing locking issues
   if TryBeginWrite then begin
      EndWrite;
      Result:=mrswUnlocked;
   end else if TryBeginRead then begin
      EndRead;
      Result:=mrswReadLock;
   end else begin
      Result:=mrswWriteLock;
   end;
end;
{$else} // SRW_FALLBACK
constructor TMultiReadSingleWrite.Create;
begin
   FLock := TdwsCriticalSection.Create;
end;

destructor TMultiReadSingleWrite.Destroy;
begin
   FLock.Free;
end;

procedure TMultiReadSingleWrite.BeginRead;
begin
   FLock.Enter;
end;

function TMultiReadSingleWrite.TryBeginRead : Boolean;
begin
   Result:=FLock.TryEnter;
end;

procedure TMultiReadSingleWrite.EndRead;
begin
   FLock.Leave;
end;

procedure TMultiReadSingleWrite.BeginWrite;
begin
   FLock.Enter;
end;

function TMultiReadSingleWrite.TryBeginWrite : Boolean;
begin
   Result:=FLock.TryEnter;
end;

procedure TMultiReadSingleWrite.EndWrite;
begin
   FLock.Leave;
end;

function TMultiReadSingleWrite.State : TMultiReadSingleWriteState;
begin
   if FLock.TryEnter then begin
      FLock.Leave;
      Result := mrswUnlocked;
   end else Result := mrswWriteLock;
end;

{$endif}

// ------------------
// ------------------ TTimerTimeout ------------------
// ------------------

{$ifdef FPC}
type TWaitOrTimerCallback = procedure (Context: Pointer; Success: Boolean); stdcall;
function CreateTimerQueueTimer(out phNewTimer: THandle;
   TimerQueue: THandle; CallBack: TWaitOrTimerCallback;
   Parameter: Pointer; DueTime: DWORD; Period: DWORD; Flags: ULONG): BOOL; stdcall; external 'kernel32.dll';
function DeleteTimerQueueTimer(TimerQueue: THandle;
   Timer: THandle; CompletionEvent: THandle): BOOL; stdcall; external 'kernel32.dll';
const
   WT_EXECUTEDEFAULT       = ULONG($00000000);
   WT_EXECUTEONLYONCE      = ULONG($00000008);
   WT_EXECUTELONGFUNCTION  = ULONG($00000010);
{$endif}

procedure TTimerTimeoutCallBack(Context: Pointer; {%H-}Success: Boolean); stdcall;
var
   tt : TTimerTimeout;
   event : TTimerEvent;
begin
   tt := TTimerTimeout(Context);
   tt._AddRef;
   try
      event := tt.FOnTimer;
      if Assigned(event) then
         event();
      DeleteTimerQueueTimer(0, tt.FTimer, 0);
      tt.FTimer := 0;
   finally
      tt._Release;
   end;
end;

// Create
//
class function TTimerTimeout.Create(delayMSec : Cardinal; onTimer : TTimerEvent) : ITimer;
var
   obj : TTimerTimeout;
begin
   obj := TTimerTimeout(inherited Create);
   Result := obj;
   obj.FOnTimer := onTimer;
   CreateTimerQueueTimer(obj.FTimer, 0, TTimerTimeoutCallBack, obj,
                         delayMSec, 0,
                         WT_EXECUTEDEFAULT or WT_EXECUTELONGFUNCTION or WT_EXECUTEONLYONCE);
end;

// Destroy
//
destructor TTimerTimeout.Destroy;
begin
   Cancel;
   inherited;
end;

// Cancel
//
procedure TTimerTimeout.Cancel;
begin
   FOnTimer := nil;
   if FTimer = 0 then Exit;
   DeleteTimerQueueTimer(0, FTimer, INVALID_HANDLE_VALUE);
   FTimer:=0;
end;

// ------------------
// ------------------ TdwsDateTime ------------------
// ------------------

// Now
//
class function TdwsDateTime.Now : TdwsDateTime;
var
   fileTime : TFileTime;
begin
   GetSystemTimeAsFileTime(fileTime);
   Result.AsFileTime := fileTime;
end;

// FromLocalDateTime
//
class function TdwsDateTime.FromLocalDateTime(const dt : TDateTime) : TdwsDateTime;
begin
   Result.AsLocalDateTime := dt;
end;

// Clear
//
procedure TdwsDateTime.Clear;
begin
   FValue := 0;
end;

// IsZero
//
function TdwsDateTime.IsZero : Boolean;
begin
   Result := FValue = 0;
end;

// Equal
//
class operator TdwsDateTime.Equal(const a, b : TdwsDateTime) : Boolean;
begin
   Result := a.FValue = b.FValue;
end;

// NotEqual
//
class operator TdwsDateTime.NotEqual(const a, b : TdwsDateTime) : Boolean;
begin
   Result := a.FValue <> b.FValue;
end;

// GreaterThan
//
class operator TdwsDateTime.GreaterThan(const a, b : TdwsDateTime) : Boolean;
begin
   Result := a.FValue > b.FValue;
end;

// GreaterThanOrEqual
//
class operator TdwsDateTime.GreaterThanOrEqual(const a, b : TdwsDateTime) : Boolean;
begin
   Result := a.FValue >= b.FValue;
end;

// LessThan
//
class operator TdwsDateTime.LessThan(const a, b : TdwsDateTime) : Boolean;
begin
   Result := a.FValue < b.FValue;
end;

// LessThanOrEqual
//
class operator TdwsDateTime.LessThanOrEqual(const a, b : TdwsDateTime) : Boolean;
begin
   Result := a.FValue <= b.FValue;
end;

// MillisecondsAheadOf
//
function TdwsDateTime.MillisecondsAheadOf(const d : TdwsDateTime) : Int64;
begin
   Result := FValue - d.FValue;
end;

// IncMilliseconds
//
procedure TdwsDateTime.IncMilliseconds(const msec : Int64);
begin
   Inc(FValue, msec);
end;

const
   cFileTime_UnixTimeStart : Int64 = $019DB1DED53E8000; // January 1, 1970 (start of Unix epoch) in "ticks"
   cFileTime_TicksPerMillisecond : Int64 = 10000;       // a tick is 100ns

// SetAsFileTime
//
procedure TdwsDateTime.SetAsFileTime(const val : TFileTime);
var
   temp : LARGE_INTEGER;
begin
   temp.LowPart := val.dwLowDateTime;
   temp.HighPart := val.dwHighDateTime;
   FValue := (temp.QuadPart - cFileTime_UnixTimeStart) div cFileTime_TicksPerMillisecond;
end;

// GetAsDosDateTime
//
function TdwsDateTime.GetAsDosDateTime : Integer;
var
   fileTime : TFileTime;
   dosTime : LongRec;
begin
   fileTime := AsFileTime;
   FileTimeToDosDateTime(fileTime, dosTime.Hi, dosTime.Lo);
   Result := Integer(dosTime);
end;

// GetAsFileTime
//
function TdwsDateTime.GetAsFileTime : TFileTime;
var
   temp : LARGE_INTEGER;
begin
   temp.QuadPart := (FValue * cFileTime_TicksPerMillisecond) + cFileTime_UnixTimeStart;
   Result.dwLowDateTime := temp.LowPart;
   Result.dwHighDateTime := temp.HighPart;
end;

// GetAsUnixTime
//
function TdwsDateTime.GetAsUnixTime : Int64;
begin
   Result := FValue div 1000;
end;

// SetAsUnixTime
//
procedure TdwsDateTime.SetAsUnixTime(const val : Int64);
begin
   FValue := val * 1000;
end;

// GetAsLocalDateTime
//
function TdwsDateTime.GetAsLocalDateTime : TDateTime;
begin
   Result := UTCDateTimeToLocalDateTime(AsUTCDateTime);
end;

// SetAsLocalDateTime
//
procedure TdwsDateTime.SetAsLocalDateTime(const val : TDateTime);
begin
   AsUTCDateTime := LocalDateTimeToUTCDateTime(val);
end;

// GetAsUTCDateTime
//
function TdwsDateTime.GetAsUTCDateTime : TDateTime;
begin
   Result := FValue / 864e5 + 25569;
end;

// SetAsUTCDateTime
//
procedure TdwsDateTime.SetAsUTCDateTime(const val : TDateTime);
begin
   FValue := Round((val - 25569) * 864e5);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   InitializeGetSystemMilliseconds;

end.
