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
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
{

   Bundles turbojpeg dll

}
unit dwsTurboJPEG.Bundle;

{$I dws.inc}

{$ifdef WIN64}
   {$R turbojpeg-64.RES}
{$endif}
{$ifdef WIN32}
   {$R turbojpeg-32.RES}
{$endif}

interface

uses
   Windows, SysUtils,
   dwsSHA3,
   dwsXPlatform, LibTurboJPEG;

procedure UnBundleTurboJPEG;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SynZip;

var
   vDLLFileName : String;
   vCS : TRTLCriticalSection;

// DoUnBundle
//
procedure DoUnBundle;
var
   dllName : String;
   sha3Index : Integer;
begin
   var zip := TZipRead.Create(0, 'turbojpeg', 'bundle');
   try
      if zip.Entry[0].infoLocal.zfullSize < 99 then
         sha3Index := 0
      else sha3Index := 1;
      var sha3 := String(zip.UnZip(sha3Index));
      dllName := IncludeTrailingPathDelimiter(TPath.GetTempPath) + Copy(sha3, 1, 40)  + '.dll';
      var buf := LoadRawBytesFromFile(dllName);
      if buf <> '' then begin
         if HashSHA3_256(buf) <> sha3 then
            buf := '';
      end;
      if buf = '' then
         SaveRawBytesToFile(vDLLFileName, zip.UnZip(1-sha3Index));
   finally
      zip.Free;
   end;
   vDLLFileName := dllName;   // only set if successful
   LoadTurboJPEG(dllName);
end;

// UnBundleTurboJPEG
//
procedure UnBundleTurboJPEG;
begin
   if vDLLFileName = '' then begin
      EnterCriticalSection(vCS);
      try
         if vDLLFileName = '' then
            DoUnBundle;
      finally
         LeaveCriticalSection(vCS);
      end;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   InitializeCriticalSection(vCS);

finalization

   DeleteCriticalSection(vCS);

end.
