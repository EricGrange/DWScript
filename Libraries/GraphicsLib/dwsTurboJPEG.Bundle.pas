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

function UnBundle_TurboJPEG_DLL : String;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
   Windows, SysUtils, Classes, System.Zip,
   dwsSHA3, dwsXPlatform, LibTurboJPEG;

var
   vDLLFileName : String;
   vCS : TRTLCriticalSection;

// DoUnBundle
//
procedure DoUnBundle;
begin
   var zip := TZipFile.Create;
   try
      var stream := TResourceStream.Create(0, 'turbojpeg', 'bundle');
      try
         zip.Open(stream, zmRead);
         var sha3Index := 0;
         if zip.FileInfo[0].UncompressedSize > 99 then
            sha3Index := 1;
         var buf : TBytes;
         zip.Read(sha3Index, buf);
         var sha3 := '';
         BytesToScriptString(Pointer(buf), Length(buf), sha3);
         var dllName := IncludeTrailingPathDelimiter(TPath.GetTempPath) + Copy(sha3, 1, 40)  + '.dll';
         var diskDllData := LoadRawBytesFromFile(dllName);
         if diskDllData <> '' then begin
            if HashSHA3_256(diskDllData) <> Trim(sha3) then
               diskDllData := '';
         end;
         if diskDllData = '' then begin
            zip.Read(1-sha3Index, buf);
            SaveDataToFile(dllName, buf);
         end;
         vDLLFileName := dllName;   // only set if successful
      finally
         stream.Free
      end;
   finally
      zip.Free;
   end;
end;

// UnBundle_TurboJPEG_DLL
//
function UnBundle_TurboJPEG_DLL : String;
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
   Result := vDLLFileName;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   InitializeCriticalSection(vCS);
   vOnNeedTurboJPEGDLLName := UnBundle_TurboJPEG_DLL;

finalization

   vOnNeedTurboJPEGDLLName := nil;
   DeleteCriticalSection(vCS);

end.
