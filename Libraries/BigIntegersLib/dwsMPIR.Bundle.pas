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
{

   Bundles MPIR dll (http://www.mpir.org)

}
unit dwsMPIR.Bundle;

{$R mpir.RES}

interface

uses
   Windows, SysUtils,
   dwsSHA3,
   dwsXPlatform, dwsMPIR;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
   vDLLFileName : String;

// UnBundle_MPIR_DLL
//
function UnBundle_MPIR_DLL : String;
var
   res : HRSRC;
   hglob : HGLOBAL;
   resourceSize : Integer;
   dataPtr : Pointer;

   procedure OpenResource(const name, typ : String);
   begin
      res := FindResource(0, PChar(name), PChar(typ));
      if res = 0 then
         dataPtr := nil
      else begin
         hglob := LoadResource(0, res);
         resourceSize := SizeofResource(0, res);
         dataPtr := LockResource(hglob);
      end;
   end;

   procedure CloseResource;
   begin
      if hglob <> 0 then begin
         UnlockResource(hglob);
         hglob := 0;
      end;
   end;

   function ResourceSHA3 : String;
   var
      buf : RawByteString;
   begin
      OpenResource('mpir', 'bundle_sha3');
      Assert(dataPtr <> nil, 'mpir.dll.sha3 not bundled');

      SetLength(buf, resourceSize);
      System.Move(dataPtr^, Pointer(buf)^, resourceSize);

      Result := Trim(UTF8ToUnicodeString(buf));

      CloseResource;
   end;

   procedure ExtractResource;
   var
      f : HFILE;
   begin
      OpenResource('mpir', 'bundle');
      Assert(dataPtr <> nil, 'mpir.dll not bundled');

      f := CreateFileW(PWideChar(vDLLFileName), GENERIC_WRITE, 0, nil, CREATE_ALWAYS,
                       FILE_ATTRIBUTE_TEMPORARY, 0);
      FileWrite(f, dataPtr, resourceSize);
      CloseFileHandle(f);

      CloseResource;
   end;

var
   buf : RawByteString;
   sha3 : String;
begin
   if vDLLFileName <> '' then Exit(vDLLFileName);
   sha3 := ResourceSHA3;
   vDLLFileName := IncludeTrailingPathDelimiter(TPath.GetTempPath) + Copy(sha3, 1, 40)  + '.dll';

   buf := LoadRawBytesFromFile(vDLLFileName);
   if buf <> '' then begin
      if HashSHA3_256(buf) <> sha3 then
         buf := '';
   end;

   if buf = '' then
      ExtractResource;

   Result := vDLLFileName;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   vOnNeedMPIRDynamicDLLName := UnBundle_MPIR_DLL;

end.
