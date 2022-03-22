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
{    Based on WriteJPEG example from OpenGL24.de                       }
{    http://www.opengl24.de/header/libjpeg                             }
{                                                                      }
{    Further changes Copyright Creative IT.                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsTurboJPEG;

interface

uses
   SysUtils,
   dwsUtils,
   LibTurboJPEG,
   dwsJPEGEncoderOptions;

function CompressJPEG(rgbData : Pointer; width, height, quality : Integer;
                      const options : TJPEGOptions = [];
                      const comment : RawByteString = '') : RawByteString;

function DecompressJPEG_RGBA(jpegData : Pointer; jpegDataLength : Integer;
                             downscale : Integer; var w, h : Integer) : TBytes;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// CompressJPEG
//
function CompressJPEG(rgbData : Pointer; width, height, quality : Integer;
                      const options : TJPEGOptions = [];
                      const comment : RawByteString = '') : RawByteString;
var
   outBuf : Pointer;
   outSize : Cardinal;
begin
   var jpeg := TJ.InitCompress;
   try
      outBuf := nil;
      outSize := 0;
      var flags := 0;
      if quality >= 90 then
         flags := flags + TJFLAG_ACCURATEDCT;
      if jpgoProgressive in options then
         flags := flags + TJFLAG_PROGRESSIVE;
      if TJ.Compress2(jpeg, PByte(rgbData), width, 0, height, TJPF_RGBA, @outBuf, @outSize, TJSAMP_420, quality, flags) <> 0 then
         RaiseLastTurboJPEGError(jpeg);
      if outSize > 0 then try
         SetLength(Result, outSize);
         System.Move(outBuf^, Pointer(Result)^, outSize);
      finally
         TJ.Free(outBuf);
      end;
   finally
      TJ.Destroy(jpeg);
   end;
end;

// DecompressJPEG_RGBA
//
function DecompressJPEG_RGBA(jpegData : Pointer; jpegDataLength : Integer;
                             downscale : Integer; var w, h : Integer) : TBytes;
begin
   var jpeg := TJ.InitDecompress;
   try
      var jpegSubSamp := 0;
      if TJ.DecompressHeader2(jpeg, jpegData, jpegDataLength, @w, @h, @jpegSubSamp) <> 0 then
         RaiseLastTurboJPEGError(jpeg);

      var flags := 0;
      if downscale > 1 then begin
         flags := TJFLAG_FASTUPSAMPLE or TJFLAG_FASTDCT;
         w := w div downscale;
         h := h div downscale;
         if w < 1 then w := 1;
         if h < 1 then h := 1;
      end;
//
      SetLength(Result, w*h*4);
      if TJ.Decompress2(jpeg, jpegData, jpegDataLength, PByte(Result), w, 0, h, TJPF_RGBA, flags) <> 0 then
         RaiseLastTurboJPEGError(jpeg);
   finally
      TJ.Destroy(jpeg);
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

finalization

  UnloadTurboJPEG;

end.
