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
unit dwsGraphicLibrary;

{$I dws.inc}

{$define USE_LIB_JPEG}

interface

uses
   Classes, SysUtils, Graphics,
   {$ifdef USE_LIB_JPEG}
   dwsJPEGEncoder,
   {$else}
   JPEG,
   {$endif}
   PNGImage,
   dwsXPlatform, dwsUtils, dwsStrings,
   dwsFunctions, dwsSymbols, dwsExprs, dwsCoreExprs, dwsExprList, dwsUnitSymbols,
   dwsConstExprs, dwsMagicExprs, dwsDataContext;

const
   SYS_PIXMAP = 'TPixmap';
   SYS_TJPEGOption = 'TJPEGOption';
   SYS_TJPEGOptions = 'TJPEGOptions';

type

   TPixmapToJPEGDataFunc = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString); override;
   end;

   TPNGDataToPixmapFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;
   TPixmapToPNGDataFunc = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   TRGB24 = record r, g, b : Byte; end;
   PRGB24 = ^TRGB24;

   TRGB32 = record r, g, b, a : Byte; end;
   PRGB32 = ^TRGB32;


// RegisterGraphicsTypes
//
procedure RegisterGraphicsTypes(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                                unitTable : TSymbolTable);
var
   typPixmap : TDynamicArraySymbol;
   jpgOption : TEnumerationSymbol;
   jpgOptions : TSetOfSymbol;
begin
   if unitTable.FindLocal(SYS_PIXMAP)<>nil then exit;

   typPixmap:=TDynamicArraySymbol.Create(SYS_PIXMAP, systemTable.TypInteger, systemTable.TypInteger);
   unitTable.AddSymbol(typPixmap);

   jpgOption:=TEnumerationSymbol.Create(SYS_TJPEGOption, systemTable.TypInteger, enumScoped);
   unitTable.AddSymbol(jpgOption);
   jpgOption.AddElement(TElementSymbol.Create('Optimize', jpgOption, Ord(jpgoOptimize), False));
   jpgOption.AddElement(TElementSymbol.Create('NoJFIFHeader', jpgOption, Ord(jpgoNoJFIFHeader), False));
   jpgOption.AddElement(TElementSymbol.Create('Progressive', jpgOption, Ord(jpgoProgressive), False));

   jpgOptions:=TSetOfSymbol.Create(SYS_TJPEGOptions, jpgOption, jpgOption.LowBound, jpgOption.HighBound);
   unitTable.AddSymbol(jpgOptions);
end;

// ------------------
// ------------------ TPixmapToJPEGDataFunc ------------------
// ------------------

// DoEvalAsString
//
procedure TPixmapToJPEGDataFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString);
var
   w, h, quality : Integer;
   jpegOptions : TJPEGOptions;
   pixmap : IScriptDynArray;

   {$ifdef USE_LIB_JPEG}
   procedure CompressWithLibJPEG(var Result : UnicodeString);
   var
      buf : array of TRGB24;
      i, x, y, c : Integer;
      outBuf : RawByteString;
   begin
      SetLength(buf, w*h*3);
      i:=0;
      for y:=0 to h-1 do begin
         for x:=0 to w-1 do begin
            c:=pixmap.AsInteger[i];
            buf[i]:=PRGB24(@c)^;
            Inc(i);
         end;
      end;
      outBuf:=CompressJPEG(Pointer(buf), w, h, quality, jpegOptions);
      SetLength(buf, 0);
      Result:=RawByteStringToScriptString(outBuf);
   end;
   {$else}
   procedure CompressWithTJPEGImage(var Result : UnicodeString);
   var
      i, x, y, c : Integer;
      bmp : TBitmap;
      jpg : TJPEGImage;
      scanLine : PRGB24;
      buf : TWriteOnlyBlockStream;
   begin
      bmp:=TBitmap.Create;
      try
         bmp.PixelFormat:=pf24bit;
         bmp.Width:=w;
         bmp.Height:=h;
         i:=0;
         for y:=0 to h-1 do begin
            scanLine:=bmp.ScanLine[y];
            for x:=0 to w-1 do begin
               c:=dynArray.AsInteger[i];
               scanLine^:=PRGB24(@c)^;
               Inc(i);
               Inc(scanLine);
            end;
         end;
         jpg:=TJPEGImage.Create;
         buf:=TWriteOnlyBlockStream.AllocFromPool;
         try
            jpg.Assign(bmp);
            jpg.CompressionQuality:=quality;
            if jpgoOp
            jpg.SaveToStream(buf);
            Result:=RawByteStringToScriptString(buf.ToRawBytes);
         finally
            buf.ReturnToPool;
            jpg.Free;
         end;
      finally
         bmp.Free;
      end;
   end;
   {$endif}

var
   opts : Integer;
   jpegOption : TJPEGOption;
begin
   args.ExprBase[0].EvalAsScriptDynArray(args.Exec, pixmap);
   w:=args.AsInteger[1];
   h:=args.AsInteger[2];
   if w*h>pixmap.ArrayLength then
      raise Exception.Create('Not enough data in pixmap');
   quality:=args.AsInteger[3];
   opts:=StrToIntDef(args.AsString[4], 0);
   jpegOptions:=[];
   for jpegOption:=Low(TJPEGOption) to High(TJPEGOption) do
      if (opts and (1 shl Ord(jpegOption)))<>0 then
         Include(jpegOptions, jpegOption);
   {$ifdef USE_LIB_JPEG}
   CompressWithLibJPEG(Result);
   {$else}
   CompressWithTJPEGImage(Result);
   {$endif}
end;

// ------------------
// ------------------ TPNGDataToPixmapFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure TPNGDataToPixmapFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);

   procedure CopyRGBtoBGR(pixmap : PVarData; pngScan : PRGB24); inline;
   begin
      pixmap.VType:=varInt64;
      PRGB24(@pixmap.VInt64).r:=pngScan.b;
      PRGB24(@pixmap.VInt64).g:=pngScan.g;
      PRGB24(@pixmap.VInt64).b:=pngScan.r;
   end;

   procedure CopyRowDefault(pixmap : PVarData; png : TPngImage; y : Integer);
   var
      x : Integer;
   begin
      for x:=0 to png.Width-1 do begin
         pixmap.VType:=varInt64;
         pixmap.VInt64:=png.Pixels[x, y];
         PByte(@pixmap.VInt64)[3]:=255;
         Inc(pixmap);
      end;
   end;

   procedure CopyRowDefaultBitAlpha(pixmap : PVarData; png : TPngImage; y : Integer);
   var
      x : Integer;
      col, trCol : TColor;
   begin
      trCol:=png.TransparentColor;
      for x:=0 to png.Width-1 do begin
         pixmap.VType:=varInt64;
         col:=png.Pixels[x, y];
         pixmap.VInt64:=col;
         if col=trCol then
            PByte(@pixmap.VInt64)[3]:=0
         else PByte(@pixmap.VInt64)[3]:=255;
         Inc(pixmap);
      end;
   end;

   procedure CopyRowPalettedPartialAlpha(pixmap : PVarData; png : TPngImage; y : Integer);
   var
      x : Integer;
      alphaScan : PByteArray;
      trns : TChunktRNS;
   begin
      trns:=png.Chunks.ItemFromClass(TChunkTRNS) as TChunktRNS;
      alphaScan:=png.AlphaScanline[y];
      if alphaScan=nil then
         alphaScan:=png.Scanline[y];
      for x:=0 to png.Width-1 do begin
         pixmap.VType:=varInt64;
         pixmap.VInt64:=png.Pixels[x, y];
         PByte(@pixmap.VInt64)[3]:=trns.PaletteValues[alphaScan[x]];
         Inc(pixmap);
      end;
   end;

   procedure CopyRow24(pixmap : PVarData; pngScan : PRGB24; n : Integer);
   var
      x : Integer;
   begin
      for x:=1 to n do begin
         CopyRGBtoBGR(pixmap, pngScan);
         PByte(@pixmap.VInt64)[3]:=255;
         Inc(pixmap);
         Inc(pngScan);
      end;
   end;

   procedure CopyRow32(pixmap : PVarData; pngScan : PRGB24; alphaScan : PByte; n : Integer);
   var
      x : Integer;
   begin
      for x:=0 to n-1 do begin
         CopyRGBtoBGR(pixmap, pngScan);
         PByte(@pixmap^.VInt64)[3]:=alphaScan[x];
         Inc(pixmap);
         Inc(pngScan);
      end;
   end;

var
   data : RawByteString;
   stream : TMemoryStream;
   png : TPngImage;
   pixmapData : TData;
   pixmap : IScriptDynArray;
   y, w, h : Integer;
   withAlpha : Boolean;
begin
   data:=args.AsDataString[0];
   stream:=TMemoryStream.Create;
   try
      stream.Write(Pointer(data)^, Length(data));
      stream.Position:=0;
      data:='';
      png:=TPngImage.Create;
      try
         png.LoadFromStream(stream);
         stream.Clear;
         w:=png.Width;
         h:=png.Height;
         SetLength(pixmapData, w*h);
         withAlpha:=args.AsBoolean[1];
         case png.Header.ColorType of
            COLOR_RGBALPHA : begin
               if withAlpha then begin
                  png.CreateAlpha;
                  for y:=0 to h-1 do
                     CopyRow32(@pixmapData[y*w], png.Scanline[y], PByte(png.AlphaScanline[y]), w);
               end else begin
                  for y:=0 to h-1 do
                     CopyRow24(@pixmapData[y*w], png.Scanline[y], w);
               end;
            end;
         else
            if withAlpha and (png.TransparencyMode<>ptmNone) then begin
               if png.TransparencyMode=ptmBit then begin
                  for y:=0 to h-1 do
                     CopyRowDefaultBitAlpha(@pixmapData[y*w], png, y);
               end else begin
                  for y:=0 to h-1 do
                     CopyRowPalettedPartialAlpha(@pixmapData[y*w], png, y);
               end;
            end else begin
               for y:=0 to h-1 do
                  CopyRowDefault(@pixmapData[y*w], png, y);
            end;
         end;

         pixmap:=TScriptDynamicArray.CreateNew((args.Exec as TdwsProgramExecution).CompilerContext.TypInteger);
         pixmap.ReplaceData(pixmapData);
      finally
         png.Free;
      end;
   finally
      stream.Free;
   end;
   args.AsInteger[2]:=w;
   args.AsInteger[3]:=h;
   result:=pixmap;
end;

// ------------------
// ------------------ TPixmapToPNGDataFunc ------------------
// ------------------

// DoEvalAsString
//
procedure TPixmapToPNGDataFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString);
var
   w, h : Integer;
   pixmap : IScriptDynArray;

   procedure CompressWithPNGImage(var Result : UnicodeString; withAlpha : Boolean);
   var
      i, x, y : Integer;
      bmp : TBitmap;
      scanLine24 : PRGB24;
      c : TRGB32;
      alphaChannel : array of Byte;
      alphaPtr : PByte;
      buf : TWriteOnlyBlockStream;
      png : TPngImage;
   begin
      buf:=TWriteOnlyBlockStream.AllocFromPool;
      try
         png:=TPngImage.Create;
         try
            bmp:=TBitmap.Create;
            try
               bmp.PixelFormat:=pf24bit;
               bmp.Width:=w;
               bmp.Height:=h;
               if withAlpha then
                  SetLength(alphaChannel, w*h);
               alphaPtr:=PByte(alphaChannel);
               i:=0;
               for y:=0 to h-1 do begin
                  scanLine24:=bmp.ScanLine[y];
                  for x:=0 to w-1 do begin
                     PInteger(@c)^:=pixmap.AsInteger[i];
                     scanLine24^.r:=c.b;
                     scanLine24^.g:=c.g;
                     scanLine24^.b:=c.r;
                     Inc(i);
                     Inc(scanLine24);
                     if withAlpha then begin
                        alphaPtr^:=c.a;
                        Inc(alphaPtr);
                     end;
                  end;
               end;
               png.Assign(bmp);
            finally
               bmp.Free;
            end;
            if withAlpha then begin
               png.CreateAlpha;
               for y:=0 to h-1 do
                  System.Move(alphaChannel[y*w], png.AlphaScanline[y]^, w);
            end;
            png.SaveToStream(buf);
         finally
            png.Free;
         end;
         Result:=RawByteStringToScriptString(buf.ToRawBytes);
      finally
         buf.ReturnToPool;
      end;
   end;

begin
   args.ExprBase[0].EvalAsScriptDynArray(args.Exec, pixmap);
   w:=args.AsInteger[1];
   h:=args.AsInteger[2];
   if w*h>pixmap.ArrayLength then
      raise Exception.Create('Not enough data in pixmap');
   CompressWithPNGImage(Result, args.AsBoolean[3]);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   dwsInternalUnit.AddSymbolsRegistrationProc(RegisterGraphicsTypes);

   RegisterInternalStringFunction(TPixmapToJPEGDataFunc, 'PixmapToJPEGData',
         ['pixmap', SYS_PIXMAP, 'width', SYS_INTEGER, 'height', SYS_INTEGER,
          'quality=90', SYS_INTEGER, 'options=', SYS_TJPEGOptions], []);

   RegisterInternalFunction(TPNGDataToPixmapFunc, 'PNGDataToPixmap',
         ['pngData', SYS_STRING, 'withAlpha=False', SYS_BOOLEAN, '@width', SYS_INTEGER, '@height', SYS_INTEGER], SYS_PIXMAP, []);
   RegisterInternalStringFunction(TPixmapToPNGDataFunc, 'PixmapToPNGData',
         ['pixmap', SYS_PIXMAP, 'width', SYS_INTEGER, 'height', SYS_INTEGER, 'withAlpha=False', SYS_BOOLEAN], []);

end.

