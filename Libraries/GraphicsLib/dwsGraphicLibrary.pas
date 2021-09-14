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

{$ifdef WIN32}
   {$define USE_LIB_JPEG}
{$endif}
{$ifdef WIN64}
   {$define USE_LIB_JPEG}
{$endif}

{.$undef USE_LIB_JPEG}

interface

uses
   Classes, SysUtils, Vcl.Graphics,
   {$ifdef USE_LIB_JPEG}
   dwsTurboJPEG,
   {$else}
   VCL.Imaging.JPEG,
   {$endif}
   VCL.Imaging.PNGImage,
   dwsJPEGEncoderOptions,
   dwsXPlatform, dwsUtils, dwsStrings,
   dwsFunctions, dwsSymbols, dwsExprs, dwsCoreExprs, dwsExprList, dwsUnitSymbols,
   dwsConstExprs, dwsMagicExprs, dwsDataContext, dwsByteBufferFunctions;

const
   SYS_PIXMAP = 'TPixmap';
   SYS_TRGBAHISTOGRAM = 'TRGBAHistogram';
   SYS_TJPEGOption = 'TJPEGOption';
   SYS_TJPEGOptions = 'TJPEGOptions';

   cMaxPixmapWidth = 16384;
   cMaxPixmapHeight = 16384;

type

   TBasePixmapSymbol = class (TBaseByteBufferSymbol)
      public
         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
   end;

   EdwsPixmap = class (Exception);

   TCreatePixmapFunc = class(TInternalMagicInterfaceFunction)
      procedure DoEvalAsInterface(const args : TExprBaseListExec; var result : IUnknown); override;
   end;

   TPixmapToJPEGDataFunc = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString); override;
   end;
   TJPEGDataToPixmapFunc = class(TInternalMagicInterfaceFunction)
      procedure DoEvalAsInterface(const args : TExprBaseListExec; var result : IUnknown); override;
   end;

   TPNGDataToPixmapFunc = class(TInternalMagicInterfaceFunction)
      procedure DoEvalAsInterface(const args : TExprBaseListExec; var result : IUnknown); override;
   end;
   TPixmapToPNGDataFunc = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString); override;
   end;

   TPixmapSetPixelFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;
   TPixmapGetPixelFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;
   TPixmapSetDataFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;
   TPixmapGetDataFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

(*
   TPixmapHistogramFunc = class(TInternalMagicInterfaceFunction)
      procedure DoEvalAsInterface(const args : TExprBaseListExec; var result : IUnknown); override;
   end;
*)
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsByteBuffer;

type
   TRGB24 = record r, g, b : Byte; end;
   PRGB24 = ^TRGB24;

   TRGB32 = record r, g, b, a : Byte; end;
   PRGB32 = ^TRGB32;

// RegisterGraphicsTypes
//
procedure RegisterGraphicsTypes(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                                unitTable : TSymbolTable);
begin
   if systemTable.FindLocal(SYS_PIXMAP) <> nil then Exit;

   var typePixmap := TBasePixmapSymbol.Create(SYS_PIXMAP);
   systemTable.AddSymbol(typePixmap);

(*   var typRGBAHistogram := TClassSymbol.Create(SYS_TRGBAHISTOGRAM, nil);
   typRGBAHistogram.AddField(TFieldSymbol.Create('R', TDynamicArraySymbol.Create('', systemTable.TypInteger, systemTable.TypInteger), cvPublic));
   typRGBAHistogram.AddField(TFieldSymbol.Create('G', TDynamicArraySymbol.Create('', systemTable.TypInteger, systemTable.TypInteger), cvPublic));
   typRGBAHistogram.AddField(TFieldSymbol.Create('B', TDynamicArraySymbol.Create('', systemTable.TypInteger, systemTable.TypInteger), cvPublic));
   typRGBAHistogram.AddField(TFieldSymbol.Create('A', TDynamicArraySymbol.Create('', systemTable.TypInteger, systemTable.TypInteger), cvPublic));
   unitTable.AddSymbol(typRGBAHistogram);*)

   var jpgOption := TEnumerationSymbol.Create(SYS_TJPEGOption, systemTable.TypInteger, enumScoped);
   systemTable.AddSymbol(jpgOption);
   jpgOption.AddElement(TElementSymbol.Create('Optimize', jpgOption, Ord(jpgoOptimize), False));
   jpgOption.AddElement(TElementSymbol.Create('NoJFIFHeader', jpgOption, Ord(jpgoNoJFIFHeader), False));
   jpgOption.AddElement(TElementSymbol.Create('Progressive', jpgOption, Ord(jpgoProgressive), False));

   var jpgOptions := TSetOfSymbol.Create(SYS_TJPEGOptions, jpgOption, jpgOption.LowBound, jpgOption.HighBound);
   systemTable.AddSymbol(jpgOptions);
end;

// ByteBufferToJPEGData
//
procedure ByteBufferToJPEGData(const buffer : IdwsByteBuffer; w, h : Integer;
                               const jpegOptions : TJPEGOptions; quality : Integer;
                               var Result : RawByteString);
{$ifdef USE_LIB_JPEG}
begin
   if w*h*4 > buffer.GetCount then
      raise EdwsPixmap.Create('Not enough data in pixmap');
   Result := CompressJPEG(buffer.DataPtr, w, h, quality, jpegOptions);
end;
{$else}
var
   i, x, y, c : Integer;
   bmp : TBitmap;
   jpg : TJPEGImage;
   scanLine : PRGB24;
   buf : TWriteOnlyBlockStream;
begin
   if w*h*4 > buffer.GetCount then
   raise EdwsPixmap.Create('Not enough data in pixmap');
   bmp:=TBitmap.Create;
   try
      bmp.PixelFormat:=pf24bit;
      bmp.Width:=w;
      bmp.Height:=h;
      i:=0;
      for y:=0 to h-1 do begin
         scanLine:=bmp.ScanLine[y];
         for x:=0 to w-1 do begin
            c:=buffer.GetDWordA(i);
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
         jpg.SaveToStream(buf);
         Result := buf.ToRawBytes;
      finally
         buf.ReturnToPool;
         jpg.Free;
      end;
   finally
      bmp.Free;
   end;
end;
{$endif}

// CheckWidthHeight
//
procedure CheckWidthHeight(w, h : Integer);
begin
   if NativeUInt(w) > cMaxPixmapWidth then
      raise EdwsPixmap.CreateFmt('Invalid width (%d)', [ w ]);
   if NativeUInt(h) > cMaxPixmapHeight then
      raise EdwsPixmap.CreateFmt('Invalid height (%d)', [ h ]);
end;

// CreateTPixmap
//
function CreateTPixmap(w, h : Integer) : TdwsByteBuffer;
begin
   CheckWidthHeight(w, h);

   var pixmap := TdwsByteBuffer.Create;
   Result := pixmap;
   pixmap.Count := w*h*4;
   pixmap.Meta[0] := w;
end;

// ------------------
// ------------------ TBasePixmapSymbol ------------------
// ------------------

// IsCompatible
//
function TBasePixmapSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   Result:=(typSym<>nil) and (typSym.UnAliasedType.ClassType = TBasePixmapSymbol);
end;

// ------------------
// ------------------ TCreatePixmapFunc ------------------
// ------------------

// DoEvalAsInterface
//
procedure TCreatePixmapFunc.DoEvalAsInterface(const args : TExprBaseListExec; var result : IUnknown);
begin
   result := CreateTPixmap(args.AsInteger[0], args.AsInteger[1]) as IdwsByteBuffer;
end;

// ------------------
// ------------------ TPixmapToJPEGDataFunc ------------------
// ------------------

// DoEvalAsString
//
procedure TPixmapToJPEGDataFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString);
var
   w, h, quality : Integer;
   pixmap : IdwsByteBuffer;
   opts : Integer;
   jpegOptions : TJPEGOptions;
   jpegOption : TJPEGOption;
   outBuf : RawByteString;
begin
   args.ExprBase[0].EvalAsInterface(args.Exec, IUnknown(pixmap));
   w := args.AsInteger[1];
   h := args.AsInteger[2];
   if w*h*4 > pixmap.GetCount then
      raise EdwsPixmap.Create('Not enough data in pixmap');
   quality:=args.AsInteger[3];
   opts:=StrToIntDef(args.AsString[4], 0);
   jpegOptions:=[];
   for jpegOption:=Low(TJPEGOption) to High(TJPEGOption) do
      if (opts and (1 shl Ord(jpegOption)))<>0 then
         Include(jpegOptions, jpegOption);
   ByteBufferToJPEGData(pixmap, w, h, jpegOptions, quality, outBuf);
   RawByteStringToScriptString(outBuf, Result);
end;

{$ifdef USE_LIB_JPEG}

// DoEvalAsInterface
//
procedure TJPEGDataToPixmapFunc.DoEvalAsInterface(const args : TExprBaseListExec; var result : IUnknown);
var
   w, h : Integer;
begin
   var data := args.AsDataString[0];
   var downscale := args.AsInteger[1];

   var pixmapData : TBytes := DecompressJPEG_RGBA(Pointer(data), Length(data), downscale, w, h);

   var pixmap := TdwsByteBuffer.Create;
   Result := pixmap as IdwsByteBuffer;

   pixmap.AssignRaw(Pointer(pixmapData), Length(pixmapData));

   args.AsInteger[2] := w;
   args.AsInteger[3] := h;
end;

{$else}

type TJPEGImageCracker = class(TJPEGImage);

// DoEvalAsInterface
//
procedure TJPEGDataToPixmapFunc.DoEvalAsInterface(const args : TExprBaseListExec; var result : IUnknown);

   procedure CopyRowDefault(pixmapRow : PCardinal; j : TJPEGImageCracker; y : Integer);
   begin
      var scanLine : PRGB24 := j.Bitmap.ScanLine[y];
      for var x :=0 to j.Width-1 do begin
         pixmapRow^ := $ff000000 or (scanLine.r shl 16)
                                 or (scanLine.g shl 8)
                                 or (scanLine.b);
         Inc(pixmapRow);
         Inc(scanLine);
      end;
   end;

var
   data : RawByteString;
   stream : TMemoryStream;
   j : TJPEGImageCracker;
   pixmap : TdwsByteBuffer;
   y, w, h, downscale : Integer;
begin
   data := args.AsDataString[0];
   downscale := args.AsInteger[1];
   stream := TMemoryStream.Create;
   try
      stream.Write(Pointer(data)^, Length(data));
      stream.Position := 0;
      data := '';
      j := TJPEGImageCracker.Create;
      try
         j.PixelFormat := jf24Bit;
         if downscale > 0 then begin
            j.Performance := jpBestSpeed;
            case downscale of
               1 : j.Scale := jsFullSize;
               2, 3 : j.Scale := jsHalf;
               4..7 : j.Scale := jsQuarter;
            else
               j.Scale := jsEighth;
            end;
         end;
         j.LoadFromStream(stream);
         stream.Clear;
         w := j.Width;
         h := j.Height;

         pixmap := CreateTPixmap(w, h);
         Result := pixmap as IdwsByteBuffer;

         pixmap.Count := w*h*4;

         for y := 0 to h-1 do
            CopyRowDefault(@PByteArray(pixmap.DataPtr)[y*w*4], j, y);
      finally
         j.Free;
      end;
   finally
      stream.Free;
   end;
   args.AsInteger[2]:=w;
   args.AsInteger[3]:=h;
end;

{$endif}

// ------------------
// ------------------ TPNGDataToPixmapFunc ------------------
// ------------------

// DoEvalAsInterface
//
procedure TPNGDataToPixmapFunc.DoEvalAsInterface(const args : TExprBaseListExec; var result : IUnknown);

   procedure CopyRGBtoBGR(dest : PRGB32; src : PRGB24); inline;
   begin
      dest.r := src.b;
      dest.g := src.g;
      dest.b := src.r;
   end;

   procedure CopyRowDefault(dest : PRGB32; png : TPngImage; y : Integer);
   begin
      for var x := 0 to png.Width-1 do begin
         PCardinal(dest)^ := Cardinal(png.Pixels[x, y]) or $FF000000;
         Inc(dest);
      end;
   end;

   procedure CopyRowDefaultBitAlpha(dest : PRGB32; png : TPngImage; y : Integer);
   begin
      var trCol := Cardinal(png.TransparentColor);
      for var x := 0 to png.Width-1 do begin
         var col := Cardinal(png.Pixels[x, y]);
         if col = trCol then
            PCardinal(dest)^ := col and $FFFFFF
         else PCardinal(dest)^ := col or $FF000000;
         Inc(dest);
      end;
   end;

   procedure CopyRowPalettedPartialAlpha(dest : PRGB32; png : TPngImage; y : Integer);
   begin
      var trns := png.Chunks.ItemFromClass(TChunkTRNS) as TChunktRNS;
      var alphaScan : PByteArray := png.AlphaScanline[y];
      if alphaScan = nil then
         alphaScan := png.Scanline[y];
      for var x := 0 to png.Width-1 do begin
         PCardinal(dest)^ := png.Pixels[x, y];
         dest.a := trns.PaletteValues[alphaScan[x]];
         Inc(dest);
      end;
   end;

   procedure CopyRow24(dest : PRGB32; pngScan : PRGB24; n : Integer);
   begin
      for var x := 1 to n do begin
         CopyRGBtoBGR(dest, pngScan);
         dest.a := 255;
         Inc(dest);
         Inc(pngScan);
      end;
   end;

   procedure CopyRow32(dest : PRGB32; pngScan : PRGB24; alphaScan : PByte; n : Integer);
   begin
      for var x := 0 to n-1 do begin
         CopyRGBtoBGR(dest, pngScan);
         dest.a := alphaScan[x];
         Inc(dest);
         Inc(pngScan);
      end;
   end;

var
   data : RawByteString;
   w, h : Integer;
begin
   data := args.AsDataString[0];
   var stream := TMemoryStream.Create;
   try
      stream.Write(Pointer(data)^, Length(data));
      stream.Position := 0;
      data := '';
      var png := TPngImage.Create;
      try
         png.LoadFromStream(stream);
         stream.Clear;
         w := png.Width;
         h := png.Height;
         var pixmap := CreateTPixmap(w, h);
         Result := IdwsByteBuffer(pixmap);
         var pixmapDataPtr := PByteArray(pixmap.DataPtr);
         var withAlpha := args.AsBoolean[1];
         case png.Header.ColorType of
            COLOR_RGBALPHA : begin
               if withAlpha then begin
                  png.CreateAlpha;
                  for var y := 0 to h-1 do
                     CopyRow32(@pixmapDataPtr[y*w], png.Scanline[y], PByte(png.AlphaScanline[y]), w);
               end else begin
                  for var y := 0 to h-1 do
                     CopyRow24(@pixmapDataPtr[y*w], png.Scanline[y], w);
               end;
            end;
         else
            if withAlpha and (png.TransparencyMode<>ptmNone) then begin
               if png.TransparencyMode=ptmBit then begin
                  for var y := 0 to h-1 do
                     CopyRowDefaultBitAlpha(@pixmapDataPtr[y*w], png, y);
               end else begin
                  for var y := 0 to h-1 do
                     CopyRowPalettedPartialAlpha(@pixmapDataPtr[y*w], png, y);
               end;
            end else begin
               for var y := 0 to h-1 do
                  CopyRowDefault(@pixmapDataPtr[y*w], png, y);
            end;
         end;
      finally
         png.Free;
      end;
   finally
      stream.Free;
   end;
   args.AsInteger[2] := w;
   args.AsInteger[3] := h;
end;

// ------------------
// ------------------ TPixmapToPNGDataFunc ------------------
// ------------------

// DoEvalAsString
//
procedure TPixmapToPNGDataFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString);
var
   w, h : Integer;
   pixmap : IdwsByteBuffer;

   procedure CompressWithPNGImage(var Result : UnicodeString; withAlpha : Boolean);
   var
      x, y : Integer;
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
               var pSrc : PCardinal := pixmap.DataPtr;
               for y:=0 to h-1 do begin
                  scanLine24:=bmp.ScanLine[y];
                  for x:=0 to w-1 do begin
                     PCardinal(@c)^ := pSrc^;
                     scanLine24^.r:=c.b;
                     scanLine24^.g:=c.g;
                     scanLine24^.b:=c.r;
                     Inc(pSrc);
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
   args.ExprBase[0].EvalAsInterface(args.Exec, IUnknown(pixmap));
   w := args.AsInteger[1];
   h := args.AsInteger[2];
   if w*h*4 > pixmap.GetCount then
      raise EdwsPixmap.Create('Not enough data in pixmap');
   CompressWithPNGImage(Result, args.AsBoolean[3]);
end;

// ------------------
// ------------------ TPixmapSetPixelFunc ------------------
// ------------------

// DoEvalProc
//
procedure TPixmapSetPixelFunc.DoEvalProc(const args : TExprBaseListExec);
var
   x, y, w : Integer;
   pixmap : IdwsByteBuffer;
begin
   args.ExprBase[0].EvalAsInterface(args.Exec, IUnknown(pixmap));
   x := args.AsInteger[1];
   y := args.AsInteger[2];
   w := pixmap.GetMeta(0);
   if Cardinal(x) > Cardinal(w) then
      raise EdwsPixmap.CreateFmt('SetPixel x out of bounds (%d)', [ x ]);
   if Cardinal(y*w) > Cardinal(pixmap.GetCount) then
      raise EdwsPixmap.CreateFmt('SetPixel y out of bounds (%d)', [ y ]);
   pixmap.SetInt32A((x + y*w) * 4, args.AsInteger[3]);
end;

// ------------------
// ------------------ TPixmapGetPixelFunc ------------------
// ------------------

// DoEvalAsInteger
//
function TPixmapGetPixelFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   x, y, w : Integer;
   pixmap : IdwsByteBuffer;
begin
   args.ExprBase[0].EvalAsInterface(args.Exec, IUnknown(pixmap));
   x := args.AsInteger[1];
   y := args.AsInteger[2];
   w := pixmap.GetMeta(0);
   if Cardinal(x) > Cardinal(w) then
      raise EdwsPixmap.CreateFmt('SetPixel x out of bounds (%d)', [ x ]);
   if Cardinal(y*w) > Cardinal(pixmap.GetCount) then
      raise EdwsPixmap.CreateFmt('SetPixel y out of bounds (%d)', [ y ]);
   Result := pixmap.GetInt32A((x + y*w) * 4);
end;

// ------------------
// ------------------ TPixmapSetDataFunc ------------------
// ------------------

// DoEvalProc
//
procedure TPixmapSetDataFunc.DoEvalProc(const args : TExprBaseListExec);
var
   offset : NativeInt;
   pixmap : IdwsByteBuffer;
begin
   args.ExprBase[0].EvalAsInterface(args.Exec, IUnknown(pixmap));
   offset := args.AsInteger[1] shl 2;
   if NativeUInt(offset) >= NativeUInt(pixmap.GetCount) then
      raise EdwsPixmap.CreateFmt('SetData out of bounds (%d)', [ offset shr 2 ]);
   pixmap.SetInt32A(offset, args.AsInteger[2]);
end;

// ------------------
// ------------------ TPixmapGetDataFunc ------------------
// ------------------

// DoEvalAsInteger
//
function TPixmapGetDataFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
var
   offset : NativeInt;
   pixmap : IdwsByteBuffer;
begin
   args.ExprBase[0].EvalAsInterface(args.Exec, IUnknown(pixmap));
   offset := args.AsInteger[1] shl 2;
   if NativeUInt(offset) >= NativeUInt(pixmap.GetCount) then
      raise EdwsPixmap.CreateFmt('SetData out of bounds (%d)', [ offset shr 2 ]);
   Result := pixmap.GetInt32A(offset);
end;

// ------------------
// ------------------ TPixmapHistogramFunc ------------------
// ------------------

// DoEvalAsInterface
(*
procedure TPixmapHistogramFunc.DoEvalAsInterface(const args : TExprBaseListExec; var result : IUnknown);
var
   pixmap : IdwsByteBuffer;
begin
   args.ExprBase[0].EvalAsInterface(args.Exec, IUnknown(pixmap));
   var offset := args.AsInteger[1];
   var width := args.AsInteger[2];
   var stride := args.AsInteger[3];
   var count := args.AsInteger[4];
//   ...
end;
*)
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   dwsInternalUnit.AddSymbolsRegistrationProc(RegisterGraphicsTypes);

   RegisterInternalFunction(TCreatePixmapFunc, 'CreatePixmap',
         ['width', SYS_INTEGER, 'height', SYS_INTEGER], SYS_PIXMAP, []);

   RegisterInternalFunction(TJPEGDataToPixmapFunc, 'JPEGDataToPixmap',
         ['jpegData', SYS_STRING, 'downscale', SYS_INTEGER, '@width', SYS_INTEGER, '@height', SYS_INTEGER], SYS_PIXMAP, []);
   RegisterInternalStringFunction(TPixmapToJPEGDataFunc, 'PixmapToJPEGData',
         ['pixmap', SYS_PIXMAP, 'width', SYS_INTEGER, 'height', SYS_INTEGER,
          'quality=90', SYS_INTEGER, 'options=', SYS_TJPEGOptions], []);

   RegisterInternalFunction(TPNGDataToPixmapFunc, 'PNGDataToPixmap',
         ['pngData', SYS_STRING, 'withAlpha=False', SYS_BOOLEAN, '@width', SYS_INTEGER, '@height', SYS_INTEGER], SYS_PIXMAP, []);
   RegisterInternalStringFunction(TPixmapToPNGDataFunc, 'PixmapToPNGData',
         ['pixmap', SYS_PIXMAP, 'width', SYS_INTEGER, 'height', SYS_INTEGER, 'withAlpha=False', SYS_BOOLEAN], []);

   RegisterInternalProcedure(TByteBufferAssignHexStringFunc, '', ['pixmap', SYS_PIXMAP, 'hexData', SYS_STRING], 'AssignHexString');
   RegisterInternalProcedure(TPixmapSetPixelFunc, '', ['pixmap', SYS_PIXMAP, 'x', SYS_INTEGER, 'y', SYS_INTEGER, 'color', SYS_INTEGER], 'SetPixel');
   RegisterInternalIntFunction(TPixmapGetPixelFunc, '', ['pixmap', SYS_PIXMAP, 'x', SYS_INTEGER, 'y', SYS_INTEGER], [], 'GetPixel');
   RegisterInternalProcedure(TPixmapSetDataFunc, '', ['pixmap', SYS_PIXMAP, 'offset', SYS_INTEGER, 'color', SYS_INTEGER], 'SetData');
   RegisterInternalIntFunction(TPixmapGetDataFunc, '', ['pixmap', SYS_PIXMAP, 'offset', SYS_INTEGER ], [], 'GetData');

//   RegisterInternalFunction(TPixmapHistogramFunc, 'PixmapHistogram',
//         ['pixmap', SYS_PIXMAP, 'offset', SYS_INTEGER, 'width', SYS_INTEGER, 'stride', SYS_INTEGER, 'count', SYS_INTEGER ], SYS_TRGBAHISTOGRAM, []);

finalization



end.

