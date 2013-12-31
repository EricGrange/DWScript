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
   SynGdiPlus,
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

// RegisterGraphicsTypes
//
procedure RegisterGraphicsTypes(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                                unitTable : TSymbolTable);
var
   typPixmap : TDynamicArraySymbol;
   jpgOption : TEnumerationSymbol;
   jpgOptions : TSetOfSymbol;
begin
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
type
   TRGB24 = record r, g, b : Byte; end;
   PRGB24 = ^TRGB24;
var
   w, h, quality : Integer;
   dynArray : TScriptDynamicArray;
   jpegOptions : TJPEGOptions;

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
            c:=dynArray.AsInteger[i];
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
   pixmap : IScriptObj;
   opts : Integer;
   jpegOption : TJPEGOption;
begin
   args.ExprBase[0].EvalAsScriptObj(args.Exec, pixmap);
   dynArray:=(pixmap.GetSelf as TScriptDynamicArray);
   w:=args.AsInteger[1];
   h:=args.AsInteger[2];
   if w*h>dynArray.DataLength then
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
// ------------------ TPixmapToPNGDataFunc ------------------
// ------------------

// DoEvalAsString
//
procedure TPixmapToPNGDataFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString);
type
   TRGB24 = record r, g, b : Byte; end;
   PRGB24 = ^TRGB24;
var
   w, h : Integer;
   dynArray : TScriptDynamicArray;

   procedure CompressWithGDIPlus(var Result : UnicodeString);
   var
      i, x, y, c : Integer;
      bmp : TBitmap;
      scanLine : PRGB24;
      buf : TWriteOnlyBlockStream;
   begin
      if Gdip=nil then
         Gdip := TGDIPlus.Create('gdiplus.dll');
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
               scanLine^.r:=PRGB24(@c)^.b;
               scanLine^.g:=PRGB24(@c)^.g;
               scanLine^.b:=PRGB24(@c)^.r;
               Inc(i);
               Inc(scanLine);
            end;
         end;
         buf:=TWriteOnlyBlockStream.AllocFromPool;
         try
            SaveAs(bmp, buf, gptPNG);
            Result:=RawByteStringToScriptString(buf.ToRawBytes);
         finally
            buf.ReturnToPool;
         end;
      finally
         bmp.Free;
      end;
   end;

var
   pixmap : IScriptObj;
begin
   args.ExprBase[0].EvalAsScriptObj(args.Exec, pixmap);
   dynArray:=(pixmap.GetSelf as TScriptDynamicArray);
   w:=args.AsInteger[1];
   h:=args.AsInteger[2];
   if w*h>dynArray.DataLength then
      raise Exception.Create('Not enough data in pixmap');
   CompressWithGDIPlus(Result);
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
   RegisterInternalStringFunction(TPixmapToPNGDataFunc, 'PixmapToPNGData',
         ['pixmap', SYS_PIXMAP, 'width', SYS_INTEGER, 'height', SYS_INTEGER], []);

end.

