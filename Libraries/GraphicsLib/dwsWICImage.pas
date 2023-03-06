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
unit dwsWICImage;

{$I dws.inc}

interface

uses
   System.Classes, System.SysUtils, System.Types,
   Winapi.Windows, Winapi.Wincodec, Winapi.ActiveX,
   Vcl.Graphics,
   dwsXPlatform;

type
   TdwsWICImageLoadOption = (
      iloApplyExifRotation    // automatically apply EXIF rotation when loading
   );
   TdwsWICImageLoadOptions = set of TdwsWICImageLoadOption;

const
   cDefaultWICImageLoadOptions = [ iloApplyExifRotation ];

type
   TWICImageWriteMetadataProc = reference to procedure (const writer : IWICMetadataQueryWriter);


   IdwsWICImage = interface
      ['{AC94EC11-5E2C-4343-B671-B2D4C743168E}']

      function WICBitmap : IWICBitmap;
      function WICMetadataQueryReader : IWICMetadataQueryReader;
      function Empty : Boolean;
      procedure Clear;

      function Width : Integer;
      function Height : Integer;
      function GetSize : TSize;
      property Size : TSize read GetSize;
      function GetPixelFormat : TGUID;
      procedure SetPixelFormat(const newFormat : TGUID);
      property PixelFormat : TGUID read GetPixelFormat write SetPixelFormat;
      function PixelFormatInfo : IWICPixelFormatInfo;
      function ContainerFormat : TGUID;
      function ExifOrientation : WICBitmapTransformOptions;

      function GetFileName : TFileName;
      procedure SetFileName(const aName : TFileName);
      property FileName : TFileName read GetFileName write SetFileName;

      function GetImageQuality : Single;
      procedure SetImageQuality(const val : Single);
      property ImageQuality : Single read GetImageQuality write SetImageQuality;

      procedure SetFromMemory(
         const aWidth, aHeight : Integer; const aPixelFormat : TGUID;
         const aStride, aBufferSize : Integer;
         const dataPtr : Pointer
      );

      procedure LoadFromFile(
         const aFileName : TFileName;
         const options : TdwsWICImageLoadOptions = cDefaultWICImageLoadOptions
      );
      procedure SaveToFile(
         const aFileName : TFileName;
         const aContainerFormat, aPixelFormat : TGUID;
         const onWriteMetadata : TWICImageWriteMetadataProc = nil
      );

      function CreateBitmap32(scale : Single = 1) : TBitmap;
      function CreateConverter(const dstFormat: WICPixelFormatGUID) : IWICFormatConverter;

   end;

   TdwsWICImage = class (TInterfacedObject, IdwsWICImage)
      private
         FWICBitmap : IWICBitmap;
         FWICFrame : IWICBitmapFrameDecode;
         FSize : TSize;
         FPixelFormat : TGUID;
         FContainerFormat : TGUID;
         FFileName : String;
         FImageQuality : Single;

      protected
         function GetFileName : TFileName;
         procedure SetFileName(const aName : TFileName);
         function GetImageQuality : Single;
         procedure SetImageQuality(const val : Single);

      public

         function WICBitmap : IWICBitmap;
         function WICMetadataQueryReader : IWICMetadataQueryReader;
         function Empty : Boolean;
         procedure Clear;

         function Width : Integer;
         function Height : Integer;
         function GetSize : TSize;
         function GetPixelFormat : TGUID;
         procedure SetPixelFormat(const newFormat : TGUID);
         function ContainerFormat : TGUID;
         function PixelFormatInfo : IWICPixelFormatInfo;
         function ExifOrientation : WICBitmapTransformOptions;

         property FileName : String read FFileName write FFileName;
         property ImageQuality : Single read FImageQuality write FImageQuality;

         procedure SetFromMemory(
            const aWidth, aHeight : Integer; const aPixelFormat : TGUID;
            const aStride, aBufferSize : Integer;
            const dataPtr : Pointer
         );

         procedure LoadFromFile(
            const aFileName : TFileName;
            const options : TdwsWICImageLoadOptions = cDefaultWICImageLoadOptions
         );
         procedure SaveToFile(
            const aFileName : TFileName;
            const aContainerFormat, aPixelFormat : TGUID;
            const onWriteMetadata : TWICImageWriteMetadataProc = nil
         );

         function CreateBitmap32(scale : Single = 1)  : TBitmap;

         function CreateConverter(const dstFormat: WICPixelFormatGUID) : IWICFormatConverter;

   end;

   EWICException = class (Exception);

   TCopyWICImageMetadataOption = (
      cimoRecurse,
      cimoReaderAlreadyPointsToSourcePath
   );
   TCopyWICImageMetadataOptions = set of TCopyWICImageMetadataOption;

procedure CopyWICImageMetadata(
   const sourcePath : String; const reader : IWICMetadataQueryReader;
   const destPath : String; const writer : IWICMetadataQueryWriter;
   options : TCopyWICImageMetadataOptions = [ ];
   const report : TStrings = nil
);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
   vWICImagingFactory : IWICImagingFactory;

// WICImagingFactory
//
function WICImagingFactory : IWICImagingFactory;

   procedure InitializeFactory;
   var
      factory : IWICImagingFactory;
   begin
      // minimize unnecessary creation and replacemet, but if it's happen it is not problematic
      CoInitialize(nil);
      CoCreateInstance(
         CLSID_WICImagingFactory, nil,
         CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER,
         IUnknown, factory
      );
      if vWICImagingFactory = nil then
         vWICImagingFactory := factory;
   end;

begin
   if not Assigned(vWICImagingFactory) then
      InitializeFactory;
   Result := vWICImagingFactory;
end;

// WicCheckFailed
//
procedure WicCheckFailed(Result: HRESULT; const context : String);
begin
   raise EWICException.CreateFmt('%s, WIC error %x', [ context, Result ]);
end;

// WicCheck
//
procedure WicCheck(Result: HRESULT; const context : String); inline;
begin
   if Failed(Result) then
      WicCheckFailed(Result, context);
end;

// EXIFOrientationToWICTransform
//
function EXIFOrientationToWICTransform(orientation : Integer) : WICBitmapTransformOptions;
const
   cEXIFOrientationToWICTransform : array[1..8] of WICBitmapTransformOptions = (
      WICBitmapTransformRotate0,
      WICBitmapTransformFlipHorizontal,
      WICBitmapTransformRotate180,
      WICBitmapTransformFlipVertical,
      WICBitmapTransformRotate90 and WICBitmapTransformFlipHorizontal,
      WICBitmapTransformRotate90,
      WICBitmapTransformRotate270 and WICBitmapTransformFlipHorizontal,
      WICBitmapTransformRotate270
  );
begin
   if orientation in [Low(cEXIFOrientationToWICTransform)..High(cEXIFOrientationToWICTransform)] then
      Result := cEXIFOrientationToWICTransform[orientation]
   else Result := WICBitmapTransformRotate0;
end;

// CopyWICImageMetadata
//
procedure CopyWICImageMetadata(
   const sourcePath : String; const reader : IWICMetadataQueryReader;
   const destPath : String; const writer : IWICMetadataQueryWriter;
   options : TCopyWICImageMetadataOptions = [ ];
   const report : TStrings = nil
);

   procedure AddToReport(const fmt : String; const args : array of const);
   begin
      if report <> nil then
         report.Add(Format(fmt, args));
   end;

var
   enumStrings : IEnumString;
   srcName, dstName : String;
begin
   var pv := Default(PROPVARIANT);
   if not (cimoReaderAlreadyPointsToSourcePath in options) then begin
      reader.GetMetadataByName(PChar(sourcePath), pv);
      try
         if pv.vt = VT_UNKNOWN then begin
            CopyWICImageMetadata(
               sourcePath, IUnknown(pv.ppunkVal) as IWICMetadataQueryReader,
               destPath, writer,
               options + [ cimoReaderAlreadyPointsToSourcePath ], report
            );
         end else AddToReport('Source path "%s" not a container', [ sourcePath ]);
      finally
         PropVariantClear(pv);
      end;
      Exit;
   end;

   WicCheck(reader.GetEnumerator(enumStrings), 'GetEnumerator');
   while enumStrings <> nil do begin
      var pStr : POleStr;
      if enumStrings.Next(1, pStr, nil) <> S_OK then Break;
      if reader.GetMetadataByName(pStr, pv) = S_OK then begin
         try
            if pStr = '/{ushort=34665}' then begin
               // work around metadataQueryWriter limitation
               srcName := sourcePath + '/exif';
               dstName := destPath + '/exif';
            end else begin
               srcName := sourcePath + pStr;
               dstName := destPath + pStr;
            end;
            if pv.vt = VT_UNKNOWN then begin
               if cimoRecurse in options then begin
                  CopyWICImageMetadata(
                     srcName, IUnknown(pv.ppunkVal) as IWICMetadataQueryReader,
                     dstName, writer,
                     options + [ cimoReaderAlreadyPointsToSourcePath ], report
                  );
               end;
            end else begin
               var err := writer.SetMetadataByName(PChar(dstName), pv);
               if err = 0 then
                  AddToReport('Copied %s to %s', [ srcName, dstName ])
               else AddToReport('Failed copy of %s to %s', [ srcName, dstName ]);
            end
         finally
            PropVariantClear(pv);
         end;
      end;
   end;
end;

// ------------------
// ------------------ TdwsWICImage ------------------
// ------------------

// WICBitmap
//
function TdwsWICImage.WICBitmap : IWICBitmap;
begin
   Result := FWICBitmap;
end;

// WICMetadataQueryReader
//
function TdwsWICImage.WICMetadataQueryReader : IWICMetadataQueryReader;
begin
   if FWICFrame <> nil then
      WicCheck(FWICFrame.GetMetadataQueryReader(Result), 'GetMetadataQueryReader')
   else Result := nil;
end;

// Empty
//
function TdwsWICImage.Empty : Boolean;
begin
   Result := FWICBitmap = nil;
end;

// Clear
//
procedure TdwsWICImage.Clear;
begin
   FWICBitmap := nil;
   FWICFrame := nil;
   FPixelFormat := GUID_NULL;
   FContainerFormat := GUID_NULL;
   FSize := Default(TSize);
   FFileName := '';
end;

// Width
//
function TdwsWICImage.Width : Integer;
begin
   Result := FSize.Width;
end;

// Height
//
function TdwsWICImage.Height : Integer;
begin
   Result := FSize.Height;
end;

// GetSize
//
function TdwsWICImage.GetSize : TSize;
begin
   Result := FSize;
end;

// GetPixelFormat
//
function TdwsWICImage.GetPixelFormat : TGUID;
begin
   Result := FPixelFormat;
end;

// SetPixelFormat
//
procedure TdwsWICImage.SetPixelFormat(const newFormat : TGUID);
var
   converter : IWICFormatConverter;
begin
   if newFormat = FPixelFormat then Exit;

   WicCheck(WICImagingFactory.CreateFormatConverter(converter), 'CreateFormatConverter');
   WicCheck(converter.Initialize(FWICBitmap, newFormat, WICBitmapDitherTypeNone, nil, 0, WICBitmapPaletteTypeMedianCut), 'Initialize');
   WicCheck(WICImagingFactory.CreateBitmapFromSource(converter, WICBitmapCacheOnLoad, FWICBitmap), 'CreateBitmapFromSource');

   FPixelFormat := newFormat;
end;

// ContainerFormat
//
function TdwsWICImage.ContainerFormat : TGUID;
begin
   Result := FContainerFormat;
end;

// PixelFormatInfo
//
function TdwsWICImage.PixelFormatInfo : IWICPixelFormatInfo;
var
   info : IWICComponentInfo;
begin
   WicCheck(WICImagingFactory.CreateComponentInfo(FPixelFormat, info), 'CreateComponentInfo');
   Result := info as IWICPixelFormatInfo;
end;

// ExifOrientation
//
function TdwsWICImage.ExifOrientation : WICBitmapTransformOptions;
var
   metadataQueryReader : IWICMetadataQueryReader;
   value: PROPVARIANT;
begin
   Result := WICBitmapTransformRotate0;

   metadataQueryReader := WICMetadataQueryReader;
   if metadataQueryReader <> nil then begin
      value := Default(PROPVARIANT);
      var orientation := 0;
      if Succeeded(metadataQueryReader.GetMetadataByName('/app1/ifd/{ushort=274}', Value)) then
         orientation := Value.uiVal
      else if Succeeded(metadataQueryReader.GetMetadataByName('/ifd/{ushort=274}', Value)) then
         orientation := Value.uiVal;
      Result := EXIFOrientationToWICTransform(orientation);
   end;
end;

// SetFromMemory
//
procedure TdwsWICImage.SetFromMemory(
            const aWidth, aHeight : Integer; const aPixelFormat : TGUID;
            const aStride, aBufferSize : Integer;
            const dataPtr : Pointer
         );
var
   lock : IWICBitmapLock;
begin
   if Empty or (aPixelFormat <> FPixelFormat) or (aWidth <> FSize.Width) or (aHeight <> FSize.Height) then begin
      Clear;
      WicCheck(WICImagingFactory.CreateBitmap(aWidth, aHeight, @aPixelFormat, WICBitmapCacheOnLoad, FWICBitmap), 'CreateBitmap');
      FSize.Width := aWidth;
      FSize.Height := aHeight;
      FPixelFormat := aPixelFormat;
   end;

   var rect := Default(WICRect);
   rect.Width := aWidth;
   rect.Height := aHeight;

   FWICBitmap.Lock(rect, WICBitmapLockWrite, lock);
   try
      var src := PByte(dataPtr);
      var destStride, destSize : Cardinal;
      var dest : WICInProcPointer;
      WicCheck(lock.GetStride(destStride), 'GetStride');
      WicCheck(lock.GetDataPointer(destSize, dest), 'GetDataPointer');
      if Cardinal(aStride) = destStride then begin
         Assert(Cardinal(aHeight * aStride) <= destSize);
         Assert(Cardinal(aBufferSize) >= destSize);
         System.Move(src^, dest^, aBufferSize);
      end else begin
         for var row := 0 to aHeight-1 do begin
            System.Move(src^, dest^, aStride);
            Inc(src, aStride);
            Inc(dest, destStride);
         end;
      end;
   finally
      lock := nil;
  end;
end;

// LoadFromFile
//
procedure TdwsWICImage.LoadFromFile(
   const aFileName : TFileName;
   const options : TdwsWICImageLoadOptions = cDefaultWICImageLoadOptions
);
var
   decoder : IWICBitmapDecoder;
   rotator : IWICBitmapFlipRotator;
   source : IWICBitmapSource;
   frameCount, bestFrame, bestPixelCount : Cardinal;
   sizeX, sizeY : Cardinal;
begin
   Clear;

   FFileName := aFileName;
   WicCheck(WICImagingFactory.CreateDecoderFromFilename(
      PChar(fileName), GUID_NULL, GENERIC_READ,
      WICDecodeMetadataCacheOnDemand, decoder
   ), 'CreateDecoderFromFilename');

   WicCheck(decoder.GetContainerFormat(FContainerFormat), 'GetContainerFormat');

   bestFrame := 0;
   WicCheck(decoder.GetFrameCount(frameCount), 'GetFrameCount');
   WicCheck(decoder.GetFrame(0, FWICFrame), 'GetFrame 0');

   if frameCount > 1 then begin
      WicCheck(FWICFrame.GetSize(sizeX, sizeY), 'GetSize 0');
      bestPixelCount := sizeX * sizeY;
      for var i := 1 to frameCount-1 do begin
         WicCheck(decoder.GetFrame(i, FWICFrame), 'GetFrame ' + IntToStr(i));
         WicCheck(FWICFrame.GetSize(sizeX, sizeY), 'GetSize ' + IntToStr(i));
         if sizeX * sizeY > bestPixelCount then begin
            bestPixelCount := sizeX * sizeY;
            bestFrame := i;
         end;
      end;
      WicCheck(decoder.GetFrame(bestFrame, FWICFrame), 'GetFrame best');
   end;

   source := FWICFrame;

   if iloApplyExifRotation in options then begin
      var orientation := ExifOrientation;
      if orientation <> WICBitmapTransformRotate0 then begin
         WicCheck(WICImagingFactory.CreateBitmapFlipRotator(rotator), 'CreateBitmapFlipRotator');
         WicCheck(rotator.Initialize(source, orientation), 'Initialize Rotator');
         source := rotator;
      end;
   end;

   WicCheck(
      WICImagingFactory.CreateBitmapFromSource(source, WICBitmapCacheOnDemand, FWICBitmap),
      'CreateBitmapFromSource'
   );

   WicCheck(FWICBitmap.GetSize(Cardinal(FSize.cx), Cardinal(FSize.cy)), 'GetSize');
   WicCheck(FWICBitmap.GetPixelFormat(FPixelFormat), 'GetPixelFormat');

end;

// SaveToFile
//
procedure TdwsWICImage.SaveToFile(
   const aFileName : TFileName; const aContainerFormat, aPixelFormat : TGUID;
   const onWriteMetadata : TWICImageWriteMetadataProc = nil
   );
var
   stream : IWICStream;
   encoder : IWICBitmapEncoder;
   frameEncode : IWICBitmapFrameEncode;
   metadataWriter : IWICMetadataQueryWriter;
   bag2 : IPropertyBag2;
begin
   WicCheck(WICImagingFactory.CreateEncoder(aContainerFormat, GUID_NULL, encoder), 'CreateEncoder');

   WicCheck(WICImagingFactory.CreateStream(stream), 'CreateStream');
   WicCheck(stream.InitializeFromFilename(PChar(aFileName), GENERIC_WRITE), 'InitializeFromFilename');

   WicCheck(encoder.Initialize(stream, WICBitmapEncoderNoCache), 'Initialize encoder');

   WicCheck(encoder.CreateNewFrame(frameEncode, bag2), 'CreateNewFrame');

   if FImageQuality <> 0 then begin
      var pb := Default(TPropBag2);
      pb.dwType := 1; // PROPBAG2_TYPE_DATA;
      pb.vt := VT_R4;
      pb.pstrName := 'ImageQuality';
      var v := Default(TVarData);
      v.VType := VT_R4;
      v.VSingle := FImageQuality;
      WicCheck(bag2.Write(1, @pb, @v), 'PropBag Write');
   end;

   WicCheck(frameEncode.Initialize(bag2), 'Initialize frame');

   WicCheck(frameEncode.SetSize(Width, Height), 'SetSize');
   WicCheck(frameEncode.SetResolution(96, 96), 'SetResolution');

   var savePixelFormat := aPixelFormat;
   if savePixelFormat = GUID_NULL then
      savePixelFormat := FPixelFormat;
   WicCheck(frameEncode.SetPixelFormat(savePixelFormat), 'SetPixelFormat');

   if Assigned(onWriteMetadata) then begin
      WicCheck(frameEncode.GetMetadataQueryWriter(metadataWriter), 'GetMetadataQueryWriter');
      onWriteMetadata(metadataWriter);
      metadataWriter := nil;
   end;

   WicCheck(frameEncode.WriteSource(FWICBitmap, nil), 'WriteSource');

   WicCheck(frameEncode.Commit, 'Commit frameEncode');
   WicCheck(encoder.Commit, 'Commit encoder');
   WicCheck(stream.Commit(STGC_DEFAULT), 'Commit stream');
end;

// CreateBitmap32
//
function TdwsWICImage.CreateBitmap32(scale : Single = 1) : TBitmap;
var
   converter : IWICFormatConverter;
   source : IWICBitmapSource;
   scaler : IWICBitmapScaler;
   interpolationMode : Integer;
begin
   Result := TBitmap.Create;
   try
      Result.PixelFormat := pf32bit;
      Result.AlphaFormat := afDefined;
      Result.SetSize(Round(FSize.Width * scale), Round(FSize.Height * scale));

      if Empty then Exit;

      if FPixelFormat = GUID_WICPixelFormat32bppBGRA then begin
         source := FWICBitmap;
      end else begin
         WicCheck(WICImagingFactory.CreateFormatConverter(converter), 'CreateFormatConverter');
         WicCheck(converter.Initialize(FWICBitmap, GUID_WICPixelFormat32bppBGRA, WICBitmapDitherTypeNone, nil, 0, WICBitmapPaletteTypeMedianCut), 'Initialize Converter');
         source := converter;
      end;

      if scale <> 1 then begin
         if scale < 0.8 then
            interpolationMode := WICBitmapInterpolationModeFant
         else interpolationMode := WICBitmapInterpolationModeHighQualityCubic;
         WicCheck(WICImagingFactory.CreateBitmapScaler(scaler), 'CreateBitmapScaler');
         WicCheck(scaler.Initialize(source, Result.Width, Result.Height, interpolationMode), 'Initialize Scaler');
         source := scaler as IWICBitmapSource;
      end;

      if FSize.Height = 1 then
         WicCheck(source.CopyPixels(nil, FSize.Width*4, FSize.Width*4, Result.ScanLine[0]), 'CopyPixels single line')
      else begin
         var rect : WICRect;
         rect.X := 0;
         rect.Width := Result.Width;
         rect.Height := 1;
         var stride := Result.Width*4;
         for var y := 0 to Result.Height-1 do begin
            rect.Y := y;
            WicCheck(source.CopyPixels(@rect, stride, stride, Result.ScanLine[y]), 'CopyPixels');
         end
      end;
   except
      Result.Free;
      raise;
   end;
end;

// CreateConverter
//
function TdwsWICImage.CreateConverter(const dstFormat: WICPixelFormatGUID) : IWICFormatConverter;
begin
   WicCheck(WICImagingFactory.CreateFormatConverter(Result), 'CreateFormatConverter');

   WicCheck(Result.Initialize(
      FWICBitmap, dstFormat,
      WICBitmapDitherTypeNone, nil, 0, WICBitmapPaletteTypeMedianCut
   ), 'Initialize');
end;

// GetFileName
//
function TdwsWICImage.GetFileName : TFileName;
begin
   Result := FFileName;
end;

// SetFileName
//
procedure TdwsWICImage.SetFileName(const aName : TFileName);
begin
   FFileName := aName;
end;

// GetImageQuality
//
function TdwsWICImage.GetImageQuality : Single;
begin
   Result := FImageQuality;
end;

// SetImageQuality
//
procedure TdwsWICImage.SetImageQuality(const val : Single);
begin
   FImageQuality := val;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   WICImagingFactory;

finalization

   vWICImagingFactory := nil;

end.
