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
   IdwsWICImage = interface
      ['{AC94EC11-5E2C-4343-B671-B2D4C743168E}']

      function WICBitmap : IWICBitmap;
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

      function GetFileName : TFileName;
      procedure SetFileName(const aName : TFileName);
      property FileName : TFileName read GetFileName write SetFileName;

      procedure SetFromMemory(
            const aWidth, aHeight : Integer; const aPixelFormat : TGUID;
            const aStride, aBufferSize : Integer;
            const dataPtr : Pointer
         );

      procedure LoadFromFile(const aFileName : TFileName);
      procedure SaveToFile(const aFileName : TFileName; const aContainerFormat, aPixelFormat : TGUID);

      function CreateBitmap32(scale : Single = 1) : TBitmap;
      function CreateConverter(const dstFormat: WICPixelFormatGUID) : IWICFormatConverter;

   end;

   TdwsWICImage = class (TInterfacedObject, IdwsWICImage)
      private
         FWICBitmap : IWICBitmap;
         FSize : TSize;
         FPixelFormat : TGUID;
         FFileName : String;

      protected
         function GetFileName : TFileName;
         procedure SetFileName(const aName : TFileName);

      public

         function WICBitmap : IWICBitmap;
         function Empty : Boolean;
         procedure Clear;

         function Width : Integer;
         function Height : Integer;
         function GetSize : TSize;
         function GetPixelFormat : TGUID;
         procedure SetPixelFormat(const newFormat : TGUID);
         function PixelFormatInfo : IWICPixelFormatInfo;

         property FileName : String read FFileName write FFileName;

         procedure SetFromMemory(
               const aWidth, aHeight : Integer; const aPixelFormat : TGUID;
               const aStride, aBufferSize : Integer;
               const dataPtr : Pointer
            );
         procedure LoadFromFile(const aFileName : TFileName);
         procedure SaveToFile(const aFileName : TFileName; const aContainerFormat, aPixelFormat : TGUID);

         function CreateBitmap32(scale : Single = 1)  : TBitmap;

         function CreateConverter(const dstFormat: WICPixelFormatGUID) : IWICFormatConverter;

   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
   vWICImagingFactory : IWICImagingFactory;

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

procedure WicCheck(Result: HRESULT; const context : String);
begin
   if Failed(Result) then
      raise Exception.CreateFmt('%s, WIC error %x', [ context, Result ]);
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
   FPixelFormat := GUID_NULL;
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

// PixelFormatInfo
//
function TdwsWICImage.PixelFormatInfo : IWICPixelFormatInfo;
var
   info : IWICComponentInfo;
begin
   WicCheck(WICImagingFactory.CreateComponentInfo(FPixelFormat, info), 'CreateComponentInfo');
   Result := info as IWICPixelFormatInfo;
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
procedure TdwsWICImage.LoadFromFile(const aFileName : TFileName);
var
   decoder : IWICBitmapDecoder;
   frame : IWICBitmapFrameDecode;
begin
   Clear;

   FFileName := aFileName;
   WicCheck(WICImagingFactory.CreateDecoderFromFilename(
      PChar(fileName), GUID_NULL, GENERIC_READ,
      WICDecodeMetadataCacheOnDemand, decoder
   ), 'CreateDecoderFromFilename');

   WicCheck(decoder.GetFrame(0, frame), 'GetFrame');
   WicCheck(
      WICImagingFactory.CreateBitmapFromSource(frame, WICBitmapCacheOnDemand, FWICBitmap),
      'CreateBitmapFromSource'
   );

   WicCheck(FWICBitmap.GetSize(Cardinal(FSize.cx), Cardinal(FSize.cy)), 'GetSize');
   WicCheck(FWICBitmap.GetPixelFormat(FPixelFormat), 'GetPixelFormat');
end;

// SaveToFile
//
procedure TdwsWICImage.SaveToFile(const aFileName : TFileName; const aContainerFormat, aPixelFormat : TGUID);
var
   stream : IWICStream;
   encoder : IWICBitmapEncoder;
   frameEncode : IWICBitmapFrameEncode;
   bag2 : IPropertyBag2;
begin
   WicCheck(WICImagingFactory.CreateEncoder(aContainerFormat, GUID_NULL, encoder), 'CreateEncoder');

   WicCheck(WICImagingFactory.CreateStream(stream), 'CreateStream');
   WicCheck(stream.InitializeFromFilename(PChar(aFileName), GENERIC_WRITE), 'InitializeFromFilename');

   WicCheck(encoder.Initialize(stream, WICBitmapEncoderNoCache), 'Initialize encoder');

   WicCheck(encoder.CreateNewFrame(frameEncode, bag2), 'CreateNewFrame');
   WicCheck(frameEncode.Initialize(bag2), 'Initialize frame');

   WicCheck(frameEncode.SetSize(Width, Height), 'SetSize');
   WicCheck(frameEncode.SetResolution(96, 96), 'SetResolution');

   var savePixelFormat := aPixelFormat;
   if savePixelFormat = GUID_NULL then
      savePixelFormat := FPixelFormat;
   WicCheck(frameEncode.SetPixelFormat(savePixelFormat), 'SetPixelFormat');

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
         else interpolationMode := WICBitmapInterpolationModeCubic;
         WicCheck(WICImagingFactory.CreateBitmapScaler(scaler), 'CreateBitmapScaler');
         WicCheck(scaler.Initialize(source, Result.Width, Result.Height, interpolationMode), 'Initialize Scaler');
         source := scaler as IWICBitmapSource;
      end;

      if FSize.Height = 1 then
         WicCheck(source.CopyPixels(nil, FSize.Width*4, FSize.Width*4, Result.ScanLine[0]), 'CopyPixels single line')
      else begin
         var stride := IntPtr(Result.ScanLine[1]) - IntPtr(Result.ScanLine[0]);
         var p : Pointer;
         if stride < 0 then begin
            stride := -stride;
            p := Result.ScanLine[Result.Height-1];
         end else begin
            p := Result.ScanLine[0];
         end;
         WicCheck(source.CopyPixels(nil, stride, stride*FSize.Height, p), 'CopyPixels');
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
