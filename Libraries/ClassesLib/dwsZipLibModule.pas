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
unit dwsZipLibModule;

interface

uses
   System.SysUtils, System.Classes,
   dwsExprList, dwsExprs, dwsComp, dwsXPlatform, dwsSymbols;

type
  TdwsZipLib = class(TDataModule)
    dwsZip: TdwsUnit;
    procedure dwsZipClassesTZipReaderCleanUp(ExternalObject: TObject);
    procedure dwsZipClassesTZipReaderConstructorsCreateEval(Info: TProgramInfo;
      var ExtObject: TObject);
    procedure dwsZipClassesTZipReaderConstructorsFromDataEval(
      Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsZipClassesTZipReaderMethodsCountEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsZipClassesTZipReaderMethodsGetNameEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsZipClassesTZipReaderMethodsGetDataEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsZipClassesTZipReaderMethodsGetFullSizeEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsZipClassesTZipReaderMethodsGetZipSizeEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsZipClassesTZipReaderMethodsIndexOfEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsZipClassesTZipReaderMethodsUnzipEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsZipClassesTZipWriterConstructorsCreateEval(Info: TProgramInfo;
      var ExtObject: TObject);
    procedure dwsZipClassesTZipWriterMethodsAddFileEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsZipClassesTZipWriterMethodsAddDataEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsZipClassesTZipWriterCleanUp(ExternalObject: TObject);
    procedure dwsZipClassesTZipWriterMethodsAddFromZipEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsZipClassesTDeflateCompressorConstructorsCreateEval(
      Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsZipClassesTDeflateCompressorCleanUp(ExternalObject: TObject);
    procedure dwsZipClassesTDeflateCompressorMethodsFlushEval(
      Info: TProgramInfo; ExtObject: TObject);
    function dwsZipClassesTDeflateCompressorMethodsWriteDataFastEval(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
    procedure dwsZipClassesTZipWriterMethodsAddDeflatedDataEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsZipClassesTDeflateCompressorMethodsCRC32Eval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsZipClassesTDeflateCompressorMethodsSizeOutEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsZipClassesTDeflateCompressorMethodsSizeInEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsZipClassesTZipWriterConstructorsCreateInMemoryEval(
      Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsZipClassesTZipWriterMethodsCloseInMemoryEval(
      Info: TProgramInfo; ExtObject: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  EdwsZIPException = class (Exception);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
   Winapi.Windows,
   SynZip, dwsUtils, dwsUTF8;

{$R *.dfm}

type

   PZipEntry = ^TZipEntry;
   PFileHeader = ^TFileHeader;

   TScriptZipRead = class (TZipRead)
      Buffer : RawByteString;
      function CheckedIndex(Info : TProgramInfo) : Integer;
   end;

   TZipCompressor = class
      FInitialized : Boolean;
      FDestStream : TStream;
      FZStream : TZStream;
      FCRC : Cardinal;
      FBufferOut : array [Word] of Byte; // a 64 KB buffer

      constructor Create(outStream : TStream; compressionLevel : Integer);
      destructor Destroy; override;

      function FlushBufferOut : Integer;
      function Write(const buffer; count : Int64) : Int64;
      function WriteData(const data : String) : NativeInt;
      procedure Flush;

      property CRC : Cardinal read FCRC;
      property SizeIn : Cardinal read FZStream.total_in;
      property SizeOut : Cardinal read FZStream.total_out;
   end;

   TScriptZipStream = class
      Flushed : Boolean;
      Compressor : TZipCompressor;
      Stream : TMemoryStream;
      procedure Flush;
   end;

   TScriptZipFileEntry = record
      Name : ZipString;
      FileHeader : TFileHeader;
   end;
   PScriptZipFileEntry = ^TScriptZipFileEntry;

   TScriptZipWrite = class

      ZipStream : TStream;

      Count : Integer;
      Entries : array of TScriptZipFileEntry;

      AppendOffset : Cardinal;
      Magic : Cardinal;

      constructor CreateFile(const fileName : String);
      constructor CreateInMemory;
      destructor Destroy; override;

      procedure CloseInMemory(var dataString : String);

      procedure WrapUp;

      procedure SetHeaderFromCompressor(
         index : Integer; offsetHead : Cardinal;
         compressor : TZipCompressor; const aDateTime : TdwsDateTime
      );
      procedure SetHeader(
            index : Integer; offsetHead, sizeIn, sizeOut, crc : Cardinal;
            zipMethod : ShortInt; const aDateTime : TdwsDateTime
      );
      function InternalAdd(const zipName : TFileName; buf : Pointer; size : Int64) : Cardinal;

      procedure AddFromCompressor(stream : TScriptZipStream; const name : TFileName);
      procedure AddData(data : Pointer; size : Int64; compression : Integer; const name : TFileName);
      procedure AddString(const data : String; compression : Integer; const name : TFileName);
      procedure AddFile(const aFileName : TFileName; compression : Integer; const nameInZip : TFileName);
      procedure AddFromZip(reader : TZipRead; index : Integer);
   end;

const
   PK_FIRSTHEADER_SIGNATURE = $04034b50;
   PK_LASTHEADER_SIGNATURE  = $06054b50;
   PK_ENTRY_SIGNATURE = $02014b50;

function ZipEntry(Info : TProgramInfo; ExtObject : TObject) : PZipEntry;
var
   z : TScriptZipRead;
   i : Integer;
begin
   z := TScriptZipRead(ExtObject);
   i := z.CheckedIndex(Info);
   Result := @z.Entry[i];
end;

// ------------------
// ------------------ TScriptZipWrite ------------------
// ------------------

// CreateFile
//
constructor TScriptZipWrite.CreateFile(const fileName : String);
begin
   Create;
   Magic := PK_FIRSTHEADER_SIGNATURE;
   ZipStream := TFileStream.Create(fileName, fmCreate);
end;

// CreateInMemory
//
constructor TScriptZipWrite.CreateInMemory;
begin
   Create;
   Magic := PK_FIRSTHEADER_SIGNATURE;
   ZipStream := TMemoryStream.Create;
end;

// Destroy
//
destructor TScriptZipWrite.Destroy;
begin
   inherited;
   if ZipStream <> nil then begin
      if ZipStream is TFileStream then begin
         WrapUp;
         ZipStream.Size := ZipStream.Position;
      end;
      ZipStream.Free;
   end;
end;

// CloseInMemory
//
procedure TScriptZipWrite.CloseInMemory(var dataString : String);
var
   nb : Int64;
begin
   if ZipStream <> nil then begin
      WrapUp;
      nb := ZipStream.Position;
      SetLength(dataString, nb);
      if ZipStream is TMemoryStream then begin
         BytesToWords(TMemoryStream(ZipStream).Memory, Pointer(dataString), nb);
      end else begin
         ZipStream.Size := nb;
         ZipStream.Read(Pointer(dataString)^, nb);
         ZipStream.Free;
         BytesToWordsInPlace(Pointer(dataString), nb);
      end;
   end;
end;

// WrapUp
//
procedure TScriptZipWrite.WrapUp;
var
   lhr : TLastHeader;
   i : Integer;
   entry : PScriptZipFileEntry;
begin
   lhr := Default(TLastHeader);
   lhr.signature := PK_LASTHEADER_SIGNATURE;
   lhr.thisFiles := Count;
   lhr.totalFiles := Count;
   lhr.headerOffset := ZipStream.Position - AppendOffset;
   for i := 0 to Count-1 do begin
      entry := @Entries[i];
      Assert(entry.FileHeader.fileInfo.nameLen = Length(entry.Name));
      Inc(lhr.headerSize, SizeOf(TFileHeader) + entry.FileHeader.fileInfo.nameLen);
      ZipStream.Write(entry.FileHeader, SizeOf(entry.FileHeader));
      ZipStream.Write(Pointer(entry.Name)^, entry.FileHeader.fileInfo.nameLen);
   end;
   ZipStream.Write(lhr, SizeOf(lhr));
end;

// SetHeaderFromCompressor
//
procedure TScriptZipWrite.SetHeaderFromCompressor(
      index : Integer; offsetHead : Cardinal;
      compressor : TZipCompressor; const aDateTime : TdwsDateTime
);
begin
   SetHeader(index, offsetHead, compressor.SizeIn, compressor.SizeOut, compressor.CRC, Z_DEFLATED, aDateTime);
end;

procedure TScriptZipWrite.SetHeader(
      index : Integer; offsetHead, sizeIn, sizeOut, crc : Cardinal;
      zipMethod : ShortInt; const aDateTime : TdwsDateTime
);
var
   offsetEnd : Cardinal;
   fhr : PFileHeader;
   fileInfo : PFileInfo;
begin
   fhr := @Entries[index].FileHeader;
   fileInfo := @fhr.fileInfo;
   fileInfo.zcrc32 := crc;
   fileInfo.zfullSize := sizeIn;
   fileInfo.zzipSize := sizeOut;
   fileInfo.zzipMethod := zipMethod;
   fileInfo.zlastMod := aDateTime.AsDosDateTime;

   offsetEnd := ZipStream.Position;
   ZipStream.Position := offsetHead + SizeOf(Magic);
   ZipStream.Write(fileInfo^, SizeOf(fhr.fileInfo));
   ZipStream.Position := offsetEnd;
end;

// InternalAdd
//
function TScriptZipWrite.InternalAdd(const zipName : TFileName; buf : Pointer; size : Int64) : Cardinal;
var
   entry : PScriptZipFileEntry;
   fileHeader : PFileHeader;
begin
   entry := @Entries[Count];
   fileHeader := @entry.FileHeader;
   fileHeader.signature := PK_ENTRY_SIGNATURE;
   fileHeader.madeBy := $14;
   fileHeader.fileInfo.neededVersion := $14;
   Result := ZipStream.Position;
   fileHeader.localHeadOff := Result - AppendOffset;
   entry.Name := StringToUTF8(zipName);
   fileHeader.fileInfo.SetUTF8FileName;
   fileHeader.fileInfo.nameLen := Length(entry.Name);
   fileHeader.fileInfo.extraLen := 0; // source may have something here
   ZipStream.Write(Magic, SizeOf(Magic));
   ZipStream.Write(fileHeader.fileInfo, SizeOf(fileHeader.fileInfo));
   ZipStream.Write(Pointer(entry.Name)^, fileHeader.fileInfo.nameLen);
   if buf <> nil then begin
      ZipStream.Write(buf^, size); // write stored data
      Inc(Count);
   end;
end;

// AddFromCompressor
//
procedure TScriptZipWrite.AddFromCompressor(stream : TScriptZipStream; const name : TFileName);
var
   offsetHead : Cardinal;
begin
   SetLength(Entries, Count+1);
   offsetHead := InternalAdd(name, nil, 0);

   stream.Flush;
   Assert(stream.Stream.Size = stream.Compressor.SizeOut);

   ZipStream.Write(stream.Stream.Memory^, stream.Compressor.SizeOut);

   SetHeaderFromCompressor(Count, offsetHead, stream.Compressor, TdwsDateTime.Now);

   Inc(Count);
end;

// AddData
//
procedure TScriptZipWrite.AddData(data : Pointer; size : Int64; compression : Integer; const name : TFileName);
var
   offsetHead : Cardinal;
   zipCompressor : TZipCompressor;
begin
   if Int64Rec(size).Hi <> 0 then
      raise EdwsZIPException.Create('File size limit for zip is 4GB');

   SetLength(Entries, Count+1);
   offsetHead := InternalAdd(name, nil, 0);

   if compression = 0 then begin
      ZipStream.Write(data^, size);
      SetHeader(Count, offsetHead, size, size, crc32(0, data, size), Z_STORED, TdwsDateTime.Now);
   end else begin
      zipCompressor := TZipCompressor.Create(ZipStream, compression);
      try
         if size > 0 then begin
            zipCompressor.Write(data^, size);
            zipCompressor.Flush;
         end;
         Assert(zipCompressor.SizeIn = size);
         SetHeaderFromCompressor(Count, offsetHead, zipCompressor, TdwsDateTime.Now);
      finally
         zipCompressor.Free;
      end;
   end;
   Inc(Count);
end;

// AddString
//
procedure TScriptZipWrite.AddString(const data : String; compression : Integer; const name : TFileName);
var
   offsetHead : Cardinal;
   zipCompressor : TZipCompressor;
begin
   SetLength(Entries, Count+1);
   offsetHead := InternalAdd(name, nil, 0);

   zipCompressor := TZipCompressor.Create(ZipStream, compression);
   try
      zipCompressor.WriteData(data);
      zipCompressor.Flush;
      Assert(zipCompressor.SizeIn = NativeUInt(Length(data)));
      SetHeaderFromCompressor(Count, offsetHead, zipCompressor, TdwsDateTime.Now);
   finally
      zipCompressor.Free;
   end;
   Inc(Count);
end;

// AddFile
//
procedure TScriptZipWrite.AddFile(const aFileName : TFileName; compression : Integer; const nameInZip : TFileName);
var
   crc : Cardinal;
   size : Int64;
   offsetHead : Cardinal;
   zipCompressor : TZipCompressor;
   fileHandle : THandle;
   buffer : array [Word] of Byte; // 64kb
   nbRead : Integer;
begin
   SetLength(Entries, Count+1);
   offsetHead := InternalAdd(nameInZip, nil, 0);

   fileHandle := OpenFileForSequentialReadOnly(aFileName);
   try
      if not GetFileSizeEx(fileHandle, size) then
         RaiseLastOSError;
      if Int64Rec(size).Hi <> 0 then
         raise EdwsZIPException.Create('File size limit for zip is 4GB');

      if compression = 0 then begin
         crc := 0;
         while True do begin
            nbRead := FileRead(fileHandle, @buffer[0], SizeOf(buffer));
            if nbRead <= 0 then break;
            crc := crc32(crc, @buffer, nbRead);
            ZipStream.Write(buffer, nbRead);
         end;
         SetHeader(Count, offsetHead, size, size, crc, Z_STORED, FileDateTime(aFileName));
      end else begin
         zipCompressor := TZipCompressor.Create(ZipStream, compression);
         try
            while True do begin
               nbRead := FileRead(fileHandle, @buffer[0], SizeOf(buffer));
               if nbRead <= 0 then break;
               zipCompressor.Write(buffer, nbRead)
            end;
            zipCompressor.Flush;
            Assert(zipCompressor.SizeIn = size);
            SetHeaderFromCompressor(Count, offsetHead, zipCompressor, FileDateTime(aFileName));
         finally
            zipCompressor.Free;
         end;
      end;
   finally
      CloseFileHandle(fileHandle);
   end;
   Inc(Count);
end;

// AddFromZip
//
procedure TScriptZipWrite.AddFromZip(reader : TZipRead; index : Integer);
var
   n : Integer;
begin
   n := Count;
   SetLength(Entries, n + 1);
   if not reader.RetrieveFileInfo(index, Entries[n].FileHeader.fileInfo) then
      raise EdwsZIPException.CreateFmt('Failed to retrieve ZIP info for index %d', [ index ]);

   // adds header and data and increments Count
   InternalAdd(reader.Entry[index].zipName, reader.Entry[index].data, Entries[n].FileHeader.fileInfo.zzipSize);
   if StrEndsWith(reader.Entry[index].zipName, '\') then
      Entries[n].FileHeader.extFileAttr := Entries[n].FileHeader.extFileAttr or $00000010;
end;

// ------------------
// ------------------ TZipCompressor ------------------
// ------------------

function zlibAllocMem(appData : Pointer; items, size : Cardinal) : Pointer; cdecl;
begin
   GetMem(result, items*size);
end;

procedure zlibFreeMem(appData, block : Pointer);  cdecl;
begin
   FreeMem(block);
end;

constructor TZipCompressor.Create(outStream : TStream; compressionLevel : Integer);
begin
   FDestStream := outStream;

   FZStream := Default(TZStream);
   FZStream.zalloc := @zlibAllocMem;
   FZStream.zfree := @zlibFreeMem;

   FZStream.next_out := @FBufferOut;
   FZStream.avail_out := SizeOf(FBufferOut);
   FInitialized := deflateInit2_(FZStream, compressionLevel, Z_DEFLATED, -MAX_WBITS, DEF_MEM_LEVEL,
                                 Z_DEFAULT_STRATEGY, ZLIB_VERSION, SizeOf(FZStream)) >= 0
end;

destructor TZipCompressor.Destroy;
begin
   if FInitialized then begin
      Flush;
      FZStream.next_out := nil;
      FZStream.avail_out := 0;
      deflateEnd(FZStream);
   end;
   inherited;
end;

procedure TZipCompressor.Flush;
begin
   if FInitialized then begin
      while     (Check(deflate(FZStream, Z_FINISH), [ Z_OK, Z_STREAM_END ], 'Flush') <> Z_STREAM_END)
            and (FZStream.avail_out = 0) do
         FlushBufferOut;
      FlushBufferOut;
   end;
end;

function TZipCompressor.FlushBufferOut : Integer;
begin
   Result := 0;
   if not FInitialized then
      Exit;
   if FZStream.avail_out < SizeOf(FBufferOut) then begin
      Result := SizeOf(FBufferOut) - FZStream.avail_out;
      FDestStream.WriteBuffer(FBufferOut, Result);
      FZStream.next_out := @FBufferOut;
      FZStream.avail_out := SizeOf(FBufferOut);
   end;
end;

function TZipCompressor.Write(const buffer; count : Int64) : Int64;
begin
   if (Self = nil) or (not FInitialized) or (count <= 0) then
      Exit(0);
   Result := count;
   FCRC := SynZip.crc32(FCRC, @buffer, count);
   FZStream.next_in := Pointer(@buffer);
   FZStream.avail_in := count;
   while FZStream.avail_in > 0 do begin
      if Check(deflate(FZStream, Z_NO_FLUSH), [ Z_OK ], 'Write') <> Z_OK then
         raise EdwsZIPException.Create('ZCompress');
      if FZStream.avail_out = 0 then
         FlushBufferOut;
   end;
   FZStream.next_in := nil;
   FZStream.avail_in := 0;
end;

// WriteData
//
function TZipCompressor.WriteData(const data : String) : NativeInt;
var
   p : PWordArray;
   buffer : array [Word] of Byte;
   size : NativeInt;
begin
   Result := Length(data);
   case Result of
      0 : Exit(0);
      1 : begin
         Write(Pointer(data)^, 1);
         Exit(1);
      end
   else
      p := Pointer(data);
      size := Result;
      while size > SizeOf(buffer) do begin
         WordsToBytes(p, @buffer, SizeOf(buffer));
         Dec(size, SizeOf(buffer));
         Inc(p, size);
         Write(buffer, SizeOf(buffer));
      end;
      if size > 0 then begin
         WordsToBytes(p, @buffer, size);
         Write(buffer, size);
      end;
   end;
end;

// ------------------
// ------------------ TScriptZipStream ------------------
// ------------------

procedure TScriptZipStream.Flush;
begin
   if not Flushed then begin
      Flushed := True;
      Compressor.Flush;
   end;
end;

// ------------------
// ------------------ TScriptZipRead ------------------
// ------------------

function TScriptZipRead.CheckedIndex(Info : TProgramInfo) : Integer;
begin
   Result := Info.ParamAsInteger[0];
   if Cardinal(Result) >= Cardinal(Count) then
      raise EdwsZIPException.CreateFmt('ZIP file index out of bounds (%d)', [ Result ]);
end;

// ------------------
// ------------------ TdwsZipLib ------------------
// ------------------

procedure TdwsZipLib.dwsZipClassesTZipReaderCleanUp(
  ExternalObject: TObject);
begin
   ExternalObject.Free;
end;

procedure TdwsZipLib.dwsZipClassesTZipReaderConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
   ExtObject := TScriptZipRead.Create(Info.ParamAsFileName[0], 0, 0);
end;

procedure TdwsZipLib.dwsZipClassesTZipReaderConstructorsFromDataEval(
  Info: TProgramInfo; var ExtObject: TObject);
var
   z : TScriptZipRead;
   buf : RawByteString;
begin
   buf:=Info.ParamAsDataString[0];
   z:=TScriptZipRead.Create(PByteArray(buf), Length(buf));
   z.Buffer:=buf;
   ExtObject:=z;
end;

procedure TdwsZipLib.dwsZipClassesTZipReaderMethodsCountEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=TScriptZipRead(ExtObject).Count;
end;

procedure TdwsZipLib.dwsZipClassesTZipReaderMethodsGetDataEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   z : TScriptZipRead;
   i : Integer;
begin
   z:=TScriptZipRead(ExtObject);
   i:=z.CheckedIndex(Info);
   Info.ResultAsDataString:=z.UnZip(i);
end;

procedure TdwsZipLib.dwsZipClassesTZipReaderMethodsGetFullSizeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=ZipEntry(Info, ExtObject).infoLocal.zfullSize;
end;

procedure TdwsZipLib.dwsZipClassesTZipReaderMethodsGetNameEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   z : TScriptZipRead;
begin
   z:=TScriptZipRead(ExtObject);
   Info.ResultAsString:=z.Entry[z.CheckedIndex(Info)].zipName;
end;

procedure TdwsZipLib.dwsZipClassesTZipReaderMethodsGetZipSizeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=ZipEntry(Info, ExtObject).infoLocal.zzipSize;
end;

procedure TdwsZipLib.dwsZipClassesTZipReaderMethodsIndexOfEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=TScriptZipRead(ExtObject).NameToIndex(Info.ParamAsString[0]);
end;

procedure TdwsZipLib.dwsZipClassesTZipReaderMethodsUnzipEval(Info: TProgramInfo; ExtObject: TObject);
var
   i : Integer;
   z : TScriptZipRead;
   fileName : String;
begin
   z := TScriptZipRead(ExtObject);
   i := z.CheckedIndex(Info);
   fileName := Info.ParamAsFileName[1];
   if ForceDirectories(ExtractFilePath(fileName)) then begin
      z.UnZip(i, fileName, True);
      Info.ResultAsBoolean := True;
   end else Info.ResultAsBoolean := False;
end;

procedure TdwsZipLib.dwsZipClassesTZipWriterCleanUp(ExternalObject: TObject);
begin
   ExternalObject.Free;
end;

procedure TdwsZipLib.dwsZipClassesTZipWriterConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
   ExtObject := TScriptZipWrite.CreateFile(Info.ParamAsFileName[0]);
end;

procedure TdwsZipLib.dwsZipClassesTZipWriterConstructorsCreateInMemoryEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
   ExtObject := TScriptZipWrite.CreateInMemory;
end;

procedure TdwsZipLib.dwsZipClassesTZipWriterMethodsCloseInMemoryEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   z : TScriptZipWrite;
   data : String;
begin
   z := TScriptZipWrite(ExtObject);
   z.CloseInMemory(data);
   Info.ResultAsString := data;
   Info.ScriptObj.Destroyed := True;
end;

procedure TdwsZipLib.dwsZipClassesTZipWriterMethodsAddDataEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   z : TScriptZipWrite;
   buf : RawByteString;
   nameInZip : String;
begin
   z := TScriptZipWrite(ExtObject);

   buf := Info.ParamAsDataString[0];
   nameInZip := Info.ParamAsString[2];

   z.AddData(Pointer(buf), Length(buf), Info.ParamAsInteger[1], nameInZip);
end;

procedure TdwsZipLib.dwsZipClassesTZipWriterMethodsAddDeflatedDataEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   z : TScriptZipWrite;
   szs : TScriptZipStream;
begin
   z := TScriptZipWrite(ExtObject);
   szs := Info.ParamAsObject[0] as TScriptZipStream;
   z.AddFromCompressor(szs, Info.ParamAsString[1]);
end;

procedure TdwsZipLib.dwsZipClassesTZipWriterMethodsAddFileEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   z : TScriptZipWrite;
   fileName, nameInZip : String;
begin
   z:=TScriptZipWrite(ExtObject);

   fileName:=Info.ParamAsFileName[0];
   nameInZip:=Info.ParamAsString[2];
   if nameInZip='' then
      nameInZip:=ExtractFileName(fileName);

   z.AddFile(fileName, Info.ParamAsInteger[1], nameInZip);
end;

procedure TdwsZipLib.dwsZipClassesTZipWriterMethodsAddFromZipEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   z : TScriptZipWrite;
   readerObj : IScriptObj;
   reader : TScriptZipRead;
   index : Integer;
begin
   z:=TScriptZipWrite(ExtObject);

   readerObj:=Info.ParamAsScriptObj[0];
   if readerObj.Destroyed then
      raise Exception.Create('zipReader object already closed');
   reader:=(readerObj.ExternalObject as TScriptZipRead);
   if reader=nil then
      raise Exception.Create('zipReader is nil');

   index:=Info.ParamAsInteger[1];
   if Cardinal(index)>=Cardinal(reader.Count) then
      raise Exception.CreateFmt('zipReader entry index out of bounds (%d)', [index]);

   z.AddFromZip(reader, index);
end;

procedure TdwsZipLib.dwsZipClassesTDeflateCompressorConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
var
   szs : TScriptZipStream;
begin
   szs := TScriptZipStream.Create;
   ExtObject := szs;
   szs.Stream := TMemoryStream.Create;
   szs.Compressor := TZipCompressor.Create(szs.Stream, Info.ParamAsInteger[0]);
end;

procedure TdwsZipLib.dwsZipClassesTDeflateCompressorCleanUp(
  ExternalObject: TObject);
var
   szs : TScriptZipStream;
begin
   szs := TScriptZipStream(ExternalObject);
   szs.Compressor.Free;
   szs.Stream.Free;
   szs.Free;
end;

function TdwsZipLib.dwsZipClassesTDeflateCompressorMethodsWriteDataFastEval(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Variant;
var
   obj : IScriptObj;
   szs : TScriptZipStream;
   buf : String;
begin
   baseExpr.EvalAsSafeScriptObj(args.Exec, obj);
   szs := TScriptZipStream(obj.ExternalObject);
   if szs.Flushed then
      raise EdwsZIPException.Create('Compressor already flushed');
   args.EvalAsString(0, buf);
   szs.Compressor.WriteData(buf);
end;

procedure TdwsZipLib.dwsZipClassesTDeflateCompressorMethodsCRC32Eval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger := TScriptZipStream(ExtObject).Compressor.CRC;
end;

procedure TdwsZipLib.dwsZipClassesTDeflateCompressorMethodsFlushEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   szs : TScriptZipStream;
   size : Int64;
   result : String;
   p : PByteArray;
begin
   szs := TScriptZipStream(ExtObject);
   szs.Flush;
   size := szs.Stream.Position;
   p := szs.Stream.Memory;
   if size > 0 then begin
      // skip ZIP compression header ($78 + compression level)
//      p := @p[2];
//      BytesToScriptString(p, size-2, result);
      BytesToScriptString(p, size, result);
   end;
   Info.ResultAsString := result;
end;

procedure TdwsZipLib.dwsZipClassesTDeflateCompressorMethodsSizeInEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger := TScriptZipStream(ExtObject).Compressor.SizeIn;
end;

procedure TdwsZipLib.dwsZipClassesTDeflateCompressorMethodsSizeOutEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger := TScriptZipStream(ExtObject).Compressor.SizeOut;
end;

end.
