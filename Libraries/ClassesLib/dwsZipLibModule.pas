unit dwsZipLibModule;

interface

uses
  Windows, SysUtils, Classes,
  dwsExprList, dwsExprs, dwsComp, dwsWebUtils, dwsUtils, dwsXPlatform, dwsSymbols,
  SynZip;

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

implementation

{$R *.dfm}

type

   PZipEntry = ^TZipEntry;
   PFileHeader = ^TFileHeader;

   TScriptZipRead = class (TZipRead)
      Buffer : RawByteString;
      function CheckedIndex(Info : TProgramInfo) : Integer;
   end;

   TScriptZipStream = class
      Flushed : Boolean;
      Compressor : TSynZipCompressor;
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
         compressor : TSynZipCompressor; const aDateTime : TdwsDateTime
      );
      function InternalAdd(const zipName : TFileName; buf : Pointer; size : Integer) : Cardinal;

      procedure AddFromCompressor(stream : TScriptZipStream; const name : TFileName);
      procedure AddData(data : Pointer; size : Int64; compression : Integer; const name : TFileName);
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
      compressor : TSynZipCompressor; const aDateTime : TdwsDateTime
);
var
   offsetEnd : Cardinal;
   fhr : PFileHeader;
   fileInfo : PFileInfo;
begin
   fhr := @Entries[index].FileHeader;
   fileInfo := @fhr.fileInfo;
   fileInfo.zcrc32 := compressor.CRC;
   fileInfo.zfullSize := compressor.SizeIn;
   fileInfo.zzipSize := compressor.SizeOut;
   fileInfo.zzipMethod := Z_DEFLATED;
   fileInfo.zlastMod := aDateTime.AsDosDateTime;

   offsetEnd := ZipStream.Position;
   ZipStream.Position := offsetHead + SizeOf(Magic);
   ZipStream.Write(fileInfo^, SizeOf(fhr.fileInfo));
   ZipStream.Position := offsetEnd;
end;

// InternalAdd
//
function TScriptZipWrite.InternalAdd(const zipName : TFileName; buf : Pointer; size : Integer) : Cardinal;
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
   entry.Name := UTF8Encode(zipName);
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
   zipCompressor: TSynZipCompressor;
begin
   SetLength(Entries, Count+1);
   offsetHead := InternalAdd(name, nil, 0);

   zipCompressor := TSynZipCompressor.Create(ZipStream, compression);
   try
      zipCompressor.Write(data^, size);
      zipCompressor.Flush;
      Assert(zipCompressor.SizeIn = size);
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
   size : Int64;
   offsetHead : cardinal;
   sourceStream : TFileStream;
   zipCompressor : TSynZipCompressor;
begin
   SetLength(Entries, Count+1);
   offsetHead := InternalAdd(nameInZip, nil, 0);

   sourceStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
   try
      size := sourceStream.size;
      if Int64Rec(size).Hi <> 0 then
         raise ESynZipException.CreateFmt('"%s" file is too big for .zip', [aFileName]);

      zipCompressor := TSynZipCompressor.Create(ZipStream, compression);
      try
         zipCompressor.CopyFrom(sourceStream, size);
         zipCompressor.Flush;
         Assert(zipCompressor.SizeIn = size);
         SetHeaderFromCompressor(Count, offsetHead, zipCompressor, FileDateTime(aFileName));
      finally
         zipCompressor.Free;
      end;
   finally
      sourceStream.Free;
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

   InternalAdd(reader.Entry[index].zipName, reader.Entry[index].data, Entries[n].FileHeader.fileInfo.zzipSize);
   if StrEndsWith(reader.Entry[index].zipName, '\') then
      Entries[n].FileHeader.extFileAttr := Entries[n].FileHeader.extFileAttr or $00000010;
   Inc(Count);
end;

// ------------------
// ------------------ TScriptZipStream ------------------
// ------------------

// Flush
//
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

// CheckedIndex
//
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

   buf:=Info.ParamAsDataString[0];
   nameInZip:=Info.ParamAsString[2];

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
   szs.Compressor := TSynZipCompressor.Create(szs.Stream, Info.ParamAsInteger[0]);
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

   case Length(buf) of
      0 : ;
      1 : szs.Compressor.Write(Pointer(buf)^, 1);
   else
      StringWordsToBytes(buf, False);
      szs.Compressor.Write(Pointer(buf)^, 2*Length(buf));
   end;
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
   result : String;
begin
   szs := TScriptZipStream(ExtObject);
   szs.Flush;
   BytesToScriptString(szs.Stream.Memory, szs.Stream.Position, result);
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
