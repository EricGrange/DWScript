unit dwsZipLibModule;

interface

uses
  Windows, SysUtils, Classes,
  dwsExprs, dwsComp, dwsWebUtils, dwsUtils, dwsXPlatform, dwsSymbols,
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

   TScriptZipWrite = class (TZipWrite)
      procedure AddData(data : Pointer; size : Int64; compression : Integer; const name : TFileName);
      procedure AddFile(const aFileName : TFileName; compression : Integer; const nameInZip : TFileName);
   end;

function DateTimeToDosDateTime(dt : TDateTime) : Integer;
var
   sysTime : TSystemTime;
   fileTime : TFileTime;
   dosTime : LongRec;
begin
   DateTimeToSystemTime(dt, sysTime);
   SystemTimeToFileTime(sysTime, fileTime);
   FileTimeToDosDateTime(fileTime, dosTime.Hi, dosTime.Lo);
   Result:=Integer(dosTime);
end;

// AddData
//
procedure TScriptZipWrite.AddData(data : Pointer; size : Int64; compression : Integer; const name : TFileName);
var
   offsetHead, offsetEnd: cardinal;
   destStream: THandleStream;
   zipCompressor: TSynZipCompressor;
   fhr : PFileHeader;
   fileInfo : PFileInfo;
begin
   SetLength(Entry, Count+1);
   offsetHead := InternalAdd(name, nil, 0);

   destStream := THandleStream.Create(Handle);
   zipCompressor := TSynZipCompressor.Create(destStream, compression);
   try
      zipCompressor.Write(data^, size);
      zipCompressor.Flush;
      assert(zipCompressor.SizeIn=size);
      fhr:=@Entry[Count].fhr;
      fileInfo:=@fhr.fileInfo;
      fileInfo.zcrc32 := zipCompressor.CRC;
      fileInfo.zfullSize := zipCompressor.SizeIn;
      fileInfo.zzipSize := zipCompressor.SizeOut;
      fileInfo.zzipMethod := Z_DEFLATED;
      fileInfo.zlastMod := DateTimeToDosDateTime(Now);
      offsetEnd := destStream.Position;
      destStream.Position := offsetHead+SizeOf(fMagic);
      destStream.Write(fileInfo^, SizeOf(fhr.fileInfo));
      destStream.Position := offsetEnd;
   finally
      zipCompressor.Free;
      destStream.Free;
   end;
   Inc(Count);
end;

// AddFile
//
procedure TScriptZipWrite.AddFile(const aFileName : TFileName; compression : Integer; const nameInZip : TFileName);
var
   size : Int64;
   offsetHead, offsetEnd : cardinal;
   sourceStream : TFileStream;
   destStream : THandleStream;
   zipCompressor : TSynZipCompressor;
   fhr : PFileHeader;
   fileInfo : PFileInfo;
begin
   SetLength(Entry, Count+1);
   offsetHead := InternalAdd(nameInZip, nil, 0);

   sourceStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
   try
      size := sourceStream.size;
      if Int64Rec(size).Hi<>0 then
         raise ESynZipException.CreateFmt('"%s" file is too big for .zip', [aFileName]);

      destStream := THandleStream.Create(Handle);
      zipCompressor := TSynZipCompressor.Create(destStream, compression);
      try
         zipCompressor.CopyFrom(sourceStream, size);
         zipCompressor.Flush;
         Assert(zipCompressor.SizeIn=size);
         fhr:=@Entry[Count].fhr;
         fileInfo:=@fhr.fileInfo;
         fileInfo.zcrc32 := zipCompressor.CRC;
         fileInfo.zfullSize := zipCompressor.SizeIn;
         fileInfo.zzipSize := zipCompressor.SizeOut;
         fileInfo.zzipMethod := Z_DEFLATED;
         fileInfo.zlastMod := DateTimeToDosDateTime(FileDateTime(aFileName));
         offsetEnd := destStream.Position;
         destStream.Position := offsetHead+SizeOf(fMagic);
         destStream.Write(fhr.fileInfo, SizeOf(fhr.fileInfo));
         destStream.Position := offsetEnd;
         Inc(Count);
      finally
         zipCompressor.Free;
         destStream.Free;
      end;
   finally
      sourceStream.Free;
   end;
end;

function TScriptZipRead.CheckedIndex(Info : TProgramInfo) : Integer;
begin
   Result:=Info.ParamAsInteger[0];
   if Cardinal(Result)>=Cardinal(Count) then
      raise EdwsZIPException.CreateFmt('ZIP file index out of bounds (%d)', [Result]);
end;

function ZipEntry(Info : TProgramInfo; ExtObject : TObject) : PZipEntry;
var
   z : TScriptZipRead;
   i : Integer;
begin
   z:=TScriptZipRead(ExtObject);
   i:=z.CheckedIndex(Info);
   Result:=@z.Entry[i];
end;

procedure TdwsZipLib.dwsZipClassesTZipReaderCleanUp(
  ExternalObject: TObject);
begin
   ExternalObject.Free;
end;

procedure TdwsZipLib.dwsZipClassesTZipReaderConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
var
   z : TScriptZipRead;
begin
   z:=TScriptZipRead.Create(Info.ParamAsString[0], 0, 0);
   ExtObject:=z;
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
   buf : RawByteString;
   h : Cardinal;
begin
   z:=TScriptZipRead(ExtObject);
   i:=z.CheckedIndex(Info);
   fileName:=Info.ParamAsString[1];
   if ForceDirectories(ExtractFilePath(fileName)) then begin
      h:=OpenFileForSequentialWriteOnly(fileName);
      try
         buf:=z.UnZip(i);
         FileWrite(h, Pointer(buf), Length(buf));
      finally
         CloseFileHandle(h);
      end;
      Info.ResultAsBoolean:=True;
   end else Info.ResultAsBoolean:=False;
end;

procedure TdwsZipLib.dwsZipClassesTZipWriterCleanUp(ExternalObject: TObject);
begin
   ExternalObject.Free;
end;

procedure TdwsZipLib.dwsZipClassesTZipWriterConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
var
   z : TScriptZipWrite;
begin
   z:=TScriptZipWrite.Create(Info.ParamAsString[0]);
   ExtObject:=z;
end;

procedure TdwsZipLib.dwsZipClassesTZipWriterMethodsAddDataEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   z : TScriptZipWrite;
   buf : RawByteString;
   nameInZip : String;
begin
   z:=TScriptZipWrite(ExtObject);

   buf:=Info.ParamAsDataString[0];
   nameInZip:=Info.ParamAsString[2];

   z.AddData(Pointer(buf), Length(buf), Info.ParamAsInteger[1], nameInZip);
end;

procedure TdwsZipLib.dwsZipClassesTZipWriterMethodsAddFileEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   z : TScriptZipWrite;
   fileName, nameInZip : String;
begin
   z:=TScriptZipWrite(ExtObject);

   fileName:=Info.ParamAsString[0];
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

   z.AddFromZip(reader.Entry[index]);
end;

end.
