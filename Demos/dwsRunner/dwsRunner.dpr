program dwsRunner;

{$SetPEFlags $0001}

{$IFNDEF VER200} // delphi 2009
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}
{$APPTYPE CONSOLE}

{$R *.dres}

uses
   Windows, Classes, SysUtils,
   dwsXPlatform, dwsComp, dwsCompiler, dwsExprs, dwsUtils, dwsFunctions,
   SynZip,
   dwsMathFunctions, dwsStringFunctions, dwsTimeFunctions, dwsVariantFunctions,
   dwsFileFunctions,
   dwsClassesLibModule, dwsZipLibModule, dwsEncodingLibModule, dwsCryptoLibModule,
   dwsWebLibModule, dwsDatabaseLibModule, dwsComConnector, dwsJSONConnector,
   dwsSynSQLiteDatabase;

type
   TZipProject = class
      zip : TZipRead;
      constructor Create(const zipFile : String); overload;
      destructor Destroy; override;
      function  Attach(script : TDelphiWebScript) : String;
      procedure DoInclude(const scriptName: UnicodeString; var scriptSource: UnicodeString);
      function  DoNeedUnit(const unitName : UnicodeString; var unitSource : UnicodeString) : IdwsUnit;
   end;

constructor TZipProject.Create(const zipFile : String);
begin
   zip := TZipRead.Create(zipFile);
end;

destructor TZipProject.Destroy;
begin
   zip.Free;
end;

function TZipProject.Attach(script : TDelphiWebScript) : String;
begin
   script.OnNeedUnit:=DoNeedUnit;
   script.OnInclude:=DoInclude;
   if zip.Count=1 then
      DoInclude(zip.Entry[0].zipName, Result)
   else DoInclude('main', Result);
end;

procedure TZipProject.DoInclude(const scriptName: UnicodeString; var scriptSource: UnicodeString);
var
   i : Integer;
begin
   i:=zip.NameToIndex(scriptName);
   if i<0 then
      i:=zip.NameToIndex(scriptName+'.pas');
   if i>=0 then
      scriptSource:=LoadTextFromRawBytes(zip.UnZip(i))
   else raise Exception.CreateFmt('Dependency not found: "%s"', [scriptName]);
end;

function TZipProject.DoNeedUnit(const unitName : UnicodeString; var unitSource : UnicodeString) : IdwsUnit;
begin
   Result:=nil;
   DoInclude(unitName, unitSource);
end;

function CreateScript : TDelphiWebScript;
begin
   Result:=TDelphiWebScript.Create(nil);

   TdwsComConnector.Create(Result).Script:=Result;
   TdwsJSONLibModule.Create(Result).Script:=Result;
   TdwsClassesLib.Create(Result).dwsUnit.Script:=Result;
   TdwsEncodingLib.Create(Result).dwsEncoding.Script:=Result;
   TdwsCryptoLib.Create(Result).dwsCrypto.Script:=Result;
   TdwsZipLib.Create(Result).dwsZip.Script:=Result;
   TdwsWebLib.Create(Result).dwsWeb.Script:=Result;
   TdwsDatabaseLib.Create(Result).dwsDatabase.Script:=Result;
end;

procedure WriteHeader;
begin
   Writeln('dwsRunner - sample code runner for DWScript');
   Writeln('');
end;

procedure MakeExe;
var
   zw : TZipWrite;
   sourceName, zipFileName, exeName : String;
   hUpdate : THandle;
   buf : RawByteString;
   fs : TFileStream;
   zip : TZipProject;
   prog : IdwsProgram;
   script : TDelphiWebScript;
   searchRec : TSearchRec;
   found : Integer;
begin
   WriteHeader;

   if ParamCount<2 then begin
      Writeln('Missing zipfile name');
      Exit;
   end;

   sourceName:=ParamStr(2);
   WriteLn('...Starting make for "', sourceName, '"');

   if not StrEndsWith(sourceName, '.zip') then begin
      zipFileName:=ChangeFileExt(sourceName, '.zip');
      WriteLn('...Zipping to "', zipFileName, '"');
      zw:=TZipWrite.Create(zipFileName);
      try
         if DirectoryExists(sourceName) then begin
            sourceName:=IncludeTrailingPathDelimiter(sourceName);
            found:=FindFirst(sourceName+'*.*', faArchive or faReadOnly, searchRec);
            while found=0 do begin
               zw.AddDeflated(sourceName+searchRec.Name, True, 9);
               found:=FindNext(searchRec);
            end;
            FindClose(searchRec);
         end else zw.AddDeflated(sourceName, True, 9);
      finally
         zw.Free;
      end;
   end else begin
      zipFileName:=sourceName;
   end;
   if ParamCount>2 then
      exeName:=ParamStr(3)
   else exeName:=ChangeFileExt(zipFileName, '.exe');

   zip:=TZipProject.Create(zipFileName);
   script:=CreateScript;
   try
      prog:=script.Compile(zip.Attach(script));
      try
         if prog.Msgs.Count>0 then begin
            WriteLn('...Compiled with ', prog.Msgs.Count, ' message(s):');
            WriteLn(prog.Msgs.AsInfo);
            if prog.Msgs.HasErrors then begin
               Write('...Generation aborted');
               Exit;
            end;
         end else begin
            WriteLn('...Compiled without errors.');
         end;
      finally
         prog:=nil;
      end;
   finally
      script.Free;
      zip.Free;
   end;

   if not FileCopy(ParamStr(0), exeName, False) then begin
      Writeln('...Failed to create "', exeName, '"');
   end;

   fs:=TFileStream.Create(zipFileName, fmOpenRead or fmShareDenyNone);
   try
      SetLength(buf, fs.Size);
      if Length(buf)<>0 then
         fs.Read(buf[1], Length(buf));
   finally
      fs.Free;
   end;

   hUpdate:=BeginUpdateResource(PChar(exeName), False);
   try
      UpdateResource(hUpdate, RT_RCDATA, 'SCRIPT', 0, Pointer(buf), Length(buf));
   finally
      EndUpdateResource(hUpdate, False);
   end;

   WriteLn('..."', exeName, '" generated successfully!');
end;

var
   fileName : String;
   source : String;
   script : TDelphiWebScript;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   i, paramOffset : Integer;
   params : array of Variant;
   zip : TZipProject;
   zr : TZipRead;
begin
   zr:=TZipRead.Create(HInstance, 'SCRIPT', RT_RCDATA);
   if zr.Count=0 then begin
      FreeAndNil(zr);
      zip:=nil;
      paramOffset:=2;
   end else begin
      zip:=TZipProject.Create;
      zip.zip:=zr;
      paramOffset:=1;
   end;

   if zip=nil then begin
      if ParamCount<1 then begin
         WriteHeader;
         Writeln('Run a simple script with:');
         Writeln('   dwsRunner <sourcefile> [param1] [param2] ... [paramN]');
         Writeln('');
         Writeln('Run a zip project with (starts from "main.pas" in the zip):');
         Writeln('   dwsRunner <zipfile> [param1] [param2] ... [paramN]');
         Writeln('');
         Writeln('Bundle a zip project into an executable:');
         Writeln('   dwsRunner make <zipFile|sourcefile> [exeName]');
         Exit;
      end;
      fileName:=ParamStr(1);
      if fileName='make' then begin
         MakeExe;
         exit;
      end;
      if not FileExists(fileName) then begin
         Writeln('File "', fileName, '" not found.');
         Exit;
      end;
   end;
   try
      script:=CreateScript;
      try
         if zip=nil then begin
            if StrEndsWith(fileName, '.zip') then begin
               zip:=TZipProject.Create(ParamStr(1));
            end else begin
               source:=LoadTextFromFile(ParamStr(1));
            end;
         end;
         if zip<>nil then
            source:=zip.Attach(script);

         prog:=script.Compile(source);

         if prog.Msgs.Count>0 then begin
            if (zip=nil) or prog.Msgs.HasErrors then
               Writeln(prog.Msgs.AsInfo);
            if prog.Msgs.HasErrors then Exit;
         end;

         SetLength(params, ParamCount-paramOffset+2);
         params[0]:=ParamStr(0);
         for i:=paramOffset to ParamCount do
            params[i-paramOffset+1]:=ParamStr(i);
         exec:=prog.ExecuteParam(params);
         Writeln(exec.Result.ToString);
         if exec.Msgs.Count>0 then
            Writeln(exec.Msgs.AsInfo);
      finally
         zip.Free;
         script.Free;
      end;
   except
      on E: Exception do
         Writeln(E.ClassName, ': ', E.Message);
   end;
end.
