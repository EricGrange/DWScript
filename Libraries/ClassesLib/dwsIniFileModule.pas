unit dwsIniFileModule;

interface

uses
  SysUtils, Classes, dwsComp, IniFiles,
  dwsExprs, dwsWebUtils, dwsUtils, dwsXPlatform, dwsSymbols;

type
  TdwsIniFileConstructor = function (const fileName : String) : TCustomIniFile of object;

  TdwsIniFileLib = class(TDataModule)
    dwsIniFile: TdwsUnit;
    procedure dwsIniFileClassesTIniFileConstructorsCreateEval(
      Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsIniFileClassesTIniFileCleanUp(ExternalObject: TObject);
    procedure dwsIniFileClassesTIniFileMethodsFileNameEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsIniFileClassesTIniFileMethodsEraseSectionEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsIniFileClassesTIniFileMethodsDeleteKeyEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsIniFileClassesTIniFileMethodsReadStringEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsIniFileClassesTIniFileMethodsWriteStringEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsIniFileClassesTIniFileMethodsReadSectionsEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsIniFileClassesTIniFileMethodsReadSectionNamesEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsIniFileClassesTIniFileMethodsGetEncodingEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsIniFileClassesTIniFileMethodsSetEncodingEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsIniFileClassesTIniFileConstructorsCreateInMemoryEval(
      Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsIniFileClassesTIniFileMethodsToStringEval(Info: TProgramInfo;
      ExtObject: TObject);
  private
    { Private declarations }
    FOnCreateIniFile : TdwsIniFileConstructor;

    function GetScript : TDelphiWebScript; inline;
    procedure SetScript(const val : TDelphiWebScript); inline;

  public
   { Public declarations }
    property OnCreateIniFile : TdwsIniFileConstructor read FOnCreateIniFile write FOnCreateIniFile;

    property Script : TDelphiWebScript read GetScript write SetScript;

  end;

implementation

{$R *.dfm}

procedure TdwsIniFileLib.dwsIniFileClassesTIniFileConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
var
   fileName : String;
begin
   fileName := Info.ParamAsFileName[0];
   if Assigned(FOnCreateIniFile) then
      ExtObject := FOnCreateIniFile(fileName)
   else begin
      ExtObject := TMemIniFile.Create(fileName);
      TMemIniFile(ExtObject).AutoSave := True;
   end;
end;

procedure TdwsIniFileLib.dwsIniFileClassesTIniFileConstructorsCreateInMemoryEval(
  Info: TProgramInfo; var ExtObject: TObject);
var
   sl : TStringList;
begin
   ExtObject := TMemIniFile.Create('');
   TMemIniFile(ExtObject).AutoSave := False;
   sl := TStringList.Create;
   try
      sl.Text := Info.ParamAsString[0];
      TMemIniFile(ExtObject).SetStrings(sl);
   finally
      sl.Free;
   end;
end;

procedure TdwsIniFileLib.dwsIniFileClassesTIniFileCleanUp(
  ExternalObject: TObject);
begin
   ExternalObject.Free;
end;

procedure TdwsIniFileLib.dwsIniFileClassesTIniFileMethodsFileNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := (ExtObject as TCustomIniFile).FileName;
end;

procedure TdwsIniFileLib.dwsIniFileClassesTIniFileMethodsGetEncodingEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   encoding : TEncoding;
begin
   if ExtObject is TMemIniFile then
      encoding := TMemIniFile(ExtObject).Encoding
   else encoding := nil;
   if encoding <> nil then
      Info.ResultAsString := encoding.MIMEName
   else Info.ResultAsString := '';
end;

procedure TdwsIniFileLib.dwsIniFileClassesTIniFileMethodsEraseSectionEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   (ExtObject as TCustomIniFile).EraseSection(Info.ParamAsString[0]);
end;

procedure TdwsIniFileLib.dwsIniFileClassesTIniFileMethodsDeleteKeyEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   (ExtObject as TCustomIniFile).DeleteKey(Info.ParamAsString[0], Info.ParamAsString[1]);
end;

procedure TdwsIniFileLib.dwsIniFileClassesTIniFileMethodsReadStringEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := (ExtObject as TCustomIniFile).ReadString(
      Info.ParamAsString[0], Info.ParamAsString[1], Info.ParamAsString[2]
   );
end;

procedure TdwsIniFileLib.dwsIniFileClassesTIniFileMethodsSetEncodingEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   encoding : TEncoding;
begin
   if ExtObject is TMemIniFile then begin
      encoding := TEncoding.GetEncoding(Info.ParamAsString[0]);
      TMemIniFile(ExtObject).Encoding := encoding;
   end else raise Exception.Create('Setting encoding not supported');
end;

procedure TdwsIniFileLib.dwsIniFileClassesTIniFileMethodsToStringEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   sl : TStringList;
begin
   sl := TStringList.Create;
   try
      TMemIniFile(ExtObject).GetStrings(sl);
      Info.ResultAsString := sl.Text;
   finally
      sl.Free;
   end;
end;

// GetScript
//
function TdwsIniFileLib.GetScript : TDelphiWebScript;
begin
   Result := dwsIniFile.Script;
end;

// SetScript
//
procedure TdwsIniFileLib.SetScript(const val : TDelphiWebScript);
begin
   dwsIniFile.Script := val;
end;

procedure TdwsIniFileLib.dwsIniFileClassesTIniFileMethodsWriteStringEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   (ExtObject as TCustomIniFile).WriteString(
      Info.ParamAsString[0], Info.ParamAsString[1], Info.ParamAsString[2]
   );
end;

procedure TdwsIniFileLib.dwsIniFileClassesTIniFileMethodsReadSectionsEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   s : TStrings;
begin
   s := TStringList.Create;
   try
      (ExtObject as TCustomIniFile).ReadSections(s);
      Info.SetResultAsStringArray(s);
   finally
      s.Free;
   end;
end;

procedure TdwsIniFileLib.dwsIniFileClassesTIniFileMethodsReadSectionNamesEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   s : TStrings;
begin
   s := TStringList.Create;
   try
      (ExtObject as TCustomIniFile).ReadSection(Info.ParamAsString[0], s);
      Info.SetResultAsStringArray(s);
   finally
      s.Free;
   end;
end;

end.
