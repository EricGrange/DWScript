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
  private
    { Private declarations }
    FOnCreateIniFile : TdwsIniFileConstructor;

  public
    { Public declarations }
    property OnCreateIniFile : TdwsIniFileConstructor read FOnCreateIniFile write FOnCreateIniFile;
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
   else ExtObject := TIniFile.Create(fileName);
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
