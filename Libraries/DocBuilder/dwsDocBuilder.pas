unit dwsDocBuilder;

interface

uses
  System.SysUtils, System.Classes, dwsUnitSymbols, dwsExprs, dwsCompiler,
  dwsComp, dwsHtmlFilter, dwsSymbols, dwsUtils, dwsXPlatform;

type
  TDocumentationBuilder = class;

  TBuildContent = procedure(Sender: TDocumentationBuilder; const FileName
    : TFileName; const Symbol: TSymbol; var Content: string) of object;

  TSymbolUnit = class(TdwsUnit)
  private
    FSymbol: TSymbol;
    FLevel: Integer;
    FScriptObj: IScriptObj;
    procedure GetSymbol(info: TProgramInfo);
    procedure GetLevel(info: TProgramInfo);
    procedure GetCaptionEval(info: TProgramInfo; ExtObject: TObject);
    procedure GetDescriptionEval(info: TProgramInfo; ExtObject: TObject);
    procedure GetNameEval(info: TProgramInfo; ExtObject: TObject);
    procedure GetIsBaseType(info: TProgramInfo; ExtObject: TObject);
    procedure GetIsType(info: TProgramInfo; ExtObject: TObject);
    procedure GetIsFuncSymbol(info: TProgramInfo; ExtObject: TObject);
    procedure GetIsOverloaded(info: TProgramInfo; ExtObject: TObject);
    procedure GetParamsDescription(info: TProgramInfo; ExtObject: TObject);
  public
    constructor Create(Symbol: TSymbol; Level: Integer); reintroduce;
  end;

  TDocumentationBuilder = class
  strict private
    FProgram: IdwsProgram;
    FCompiler: TDelphiWebScript;
    FHtmlFilter: TdwsHtmlFilter;
    FTemplateSource: string;
    FDirectory: string;
    FFileExtension: string;
  private
    FOnBeginBuildContent: TBuildContent;
    FOnAfterBuildContent: TBuildContent;
  protected
    procedure BuildUnit(const Directory: string; UnitSymbol: TUnitSymbol);
    procedure BuildClass(const Directory: string; ClassSymbol: TClassSymbol);
    function BuildContent(Level: Integer; const Symbol: TSymbol = nil): string;
  public
    constructor Create(AProgram: IdwsProgram);
    destructor Destroy; override;

    procedure Build(const Directory: string);
    procedure GenerateTemplate;
    procedure LoadTemplate(const FileName: TFileName; AdaptFileExtension: Boolean = True);

    property TemplateSource: string read FTemplateSource write FTemplateSource;
    property FileExtension: string read FFileExtension write FFileExtension;

    property OnBeginBuildContent: TBuildContent read FOnBeginBuildContent write FOnBeginBuildContent;
    property OnAfterBuildContent: TBuildContent read FOnAfterBuildContent write FOnAfterBuildContent;
  end;

implementation

resourcestring
  RStrInvalidDirectory = 'Directory "%s" does not exist!';
  RStrTemplateDoesNotExist = 'Template file %s does not exist!';

type
  TUnitSymbolHelper = class helper for TUnitSymbol
  public
    procedure CollectSymbols(SymbolClass: TSymbolClass;
      SymbolList: TSimpleSymbolList);
  end;

{ TUnitSymbolHelper }

procedure TUnitSymbolHelper.CollectSymbols(SymbolClass: TSymbolClass;
  SymbolList: TSimpleSymbolList);
var
  CurrentSymbol: TSymbol;
begin
  for CurrentSymbol in Table do
    if CurrentSymbol.InheritsFrom(SymbolClass) then
      SymbolList.Add(CurrentSymbol);
end;

{ TSymbolUnit }

constructor TSymbolUnit.Create(Symbol: TSymbol; Level: Integer);
begin
  inherited Create(nil);

  FSymbol := Symbol;
  FLevel := Level;

  with Classes.Add do
  begin
    Name := 'TSymbol';
    with Methods.Add do
    begin
      Name := 'GetCaption';
      ResultType := 'String';
      Visibility := cvPrivate;
      OnEval := GetCaptionEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetDescription';
      ResultType := 'String';
      Visibility := cvPrivate;
      OnEval := GetDescriptionEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetName';
      ResultType := 'String';
      Visibility := cvPrivate;
      OnEval := GetNameEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsBaseType';
      ResultType := 'Boolean';
      OnEval := GetIsBaseType;
    end;
    with Methods.Add do
    begin
      Name := 'IsType';
      ResultType := 'Boolean';
      OnEval := GetIsType;
    end;
    with Methods.Add do
    begin
      Name := 'IsFuncSymbol';
      ResultType := 'Boolean';
      OnEval := GetIsFuncSymbol;
    end;

    // properties
    with Properties.Add do
    begin
      Name := 'Caption';
      DataType := 'String';
      ReadAccess := 'GetCaption';
    end;
    with Properties.Add do
    begin
      Name := 'Description';
      DataType := 'String';
      ReadAccess := 'GetDescription';
    end;
    with Properties.Add do
    begin
      Name := 'Name';
      DataType := 'String';
      ReadAccess := 'GetName';
    end;
  end;

  with Classes.Add do
  begin
    Name := 'TUnitSymbol';
    Ancestor := 'TSymbol';
  end;

  with Classes.Add do
  begin
    Name := 'TCompositeTypeSymbol';
    Ancestor := 'TSymbol';

(*
    with Methods.Add do
    begin

    end;
*)
  end;

  with Classes.Add do
  begin
    Name := 'TStructuredTypeSymbol';
    Ancestor := 'TCompositeTypeSymbol';
  end;

  with Classes.Add do
  begin
    Name := 'TClassSymbol';
    Ancestor := 'TStructuredTypeSymbol';
  end;

  with Classes.Add do
  begin
    Name := 'TConstSymbol';
    Ancestor := 'TSymbol';
  end;

  with Classes.Add do
  begin
    Name := 'TTypeSymbol';
    Ancestor := 'TSymbol';

    with Methods.Add do
    begin
      Name := 'IsOverloaded';
      ResultType := 'Boolean';
      OnEval := GetIsOverloaded;
    end;
  end;

  with Classes.Add do
  begin
    Name := 'TFuncSymbol';
    Ancestor := 'TTypeSymbol';

    with Methods.Add do
    begin
      Name := 'IsOverloaded';
      ResultType := 'Boolean';
      OnEval := GetIsOverloaded;
    end;
    with Methods.Add do
    begin
      Name := 'ParamsDescription';
      ResultType := 'String';
      OnEval := GetParamsDescription;
    end;
  end;

  with Functions.Add do
  begin
    Name := 'Symbol';
    ResultType := 'TSymbol';
    OnEval := GetSymbol;
  end;
  with Functions.Add do
  begin
    Name := 'Level';
    ResultType := 'Integer';
    OnEval := GetLevel;
  end;

  FScriptObj := nil;
end;

procedure TSymbolUnit.GetDescriptionEval(info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsString := TSymbol(ExtObject).Description;
end;

procedure TSymbolUnit.GetIsBaseType(info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(TSymbol(ExtObject).BaseType);
end;

procedure TSymbolUnit.GetIsFuncSymbol(info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsBoolean := TSymbol(ExtObject).IsFuncSymbol;
end;

procedure TSymbolUnit.GetIsOverloaded(info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsBoolean := TFuncSymbol(ExtObject).IsOverloaded;
end;

procedure TSymbolUnit.GetIsType(info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsBoolean := TSymbol(ExtObject).IsType;
end;

procedure TSymbolUnit.GetLevel(info: TProgramInfo);
begin
  info.ResultAsInteger := FLevel;
end;

procedure TSymbolUnit.GetNameEval(info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsString := TSymbol(ExtObject).Name;
end;

procedure TSymbolUnit.GetParamsDescription(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsString := TFuncSymbol(ExtObject).ParamsDescription;
end;

procedure TSymbolUnit.GetCaptionEval(info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsString := TSymbol(ExtObject).Caption;
end;

procedure TSymbolUnit.GetSymbol(info: TProgramInfo);
begin
  if not Assigned(FScriptObj) then
    FScriptObj := info.RegisterExternalObject(FSymbol, False, False);

  info.ResultAsVariant := FScriptObj;
end;


{ TDocumentationBuilder }

constructor TDocumentationBuilder.Create(AProgram: IdwsProgram);
begin
  FProgram := AProgram;

  FCompiler := TDelphiWebScript.Create(nil);
  FHtmlFilter := TdwsHtmlFilter.Create(nil);
  FCompiler.Config.Filter := FHtmlFilter;

  GenerateTemplate;
end;

destructor TDocumentationBuilder.Destroy;
begin
  FreeAndNil(FHtmlFilter);
  FreeAndNil(FCompiler);
  FProgram := nil;
  inherited;
end;

procedure TDocumentationBuilder.GenerateTemplate;
begin
  with TStringList.Create do
  try
    Add('<!DOCTYPE html>');
    Add('<html>');
    Add('<head>');
    Add('  <title><?pas=Symbol.Name ?></title>');
    Add('</head>');
    Add('');
    Add('<body>');
    Add('<h1 class="Name"><?pas=Symbol.Name ?></h1>');
    Add('<p><?pas=Symbol.Description ?></p>');
    Add('<h2 class="Description">Description</h2>');
    Add('<p>No description available yet!</p>');
    Add('<p class="Footer">Copyright DWS Team - ' +
      'Build on <?pas=FormatDateTime(''yyyy-mmmm-dd'', Now) ?></p>');
    Add('</body>');
    Add('');
    Add('</html>');
    FTemplateSource := Text;
  finally
    Free;
  end;

  FFileExtension := '.html';
end;

procedure TDocumentationBuilder.LoadTemplate(const FileName: TFileName;
  AdaptFileExtension: Boolean);
begin
  if not FileExists(FileName) then
    raise Exception.CreateFmt(RStrTemplateDoesNotExist, [FileName]);

  // load template source
  TemplateSource := LoadTextFromFile(FileName);

  // eventually extract file extension
  if AdaptFileExtension then
    FFileExtension := ExtractFileExt(FileName)
end;

function TDocumentationBuilder.BuildContent(Level: Integer; const Symbol: TSymbol): string;
var
  prog: IdwsProgram;
  SymbolUnit: TSymbolUnit;
begin
  SymbolUnit := TSymbolUnit.Create(Symbol, Level);
  SymbolUnit.UnitName := 'Help';
  SymbolUnit.Script := FCompiler;
  try
    prog := FCompiler.Compile(FTemplateSource);
    if prog.Msgs.HasErrors then
      Result := prog.Msgs.AsInfo
    else
      Result := prog.Execute.Result.ToString;
  finally
    prog := nil;
    FreeAndNil(SymbolUnit);
  end;
end;

procedure TDocumentationBuilder.Build(const Directory: string);
var
  Symbol: TSymbol;
  UnitsDir: string;
begin
  // eventually create directory if not present
  if not DirectoryExists(Directory) then
    CreateDir(Directory);

  FDirectory := Directory;

  // check for errors
  if FProgram.Msgs.HasErrors then
  begin
    OutputDebugString(FProgram.Msgs.AsInfo);
    Exit;
  end;

  // check if symbols are available at all
  if FProgram.Table.Count = 0 then
    Exit;

  UnitsDir := Directory + 'Units\';

  // create unit directoy
  if not DirectoryExists(UnitsDir) then
    CreateDir(UnitsDir);

  // build unit list
  for Symbol in FProgram.Table do
    if Symbol is TUnitSymbol then
      BuildUnit(UnitsDir, TUnitSymbol(Symbol));
end;

procedure TDocumentationBuilder.BuildUnit(const Directory: string; UnitSymbol: TUnitSymbol);
var
  Content: string;
  Symbol: TSymbol;

  procedure BuildContentFileSimple(const Directory: string);
  var
    FileName: TFileName;
  begin
    // create directoy
    if not DirectoryExists(Directory) then
      CreateDir(Directory);

    // build filename
    FileName := Directory + Symbol.Name + FFileExtension;

    // eventually call before build content
    if Assigned(FOnBeginBuildContent) then
      FOnBeginBuildContent(Self, FileName, Symbol, Content);

    // build documentation content
    Content := BuildContent(2, Symbol);

    // eventually call after build content
    if Assigned(FOnAfterBuildContent) then
      FOnAfterBuildContent(Self, FileName, Symbol, Content);

    // save content to file
    SaveTextToUTF8File(FileName, Content);
  end;

var
  FileName: TFileName;
  UnitDir: string;
begin
  // check if unit is empty
  if (UnitSymbol.Table.Count) = 0 then
    Exit;

  UnitDir := Directory + UnitSymbol.Name + '\';

  // create unit directoy
  if not DirectoryExists(UnitDir) then
    CreateDir(UnitDir);

  for Symbol in UnitSymbol.Table do
  begin
    if Symbol is TClassSymbol then
    begin
      // create classes directoy
      if not DirectoryExists(UnitDir + 'Classes\') then
        CreateDir(UnitDir + 'Classes\');

      BuildClass(UnitDir + 'Classes\', TClassSymbol(Symbol));
    end
    else if Symbol is TConstSymbol then
    begin
      // build documentation for constant symbol
      BuildContentFileSimple(UnitDir + 'Constants\');
    end
    else if Symbol is TFuncSymbol then
    begin
      // build documentation for function symbol
      BuildContentFileSimple(UnitDir + 'Routines\');
    end
    else if Symbol is TTypeSymbol then
    begin
      // build documentation for function symbol
      BuildContentFileSimple(UnitDir + 'Types\');
    end;
  end;

  FileName := UnitDir + 'index' + FFileExtension;

  // eventually call before build content
  if Assigned(FOnBeginBuildContent) then
    FOnBeginBuildContent(Self, FileName, UnitSymbol, Content);

  // build documentation for unit symbol
  Content := BuildContent(1, UnitSymbol);

  // eventually call after build content
  if Assigned(FOnAfterBuildContent) then
    FOnAfterBuildContent(Self, FileName, UnitSymbol, Content);

  SaveTextToUTF8File(FileName, Content);
end;

procedure TDocumentationBuilder.BuildClass(const Directory: string;
  ClassSymbol: TClassSymbol);
var
  Content: string;
  MemberSymbol: TSymbol;

  procedure BuildContentFileSimple(const Directory: string);
  var
    FileName: TFileName;
  begin
    // create directoy
    if not DirectoryExists(Directory) then
      CreateDir(Directory);

    // build filename
    FileName := Directory + MemberSymbol.Name + FFileExtension;

    // eventually call before build content
    if Assigned(FOnBeginBuildContent) then
      FOnBeginBuildContent(Self, FileName, MemberSymbol, Content);

    // build documentation content
    Content := BuildContent(4, MemberSymbol);

    // eventually call after build content
    if Assigned(FOnAfterBuildContent) then
      FOnAfterBuildContent(Self, FileName, MemberSymbol, Content);

    // save content to file
    SaveTextToUTF8File(FileName, Content);
  end;

var
  FileName: TFileName;
  ClassDir: string;
begin
  ClassDir := Directory + ClassSymbol.Name + '\';

  // check if directory exists
  if not DirectoryExists(ClassDir) then
    CreateDir(ClassDir);

  for MemberSymbol in ClassSymbol.Members do
  begin
    if MemberSymbol is TMethodSymbol then
      BuildContentFileSimple(ClassDir + 'Methods\')
    else if MemberSymbol is TPropertySymbol then
      BuildContentFileSimple(ClassDir + 'Properties\');
  end;

  FileName := ClassDir + 'index' + FFileExtension;

  // eventually call before build content
  if Assigned(FOnBeginBuildContent) then
    FOnBeginBuildContent(Self, FileName, ClassSymbol, Content);

  // build documentation for class symbol
  Content := BuildContent(3, ClassSymbol);

  // eventually call after build content
  if Assigned(FOnAfterBuildContent) then
    FOnAfterBuildContent(Self, FileName, ClassSymbol, Content);

  SaveTextToUTF8File(FileName, Content);
end;

end.
