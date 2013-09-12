program DocBuilder;

{$APPTYPE CONSOLE}

uses
  System.SysUtils, System.Classes, dwsComp, dwsExprs, dwsSymbols, dwsErrors,
  dwsStrings, dwsUnitSymbols, dwsXPlatform, dwsCompiler, dwsFunctions, dwsUtils,
  dwsDocBuilder in '..\..\..\Libraries\DocBuilder\dwsDocBuilder.pas',
  dwsSymbolsLibModule in '..\..\..\Libraries\SymbolsLib\dwsSymbolsLibModule.pas';

resourcestring
  RStrParameterParsingError = 'Parameter parsing error';
  RStrColonOrEqualExpected = ''':'' or ''='' expected!';

type
  TDocCompiler = class
  private
    FFiles: TStringList;
    FLibPaths: TStringList;
    FOutputPath: string;
    FTemplate: TFileName;
    FSelfDocumentation: Boolean;
    procedure OnIncludeHandler(const AIncludeName: string; var AScriptSource: string); virtual;
    function OnNeedUnitHandler(const AUnitName: string; var AUnitSource: string): IdwsUnit; virtual;
    procedure GetFile(const FileName: TFileName; var FileContent: string);
  protected
    function ParseCommandLine: Boolean;
    procedure BuildDocForProgram(Prog: IdwsProgram);
  public
    constructor Create;
    destructor Destroy; override;

    procedure BuildDocumentation;
    procedure BeforeBuildContentHandler(Sender: TDocumentationBuilder;
      const FileName: TFileName; const Symbol: TSymbol; var Content: string);
  end;

{ TDocCompiler }

constructor TDocCompiler.Create;
begin
  FFiles := TStringList.Create;
  FLibPaths := TStringList.Create;
  FLibPaths.Add(ExtractFilePath(ParamStr(0)));
  FOutputPath := ExtractFilePath(ParamStr(0)) + 'Documentation\';
  FTemplate := '';
end;

destructor TDocCompiler.Destroy;
begin
  FFiles.Free;
  FLibPaths.Free;
  inherited;
end;

procedure TDocCompiler.GetFile(const FileName: TFileName;
  var FileContent: string);
var
  CurrentFileName: TFileName;
  Path: string;
begin
  // search unit in passed files
  for CurrentFileName in FFiles do
    if UnicodeSameText(CurrentFileName, FileName) then
    begin
      FileContent := LoadTextFromFile(FileName);
      Exit;
    end;

  // search unit in passed files
  for Path in FLibPaths do
  begin
    CurrentFileName := IncludeTrailingPathDelimiter(Path) + FileName;
    if FileExists(CurrentFileName) then
    begin
      FileContent := LoadTextFromFile(CurrentFileName);
      Exit;
    end;
  end;
end;

procedure TDocCompiler.OnIncludeHandler(const AIncludeName: string;
  var AScriptSource: string);
begin
  GetFile(AIncludeName, AScriptSource);
end;

function TDocCompiler.OnNeedUnitHandler(const AUnitName: string;
  var AUnitSource: string): IdwsUnit;
begin
  GetFile(AUnitName + '.pas', AUnitSource);
end;

function TDocCompiler.ParseCommandLine: Boolean;
var
  Index: Integer;
  CurParam: string;
  AddParam: Boolean;

  procedure EnsureMinLength(MinLength: Integer);
  begin
    if Length(CurParam) < MinLength then
      raise Exception.Create(RStrParameterParsingError);
  end;

begin
  if ParamCount <= 0 then
    Exit(False);

  Result := True;
  try
    for Index := 1 to ParamCount do
    begin
      CurParam := ParamStr(Index);

      Assert(Length(CurParam) > 0);

      // check if current parameter is optional
      if CurParam[1] = '/' then
      begin
        EnsureMinLength(2);

        case CurParam[2] of
          'l':
            begin
              // library path
              EnsureMinLength(3);
              if not CharInSet(CurParam[3], [':', '=']) then
                raise Exception.Create(RStrColonOrEqualExpected);

              AddParam := CurParam[3] = ':';
              Delete(CurParam, 1, 3);

              if not AddParam then
                FLibPaths.Clear;

              FLibPaths.Add(IncludeTrailingPathDelimiter(ExpandFileName(CurParam)));
            end;
          'o':
            begin
              // output path
              EnsureMinLength(3);
              if not CharInSet(CurParam[3], [':', '=']) then
                raise Exception.Create(RStrColonOrEqualExpected);

              Delete(CurParam, 1, 3);
              FOutputPath := IncludeTrailingPathDelimiter(ExpandFileName(CurParam));
            end;
          't':
            begin
              EnsureMinLength(3);
              if not CharInSet(CurParam[3], [':', '=']) then
                raise Exception.Create(RStrColonOrEqualExpected);
              Delete(CurParam, 1, 3);
              FTemplate := CurParam;
            end;
          'h', '?': Exit(False);
        end;
      end
      else
      begin
        if LowerCase(CurParam) = 'self' then
          FSelfDocumentation := True
        else begin
          FFiles.Add(CurParam);
          FLibPaths.Add(ExtractFilePath(CurParam));
        end;
      end;
    end;
    if FFiles.Count = 0 then
      raise Exception.Create('No units to compile!');
  except
    on E: Exception do
    begin
      Result := False;
      Writeln('ERROR: ' + E.Message);
    end;
  end;
end;

procedure TDocCompiler.BeforeBuildContentHandler(Sender: TDocumentationBuilder;
  const FileName: TFileName; const Symbol: TSymbol; var Content: string);
begin
  WriteLn('Building: ' + ExtractRelativePath(ExtractFilePath(ParamStr(0)),
    FileName));
end;

procedure TDocCompiler.BuildDocForProgram(Prog: IdwsProgram);
begin
  // check if program is available at all
  if not Assigned(Prog) then
    raise Exception.Create('Unknown Compile Error!');

  // check if an error has occured
  Assert(Assigned(Prog.Msgs));
  if Prog.Msgs.HasErrors then
  begin
    WriteLn('Compile Error: ' + Prog.Msgs.AsInfo);
    Exit;
  end;

  with TDocumentationBuilder.Create(Prog) do
  try
    OnBeginBuildContent := BeforeBuildContentHandler;

    // eventually load template from file
    if FileExists(FTemplate) then
      LoadTemplate(FTemplate)
    else
      GenerateTemplate;

    Writeln('Building Documentation...');
    Build(FOutputPath);
  finally
    Free;
  end;
end;

procedure TDocCompiler.BuildDocumentation;
var
  Index: Integer;
  Source: string;
  SymbolUnit: TSymbolUnit;
  Compiler: TDelphiWebScript;
begin
  // eventually get directory to default
  if FOutputPath = '' then
    FOutputPath := ExtractFilePath(ParamStr(0)) + 'Documentation\';

  Compiler := TDelphiWebScript.Create(nil);
  try
    Compiler.Config.CompilerOptions := [coSymbolDictionary, coContextMap,
      coAssertions, coHintsDisabled, coWarningsDisabled, coAllowClosures];
    Compiler.OnNeedUnit := OnNeedUnitHandler;
    Compiler.OnInclude := OnIncludeHandler;

    if FSelfDocumentation then
    begin
      SymbolUnit := TSymbolUnit.Create(nil, 0);
      try
        SymbolUnit.UnitName := 'Help';
        SymbolUnit.Script := Compiler;

        Writeln('Compiling...');
        BuildDocForProgram(Compiler.Compile('uses Help;'));
      finally
        FreeAndNil(SymbolUnit);
      end;
    end
    else
    begin
      Source := 'uses ' + ChangeFileExt(FFiles.Strings[0], '');
      for Index := 1 to FFiles.Count - 1 do
        Source := Source + ', ' + ChangeFileExt(FFiles.Strings[Index], '');

      Writeln('Compiling...');
      BuildDocForProgram(Compiler.Compile(Source + ';'));
    end;
  finally
    Compiler.Free;
  end;
end;

procedure ShowUsage;
var
  ExeName: TFileName;
begin
  ExeName := ExtractFileName(ParamStr(0));
  WriteLn('Usage: ' + ExeName + 'unitfile(s) [options]');
  WriteLn;
  WriteLn('Example: ' + ExeName + ' Test.pas AnotherUnit.pas /o=.\Docs');
  WriteLn;
  WriteLn('Options');
  WriteLn;
  WriteLn('  /l:Path           Add library path');
  WriteLn('  /l=Path           Set library path');
  WriteLn('  /o=Path           Set output path');
  WriteLn('  /t=FileName       Use this template instead of built-in template');
  WriteLn('  /h or /?          Show this information');
  Writeln;
  WriteLn('Self documentation: ' + ExeName + ' Self /o=.\Docs');
  Writeln;
end;

begin
  try
    Writeln('DWS Automatic Documentation Builder');
    Writeln('===================================');
    Writeln;

    with TDocCompiler.Create do
    try
      if ParseCommandLine then
        BuildDocumentation
      else
        ShowUsage;
    finally
      Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
