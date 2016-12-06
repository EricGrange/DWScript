unit dwsDocBuilder;

interface

uses
  SysUtils, Classes, dwsUnitSymbols, dwsExprs, dwsCompiler, dwsComp,
  dwsHtmlFilter, dwsSymbols, dwsErrors, dwsUtils, dwsXPlatform,
  {$IFDEF UseDebugger}
  dwsDebugger,
  {$ENDIF}
  dwsSymbolsLibModule, dwsClassesLibModule, dwsDataContext, dwsScriptSource;

type
  TDocumentationBuilder = class;

  TBuildContentEvent = procedure(Sender: TDocumentationBuilder;
    const FileName: TFileName; const Symbol: TSymbol;
    var Content: string) of object;

  TSymbolUnit = class(TdwsUnit)
  private
    FSymbol: TSymbol;
    FLevel: Integer;
    FScriptObj: IScriptObj;
    procedure GetSymbol(info: TProgramInfo);
    procedure GetLevel(info: TProgramInfo);
    procedure GetFileName(info: TProgramInfo);
    {$REGION 'Eval Declarations for TSymbol'}
    procedure TSymbolGetCaptionEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolGetDescriptionEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolIsBaseTypeEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolIsFuncSymbolEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolIsTypeEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolGetNameEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolGetTypeEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolGetQualifiedNameEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TResourceString'}
    procedure TResourceStringSymbolGetValueEval(info: TProgramInfo; ExtObject: TObject);
    procedure TResourceStringSymbolGetIndexEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TConstSymbol'}
    procedure TConstSymbolGetDataEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TParamSymbol'}
    procedure TParamSymbolGetSameParamEval(info: TProgramInfo; ExtObject: TObject);
    procedure TParamSymbolWithDefaultValueGetDefaultValueEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TDataSymbol'}
    procedure TDataSymbolHasExternalNameEval(info: TProgramInfo; ExtObject: TObject);
    procedure TDataSymbolGetExternalNameEval(info: TProgramInfo; ExtObject: TObject);
    procedure TDataSymbolGetLevelEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TTypeSymbol'}
    procedure TTypeSymbolGetDeprecatedMessageEval(info: TProgramInfo; ExtObject: TObject);
    procedure TTypeSymbolGetIsDeprecatedEval(info: TProgramInfo; ExtObject: TObject);
    procedure TTypeSymbolHasMetaSymbolEval(info: TProgramInfo; ExtObject: TObject);
    procedure TTypeSymbolGetUnAliasedTypeEval(info: TProgramInfo; ExtObject: TObject);
    procedure TTypeSymbolIsOfTypeEval(info: TProgramInfo; ExtObject: TObject);
    procedure TTypeSymbolIsCompatibleEval(info: TProgramInfo; ExtObject: TObject);
    procedure TTypeSymbolGetDistanceToEval(info: TProgramInfo; ExtObject: TObject);
    procedure TTypeSymbolGetSameTypeEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TCompositeTypeSymbol'}
    procedure TCompositeTypeSymbolGetParentEval(info: TProgramInfo; ExtObject: TObject);
    procedure TCompositeTypeSymbolGetUnitSymbolEval(info: TProgramInfo; ExtObject: TObject);
    procedure TCompositeTypeSymbolAllowVirtualMembersEval(info: TProgramInfo; ExtObject: TObject);
    procedure TCompositeTypeSymbolAllowOverloadsEval(info: TProgramInfo; ExtObject: TObject);
    procedure TCompositeTypeSymbolAllowDefaultPropertyEval(info: TProgramInfo; ExtObject: TObject);
    procedure TCompositeTypeSymbolAllowFieldsEval(info: TProgramInfo; ExtObject: TObject);
    procedure TCompositeTypeSymbolAllowAnonymousMethodsEval(info: TProgramInfo; ExtObject: TObject);
    procedure TCompositeTypeSymbolFirstFieldEval(info: TProgramInfo; ExtObject: TObject);
    procedure TCompositeTypeSymbolExternalRootEval(info: TProgramInfo; ExtObject: TObject);
    procedure TCompositeTypeSymbolGetMembersEval(info: TProgramInfo; ExtObject: TObject);
    procedure TCompositeTypeSymbolGetDefaultPropertyEval(info: TProgramInfo; ExtObject: TObject);
    procedure TCompositeTypeSymbolGetIsStaticEval(info: TProgramInfo; ExtObject: TObject);
    procedure TCompositeTypeSymbolGetIsPartialEval(info: TProgramInfo; ExtObject: TObject);
    procedure TCompositeTypeSymbolGetIsExternalEval(info: TProgramInfo; ExtObject: TObject);
    procedure TCompositeTypeSymbolGetIsExternalRootedEval(info: TProgramInfo; ExtObject: TObject);
    procedure TCompositeTypeSymbolGetExternalNameEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TStructuredTypeSymbol'}
    procedure TStructuredTypeSymbolGetIsForwardedEval(info: TProgramInfo; ExtObject: TObject);
    procedure TStructuredTypeSymbolGetMetaSymbolEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TFuncSymbol'}
    procedure TFuncSymbolGetExternalNameEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolGetHasExternalNameEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolGetIsDeprecatedEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolGetIsExternalEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolGetIsForwardedEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolGetIsLambdaEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolGetIsOverloadedEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolGetIsStatelessEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolGetKindEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolGetLevelEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolGetParamsDescriptionEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolGetParamsEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolGetParamTypeEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolGetResultEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolHasParamEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolIsSameOverloadOfEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolIsValidOverloadOfEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TMethodSymbol'}
    procedure TMethodSymbolGetParentMethEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolGetRootParentMethEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolGetSelfSymEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolGetStructSymbolEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolGetVisibilityEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolGetIsAbstractEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolGetIsClassMethodEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolGetIsDefaultEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolGetIsFinalEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolGetIsInterfacedEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolGetIsOverlapEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolGetIsOverrideEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolGetIsStaticEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolGetIsVirtualEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TSymbolTable'}
    procedure TSymbolTableGetCountEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolTableGetParentCountEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolTableGetSymbolEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolTableGetParentEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolTableIsUnitTableEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolTableHasMethodsEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolTableHasOperatorsEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolTableHasClassEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolTableHasSymbolEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolTableIndexOfParentEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolTableFindLocalEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolTableFindTypeLocalEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolTableFindTypeSymbolEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolTableTypeSymbolEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TParamsSymbolTable'}
    procedure TParamsSymbolTableGetSymbolEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TSetOfSymbol'}
    procedure TSetOfSymbolGetMinValueEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSetOfSymbolGetMaxValueEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSetOfSymbolGetCountValueEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TArraySymbol'}
    procedure TArraySymbolGetIndexTypeEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TStaticArraySymbol'}
    procedure TStaticArraySymbolGetIsEmptyArrayEval(info: TProgramInfo; ExtObject: TObject);
    procedure TStaticArraySymbolGetHighBoundEval(info: TProgramInfo; ExtObject: TObject);
    procedure TStaticArraySymbolGetLowBoundEval(info: TProgramInfo; ExtObject: TObject);
    procedure TStaticArraySymbolGetElementCountEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TClassConstSymbol'}
    procedure TClassConstSymbolGetOwnerSymbolEval(info: TProgramInfo; ExtObject: TObject);
    procedure TClassConstSymbolGetVisibilityEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TClassVarSymbol'}
    procedure TClassVarSymbolGetOwnerSymbolEval(info: TProgramInfo; ExtObject: TObject);
    procedure TClassVarSymbolGetVisibilityEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TStructuredTypeMetaSymbol'}
    procedure TStructuredTypeMetaSymbolStructSymbolEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TFieldSymbol'}
    procedure TFieldSymbolStructSymbolEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFieldSymbolVisibilityEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFieldSymbolDefaultValueEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFieldSymbolExternalNameEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFieldSymbolNextFieldEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TRecordSymbol'}
    procedure TRecordSymbolIsDynamicEval(info: TProgramInfo; ExtObject: TObject);
    procedure TRecordSymbolIsFullyDefinedEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TInterfaceSymbol'}
    procedure TInterfaceSymbolGetParentEval(info: TProgramInfo; ExtObject: TObject);
    procedure TInterfaceSymbolMethodCountEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TPropertySymbol'}
    procedure TPropertySymbolOwnerSymbolEval(info: TProgramInfo; ExtObject: TObject);
    procedure TPropertySymbolVisibilityEval(info: TProgramInfo; ExtObject: TObject);
    procedure TPropertySymbolArrayIndicesEval(info: TProgramInfo; ExtObject: TObject);
    procedure TPropertySymbolReadSymEval(info: TProgramInfo; ExtObject: TObject);
    procedure TPropertySymbolWriteSymEval(info: TProgramInfo; ExtObject: TObject);
    procedure TPropertySymbolIsDefaultEval(info: TProgramInfo; ExtObject: TObject);
    procedure TPropertySymbolIndexValueEval(info: TProgramInfo; ExtObject: TObject);
    procedure TPropertySymbolIndexSymEval(info: TProgramInfo; ExtObject: TObject);
    procedure TPropertySymbolDeprecatedMessageEval(info: TProgramInfo; ExtObject: TObject);
    procedure TPropertySymbolIsDeprecatedEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TClassSymbol'}
    procedure TClassSymbolGetParentEval(info: TProgramInfo; ExtObject: TObject);
    procedure TClassSymbolGetIsExplicitAbstractEval(info: TProgramInfo; ExtObject: TObject);
    procedure TClassSymbolGetIsAbstractEval(info: TProgramInfo; ExtObject: TObject);
    procedure TClassSymbolGetIsSealedEval(info: TProgramInfo; ExtObject: TObject);
    procedure TClassSymbolGetIsExternalEval(info: TProgramInfo; ExtObject: TObject);
    procedure TClassSymbolGetIsPartialEval(info: TProgramInfo; ExtObject: TObject);
    procedure TClassSymbolImplementsInterfaceEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TElementSymbol'}
    procedure TElementSymbolEnumerationEval(info: TProgramInfo; ExtObject: TObject);
    procedure TElementSymbolIsUserDefEval(info: TProgramInfo; ExtObject: TObject);
    procedure TElementSymbolValueEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TElementSymbol'}
    procedure TEnumerationSymbolDefaultValueEval(info: TProgramInfo; ExtObject: TObject);
    procedure TEnumerationSymbolElementsEval(info: TProgramInfo; ExtObject: TObject);
    procedure TEnumerationSymbolStyleEval(info: TProgramInfo; ExtObject: TObject);
    procedure TEnumerationSymbolContinuousEval(info: TProgramInfo; ExtObject: TObject);
    procedure TEnumerationSymbolLowBoundEval(info: TProgramInfo; ExtObject: TObject);
    procedure TEnumerationSymbolHighBoundEval(info: TProgramInfo; ExtObject: TObject);
    procedure TEnumerationSymbolShortDescriptionEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
    {$REGION 'Eval Declarations for TExternalVarSymbol'}
    procedure TExternalVarSymbolReadFuncEval(info: TProgramInfo; ExtObject: TObject);
    procedure TExternalVarSymbolWriteFuncEval(info: TProgramInfo; ExtObject: TObject);
    {$ENDREGION}
  public
    constructor Create(Symbol: TSymbol; Level: Integer); reintroduce;
  end;

  TDocumentationBuilder = class
  strict private
    FProgram: IdwsProgram;
    FCompiler: TDelphiWebScript;
    FHtmlFilter: TdwsHtmlFilter;
    FClassesLib: TdwsClassesLib;
    FSymbolsLib: TdwsSymbolsLib;
    FTemplateSource: string;
    FDirectory: string;
    FFileExtension: string;
    {$IFDEF UseDebugger}
    FDebugger: TdwsDebugger;
    {$ENDIF}
    FAborted: Boolean;
  private
    FOnBeginBuildContent: TBuildContentEvent;
    FOnAfterBuildContent: TBuildContentEvent;
    procedure BuildContentFile(const FileName: string;
      const Symbol: TSymbol; const Level: Integer = -1);
  protected
    procedure BuildUnit(const Directory: string; UnitSymbol: TUnitSymbol);
    procedure BuildClass(const Directory: string; ClassSymbol: TClassSymbol);
    function BuildContent(const Template: string; Level: Integer;
      const Symbol: TSymbol = nil): string;
  public
    constructor Create(AProgram: IdwsProgram);
    destructor Destroy; override;

    procedure Abort;
    procedure Build(const Directory: string);
    procedure GenerateTemplate;
    procedure LoadTemplate(const FileName: TFileName; AdaptFileExtension: Boolean = True);

    property TemplateSource: string read FTemplateSource write FTemplateSource;
    property FileExtension: string read FFileExtension write FFileExtension;

    property Aborted: Boolean read FAborted;
    {$IFDEF UseDebugger}
    property Debugger: TdwsDebugger read FDebugger write FDebugger;
    {$ENDIF}
    property OnBeginBuildContent: TBuildContentEvent read FOnBeginBuildContent write FOnBeginBuildContent;
    property OnAfterBuildContent: TBuildContentEvent read FOnAfterBuildContent write FOnAfterBuildContent;
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

  UnitName := 'dwsSymbols';

  FSymbol := Symbol;
  FLevel := Level;

  // TData array definition
  with Arrays.Add do
  begin
    Name := 'TData';
    DataType := 'Variant';
    IsDynamic := True;
  end;

  // TFuncKind enumeration
  with Enumerations.Add do
  begin
    Name := 'TFuncKind';
    Elements.Add.Name := 'fkFunction';
    Elements.Add.Name := 'fkProcedure';
    Elements.Add.Name := 'fkConstructor';
    Elements.Add.Name := 'fkDestructor';
    Elements.Add.Name := 'fkMethod';
    Elements.Add.Name := 'fkLambda';
  end;
  with Enumerations.Add do
  begin
    Name := 'TMethodKind';
    Elements.Add.Name := 'mkProcedure';
    Elements.Add.Name := 'mkFunction';
    Elements.Add.Name := 'mkConstructor';
    Elements.Add.Name := 'mkDestructor';
    Elements.Add.Name := 'mkMethod';
    Elements.Add.Name := 'mkClassProcedure';
    Elements.Add.Name := 'mkClassFunction';
    Elements.Add.Name := 'mkClassMethod';
  end;
  with Enumerations.Add do
  begin
    Name := 'TMethodAttribute';
    Elements.Add.Name := 'maVirtual';
    Elements.Add.Name := 'maOverride';
    Elements.Add.Name := 'maReintroduce';
    Elements.Add.Name := 'maAbstract';
    Elements.Add.Name := 'maOverlap';
    Elements.Add.Name := 'maClassMethod';
    Elements.Add.Name := 'maFinal';
    Elements.Add.Name := 'maDefault';
    Elements.Add.Name := 'maInterfaced';
    Elements.Add.Name := 'maStatic';
  end;

  with Enumerations.Add do
  begin
    Name := 'TdwsVisibility';
    Elements.Add.Name := 'cvMagic';
    Elements.Add.Name := 'cvPrivate';
    Elements.Add.Name := 'cvProtected';
    Elements.Add.Name := 'cvPublic';
    Elements.Add.Name := 'cvPublished';
  end;

  with Enumerations.Add do
  begin
    Name := 'TEnumerationSymbolStyle';
    Elements.Add.Name := 'enumClassic';
    Elements.Add.Name := 'enumScoped';
    Elements.Add.Name := 'enumFlags';
  end;

  with Synonyms.Add do
  begin
    Name := 'TSymbolClass';
    DataType := 'TClass';
  end;

  {$REGION 'Definition for TSymbol'}
  with Classes.Add do
  begin
    Name := 'TSymbol';
    with Methods.Add do
    begin
      Name := 'GetCaption';
      ResultType := 'String';
      Visibility := cvPrivate;
      OnEval := TSymbolGetCaptionEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetDescription';
      ResultType := 'String';
      Visibility := cvPrivate;
      OnEval := TSymbolGetDescriptionEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetName';
      ResultType := 'String';
      Visibility := cvPrivate;
      OnEval := TSymbolGetNameEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetType';
      ResultType := 'TTypeSymbol';
      Visibility := cvPrivate;
      OnEval := TSymbolGetTypeEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsBaseType';
      ResultType := 'Boolean';
      OnEval := TSymbolIsBaseTypeEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsType';
      ResultType := 'Boolean';
      OnEval := TSymbolIsTypeEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsFuncSymbol';
      ResultType := 'Boolean';
      OnEval := TSymbolIsFuncSymbolEval;
    end;
    with Methods.Add do
    begin
      Name := 'QualifiedName';
      ResultType := 'String';
      OnEval := TSymbolGetQualifiedNameEval;
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
    with Properties.Add do
    begin
      Name := 'Type';
      DataType := 'TTypeSymbol';
      ReadAccess := 'GetType';
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TResourceStringSymbol'}
  with Classes.Add do
  begin
    Name := 'TResourceStringSymbol';
    Ancestor := 'TSymbol';
    with Methods.Add do
    begin
      Name := 'GetValue';
      ResultType := 'String';
      Visibility := cvPrivate;
      OnEval := TResourceStringSymbolGetValueEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIndex';
      ResultType := 'Integer';
      Visibility := cvPrivate;
      OnEval := TResourceStringSymbolGetIndexEval;
    end;

    with Properties.Add do
    begin
      Name := 'Index';
      DataType := 'String';
      ReadAccess := 'GetIndex';
    end;
    with Properties.Add do
    begin
      Name := 'Value';
      DataType := 'Integer';
      ReadAccess := 'GetValue';
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TValueSymbol'}
  with Classes.Add do
  begin
    Name := 'TValueSymbol';
    Ancestor := 'TSymbol';
  end;
  {$ENDREGION}

  {$REGION 'Definition for TConstSymbol'}
  with Classes.Add do
  begin
    Name := 'TConstSymbol';
    Ancestor := 'TValueSymbol';

    with Methods.Add do
    begin
      Name := 'GetData';
      ResultType := 'TData';
      Visibility := cvPrivate;
      OnEval := TConstSymbolGetDataEval;
    end;
    with Properties.Add do
    begin
      Name := 'Data';
      DataType := 'TData';
      ReadAccess := 'GetData';
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TDataSymbol'}
  with Classes.Add do
  begin
    Name := 'TDataSymbol';
    Ancestor := 'TValueSymbol';

    with Methods.Add do
    begin
      Name := 'HasExternalName';
      ResultType := 'Boolean';
      Visibility := cvPrivate;
      OnEval := TDataSymbolHasExternalNameEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetExternalName';
      ResultType := 'String';
      Visibility := cvPrivate;
      OnEval := TDataSymbolGetExternalNameEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetLevel';
      ResultType := 'Integer';
      Visibility := cvPrivate;
      OnEval := TDataSymbolGetLevelEval;
    end;

    with Properties.Add do
    begin
      Name := 'ExternalName';
      DataType := 'String';
      ReadAccess := 'GetExternalName';
    end;
    with Properties.Add do
    begin
      Name := 'Level';
      DataType := 'Integer';
      ReadAccess := 'GetLevel';
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TParamSymbol'}
  with Classes.Add do
  begin
    Name := 'TParamSymbol';
    Ancestor := 'TDataSymbol';

    with Methods.Add do
    begin
      Name := 'SameParam';
      ResultType := 'Boolean';
      OnEval := TParamSymbolGetSameParamEval;
      with Parameters.Add do
      begin
        Name := 'Other';
        DataType := 'TParamSymbol';
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TParamSymbolWithDefaultValue'}
  with Classes.Add do
  begin
    Name := 'TParamSymbolWithDefaultValue';
    Ancestor := 'TParamSymbol';

    with Methods.Add do
    begin
      Name := 'GetDefaultValue';
      ResultType := 'TData';
      Visibility := cvPrivate;
      OnEval := TParamSymbolWithDefaultValueGetDefaultValueEval;
    end;
    with Properties.Add do
    begin
      Name := 'DefaultValue';
      DataType := 'TData';
      ReadAccess := 'GetDefaultValue';
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TTypeSymbol'}
  with Classes.Add do
  begin
    Name := 'TTypeSymbol';
    Ancestor := 'TSymbol';

    with Methods.Add do
    begin
      Name := 'UnAliasedType';
      ResultType := 'TTypeSymbol';
      OnEval := TTypeSymbolGetUnAliasedTypeEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsOfType';
      ResultType := 'Boolean';
      OnEval := TTypeSymbolIsOfTypeEval;
      with Parameters.Add do
      begin
        Name := 'typSym';
        DataType := 'TTypeSymbol';
      end;
    end;
    with Methods.Add do
    begin
      Name := 'IsCompatible';
      ResultType := 'Boolean';
      OnEval := TTypeSymbolIsCompatibleEval;
      with Parameters.Add do
      begin
        Name := 'typSym';
        DataType := 'TTypeSymbol';
      end;
    end;
    with Methods.Add do
    begin
      Name := 'DistanceTo';
      ResultType := 'Integer';
      OnEval := TTypeSymbolGetDistanceToEval;
      with Parameters.Add do
      begin
        Name := 'typSym';
        DataType := 'TTypeSymbol';
      end;
    end;
    with Methods.Add do
    begin
      Name := 'SameType';
      ResultType := 'Boolean';
      OnEval := TTypeSymbolGetSameTypeEval;
      with Parameters.Add do
      begin
        Name := 'typSym';
        DataType := 'TTypeSymbol';
      end;
    end;
    with Methods.Add do
    begin
      Name := 'HasMetaSymbol';
      ResultType := 'Boolean';
      OnEval := TTypeSymbolHasMetaSymbolEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetDeprecatedMessage';
      ResultType := 'String';
      Visibility := cvPrivate;
      OnEval := TTypeSymbolGetDeprecatedMessageEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsDeprecated';
      ResultType := 'Boolean';
      Visibility := cvPrivate;
      OnEval := TTypeSymbolGetIsDeprecatedEval;
    end;

    // properties
    with Properties.Add do
    begin
      Name := 'DeprecatedMessage';
      DataType := 'String';
      ReadAccess := 'GetDeprecatedMessage';
    end;
    with Properties.Add do
    begin
      Name := 'IsDeprecated';
      DataType := 'Boolean';
      ReadAccess := 'GetIsDeprecated';
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TSymbolTable'}
  with Classes.Add do
  begin
    Name := 'TSymbolTable';

    with Methods.Add do
    begin
      Name := 'IndexOfParent';
      ResultType := 'Integer';
      with Parameters.Add do
      begin
        Name := 'Parent';
        DataType := 'TSymbolTable';
      end;
      OnEval := TSymbolTableIndexOfParentEval;
    end;

    with Methods.Add do
    begin
      Name := 'FindLocal';
      ResultType := 'TSymbol';
      with Parameters.Add do
      begin
        Name := 'Name';
        DataType := 'String';
      end;
(*
      with Parameters.Add do
      begin
        Name := 'ofClass';
        DataType := 'TSymbolClass';
        DefaultValue := 'nil';
      end;
*)
      OnEval := TSymbolTableFindLocalEval;
    end;
    with Methods.Add do
    begin
      Name := 'FindTypeLocal';
      ResultType := 'TTypeSymbol';
      with Parameters.Add do
      begin
        Name := 'Name';
        DataType := 'String';
      end;
      OnEval := TSymbolTableFindTypeLocalEval;
    end;
    with Methods.Add do
    begin
      Name := 'FindSymbol';
      ResultType := 'TSymbol';
      with Parameters.Add do
      begin
        Name := 'Name';
        DataType := 'String';
      end;
      with Parameters.Add do
      begin
        Name := 'MinVisibility';
        DataType := 'TdwsVisibility';
      end;
(*
      with Parameters.Add do
      begin
        Name := 'ofClass';
        DataType := 'TSymbolClass';
        DefaultValue := 'nil';
      end;
*)
      OnEval := TSymbolTableTypeSymbolEval;
    end;
    with Methods.Add do
    begin
      Name := 'FindTypeSymbol';
      ResultType := 'TTypeSymbol';
      with Parameters.Add do
      begin
        Name := 'Name';
        DataType := 'String';
      end;
      with Parameters.Add do
      begin
        Name := 'MinVisibility';
        DataType := 'TdwsVisibility';
      end;
      OnEval := TSymbolTableFindTypeSymbolEval;
    end;

    with Methods.Add do
    begin
      Name := 'HasOperators';
      ResultType := 'Boolean';
      OnEval := TSymbolTableHasOperatorsEval;
    end;
    with Methods.Add do
    begin
      Name := 'HasClass';
      ResultType := 'Boolean';
      with Parameters.Add do
      begin
        Name := 'AClass';
        DataType := 'TSymbolClass';
      end;
      OnEval := TSymbolTableHasClassEval;
    end;
    with Methods.Add do
    begin
      Name := 'HasSymbol';
      ResultType := 'Boolean';
      with Parameters.Add do
      begin
        Name := 'Symbol';
        DataType := 'TSymbol';
      end;
      OnEval := TSymbolTableHasSymbolEval;
    end;
    with Methods.Add do
    begin
      Name := 'HasMethods';
      ResultType := 'Boolean';
      OnEval := TSymbolTableHasMethodsEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsUnitTable';
      ResultType := 'Boolean';
      OnEval := TSymbolTableIsUnitTableEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetCount';
      ResultType := 'Integer';
      Visibility := cvPrivate;
      OnEval := TSymbolTableGetCountEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetSymbol';
      ResultType := 'TSymbol';
      with Parameters.Add do
      begin
        Name := 'Index';
        DataType := 'Integer';
      end;
      OnEval := TSymbolTableGetSymbolEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetParentCount';
      ResultType := 'Integer';
      Visibility := cvPrivate;
      OnEval := TSymbolTableGetParentCountEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetParent';
      ResultType := 'TSymbolTable';
      Visibility := cvPrivate;
      with Parameters.Add do
      begin
        Name := 'Index';
        DataType := 'Integer';
      end;
      OnEval := TSymbolTableGetParentEval;
    end;

    // properties
    with Properties.Add do
    begin
      Name := 'Count';
      DataType := 'Integer';
      ReadAccess := 'GetCount';
    end;
    with Properties.Add do
    begin
      Name := 'ParentCount';
      DataType := 'Integer';
      ReadAccess := 'GetParentCount';
    end;
    with Properties.Add do
    begin
      Name := 'Parents';
      DataType := 'TSymbolTable';
      with Parameters.Add do
      begin
        Name := 'Index';
        DataType := 'Integer';
      end;
      ReadAccess := 'GetParent';
    end;
    with Properties.Add do
    begin
      Name := 'Symbols';
      DataType := 'TSymbol';
      with Parameters.Add do
      begin
        Name := 'Index';
        DataType := 'Integer';
      end;
      IsDefault := True;
      ReadAccess := 'GetSymbol';
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TUnSortedSymbolTable'}
  with Classes.Add do
  begin
    Name := 'TUnSortedSymbolTable';
    Ancestor := 'TSymbolTable';
  end;
  {$ENDREGION}

  {$REGION 'Definition for TParamsSymbolTable'}
  with Classes.Add do
  begin
    Name := 'TParamsSymbolTable';
    Ancestor := 'TUnSortedSymbolTable';
    with Methods.Add do
    begin
      Name := 'GetParamSymbol';
      ResultType := 'TParamSymbol';
      Visibility := cvPrivate;
      with Parameters.Add do
      begin
        Name := 'Index';
        DataType := 'Integer';
      end;
      OnEval := TParamsSymbolTableGetSymbolEval;
    end;

    with Properties.Add do
    begin
      Name := 'Symbols';
      DataType := 'TParamSymbol';
      with Parameters.Add do
      begin
        Name := 'Index';
        DataType := 'Integer';
      end;
      IsDefault := True;
      ReadAccess := 'GetParamSymbol';
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TFuncSymbol'}
  with Classes.Add do
  begin
    Name := 'TFuncSymbol';
    Ancestor := 'TTypeSymbol';

    with Methods.Add do
    begin
      Name := 'HasParam';
      ResultType := 'Boolean';
      OnEval := TFuncSymbolHasParamEval;
      with Parameters.Add do
      begin
        Name := 'Parameter';
        DataType := 'TParamSymbol';
      end;
    end;
    with Methods.Add do
    begin
      Name := 'GetParamType';
      ResultType := 'TTypeSymbol';
      OnEval := TFuncSymbolGetParamTypeEval;
      with Parameters.Add do
      begin
        Name := 'Index';
        DataType := 'Integer';
      end;
    end;
    with Methods.Add do
    begin
      Name := 'IsValidOverloadOf';
      ResultType := 'Boolean';
      OnEval := TFuncSymbolIsValidOverloadOfEval;
      with Parameters.Add do
      begin
        Name := 'Other';
        DataType := 'TFuncSymbol';
      end;
    end;
    with Methods.Add do
    begin
      Name := 'IsSameOverloadOf';
      ResultType := 'Boolean';
      OnEval := TFuncSymbolIsSameOverloadOfEval;
      with Parameters.Add do
      begin
        Name := 'Other';
        DataType := 'TFuncSymbol';
      end;
    end;
    with Methods.Add do
    begin
      Name := 'ParamsDescription';
      ResultType := 'String';
      OnEval := TFuncSymbolGetParamsDescriptionEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsDeprecated';
      ResultType := 'Boolean';
      Visibility := cvPrivate;
      OnEval := TFuncSymbolGetIsDeprecatedEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsStateless';
      ResultType := 'Boolean';
      Visibility := cvPrivate;
      OnEval := TFuncSymbolGetIsStatelessEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsForwarded';
      ResultType := 'Boolean';
      Visibility := cvPrivate;
      OnEval := TFuncSymbolGetIsForwardedEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsOverloaded';
      ResultType := 'Boolean';
      Visibility := cvPrivate;
      OnEval := TFuncSymbolGetIsOverloadedEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsExternal';
      ResultType := 'Boolean';
      Visibility := cvPrivate;
      OnEval := TFuncSymbolGetIsExternalEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetKind';
      ResultType := 'TFuncKind';
      Visibility := cvPrivate;
      OnEval := TFuncSymbolGetKindEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetExternalName';
      ResultType := 'String';
      Visibility := cvPrivate;
      OnEval := TFuncSymbolGetExternalNameEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetHasExternalName';
      ResultType := 'Boolean';
      Visibility := cvPrivate;
      OnEval := TFuncSymbolGetHasExternalNameEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsLambda';
      ResultType := 'Boolean';
      Visibility := cvPrivate;
      OnEval := TFuncSymbolGetIsLambdaEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetLevel';
      ResultType := 'Integer';
      Visibility := cvPrivate;
      OnEval := TFuncSymbolGetLevelEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetParams';
      ResultType := 'TParamsSymbolTable';
      Visibility := cvPrivate;
      OnEval := TFuncSymbolGetParamsEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetResult';
      ResultType := 'TDataSymbol';
      Visibility := cvPrivate;
      OnEval := TFuncSymbolGetResultEval;
    end;

    // properties
    with Properties.Add do
    begin
      Name := 'ExternalName';
      DataType := 'String';
      ReadAccess := 'GetExternalName';
    end;
    with Properties.Add do
    begin
      Name := 'Kind';
      DataType := 'TFuncKind';
      ReadAccess := 'GetKind';
    end;
    with Properties.Add do
    begin
      Name := 'IsDeprecated';
      DataType := 'Boolean';
      ReadAccess := 'GetIsDeprecated';
    end;
    with Properties.Add do
    begin
      Name := 'IsStateless';
      DataType := 'Boolean';
      ReadAccess := 'GetIsStateless';
    end;
    with Properties.Add do
    begin
      Name := 'IsForwarded';
      DataType := 'Boolean';
      ReadAccess := 'GetIsForwarded';
    end;
    with Properties.Add do
    begin
      Name := 'IsOverloaded';
      DataType := 'Boolean';
      ReadAccess := 'GetIsOverloaded';
    end;
    with Properties.Add do
    begin
      Name := 'IsExternal';
      DataType := 'Boolean';
      ReadAccess := 'GetIsExternal';
    end;
    with Properties.Add do
    begin
      Name := 'HasExternalName';
      DataType := 'Boolean';
      ReadAccess := 'GetHasExternalName';
    end;
    with Properties.Add do
    begin
      Name := 'IsLambda';
      DataType := 'Boolean';
      ReadAccess := 'GetIsLambda';
    end;
    with Properties.Add do
    begin
      Name := 'Level';
      DataType := 'Integer';
      ReadAccess := 'GetLevel';
    end;
    with Properties.Add do
    begin
      Name := 'Params';
      DataType := 'TParamsSymbolTable';
      ReadAccess := 'GetParams';
    end;
    with Properties.Add do
    begin
      Name := 'Result';
      DataType := 'TDataSymbol';
      ReadAccess := 'GetResult';
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TMethodSymbol'}
  with Classes.Add do
  begin
    Name := 'TMethodSymbol';
    Ancestor := 'TFuncSymbol';

    with Methods.Add do
    begin
      Name := 'GetStructSymbol';
      ResultType := 'TCompositeTypeSymbol';
      Visibility := cvPrivate;
      OnEval := TMethodSymbolGetStructSymbolEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsDefault';
      ResultType := 'Boolean';
      Visibility := cvPrivate;
      OnEval := TMethodSymbolGetIsDefaultEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsAbstract';
      ResultType := 'Boolean';
      Visibility := cvPrivate;
      OnEval := TMethodSymbolGetIsAbstractEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsVirtual';
      ResultType := 'Boolean';
      Visibility := cvPrivate;
      OnEval := TMethodSymbolGetIsVirtualEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsOverride';
      ResultType := 'Boolean';
      Visibility := cvPrivate;
      OnEval := TMethodSymbolGetIsOverrideEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsInterfaced';
      ResultType := 'Boolean';
      Visibility := cvPrivate;
      OnEval := TMethodSymbolGetIsInterfacedEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsFinal';
      ResultType := 'Boolean';
      Visibility := cvPrivate;
      OnEval := TMethodSymbolGetIsFinalEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsOverlap';
      ResultType := 'Boolean';
      Visibility := cvPrivate;
      OnEval := TMethodSymbolGetIsOverlapEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsClassMethod';
      ResultType := 'Boolean';
      Visibility := cvPrivate;
      OnEval := TMethodSymbolGetIsClassMethodEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsStatic';
      ResultType := 'Boolean';
      Visibility := cvPrivate;
      OnEval := TMethodSymbolGetIsStaticEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetParentMeth';
      ResultType := 'TMethodSymbol';
      Visibility := cvPrivate;
      OnEval := TMethodSymbolGetParentMethEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetRootParentMeth';
      ResultType := 'TMethodSymbol';
      Visibility := cvPrivate;
      OnEval := TMethodSymbolGetRootParentMethEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetSelfSym';
      ResultType := 'TDataSymbol';
      Visibility := cvPrivate;
      OnEval := TMethodSymbolGetSelfSymEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetVisibility';
      ResultType := 'TdwsVisibility';
      Visibility := cvPrivate;
      OnEval := TMethodSymbolGetVisibilityEval;
    end;

    // properties
    with Properties.Add do
    begin
      Name := 'StructSymbol';
      DataType := 'TCompositeTypeSymbol';
      ReadAccess := 'GetStructSymbol';
    end;
    with Properties.Add do
    begin
      Name := 'IsDefault';
      DataType := 'Boolean';
      ReadAccess := 'GetIsDefault';
    end;
    with Properties.Add do
    begin
      Name := 'IsAbstract';
      DataType := 'Boolean';
      ReadAccess := 'GetIsAbstract';
    end;
    with Properties.Add do
    begin
      Name := 'IsOverride';
      DataType := 'Boolean';
      ReadAccess := 'GetIsOverride';
    end;
    with Properties.Add do
    begin
      Name := 'IsInterfaced';
      DataType := 'Boolean';
      ReadAccess := 'GetIsInterfaced';
    end;
    with Properties.Add do
    begin
      Name := 'IsFinal';
      DataType := 'Boolean';
      ReadAccess := 'GetIsFinal';
    end;
    with Properties.Add do
    begin
      Name := 'IsOverlap';
      DataType := 'Boolean';
      ReadAccess := 'GetIsOverlap';
    end;
    with Properties.Add do
    begin
      Name := 'IsClassMethod';
      DataType := 'Boolean';
      ReadAccess := 'GetIsClassMethod';
    end;
    with Properties.Add do
    begin
      Name := 'IsStatic';
      DataType := 'Boolean';
      ReadAccess := 'GetIsStatic';
    end;
    with Properties.Add do
    begin
      Name := 'ParentMeth';
      DataType := 'Boolean';
      ReadAccess := 'GetParentMeth';
    end;
    with Properties.Add do
    begin
      Name := 'RootParentMeth';
      DataType := 'Boolean';
      ReadAccess := 'GetRootParentMeth';
    end;
    with Properties.Add do
    begin
      Name := 'SelfSym';
      DataType := 'Boolean';
      ReadAccess := 'GetSelfSym';
    end;
    with Properties.Add do
    begin
      Name := 'Visibility';
      DataType := 'Boolean';
      ReadAccess := 'GetVisibility';
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TSetOfSymbol'}
  with Classes.Add do
  begin
    Name := 'TSetOfSymbol';
    Ancestor := 'TTypeSymbol';
    with Methods.Add do
    begin
      Name := 'GetMinValue';
      ResultType := 'Integer';
      OnEval := TSetOfSymbolGetMinValueEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetMaxValue';
      ResultType := 'Integer';
      OnEval := TSetOfSymbolGetMaxValueEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetCountValue';
      ResultType := 'Integer';
      OnEval := TSetOfSymbolGetCountValueEval;
    end;

    // properties
    with Properties.Add do
    begin
      Name := 'MinValue';
      DataType := 'Integer';
      ReadAccess := 'GetMinValue';
    end;
    with Properties.Add do
    begin
      Name := 'MaxValue';
      DataType := 'Integer';
      ReadAccess := 'GetMaxValue';
    end;
    with Properties.Add do
    begin
      Name := 'CountValue';
      DataType := 'Integer';
      ReadAccess := 'GetCountValue';
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TArraySymbol'}
  with Classes.Add do
  begin
    Name := 'TArraySymbol';
    Ancestor := 'TTypeSymbol';

    with Methods.Add do
    begin
      Name := 'GetIndexType';
      ResultType := 'TTypeSymbol';
      OnEval := TArraySymbolGetIndexTypeEval;
    end;
    with Properties.Add do
    begin
      Name := 'IndexType';
      DataType := 'TTypeSymbol';
      ReadAccess := 'GetIndexType';
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TStaticArraySymbol'}
  with Classes.Add do
  begin
    Name := 'TStaticArraySymbol';
    Ancestor := 'TArraySymbol';

    with Methods.Add do
    begin
      Name := 'GetIsEmptyArray';
      ResultType := 'Boolean';
      OnEval := TStaticArraySymbolGetIsEmptyArrayEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetHighBound';
      ResultType := 'Integer';
      OnEval := TStaticArraySymbolGetHighBoundEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetLowBound';
      ResultType := 'Integer';
      OnEval := TStaticArraySymbolGetLowBoundEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetElementCount';
      ResultType := 'Integer';
      OnEval := TStaticArraySymbolGetElementCountEval;
    end;

    // properties
    with Properties.Add do
    begin
      Name := 'IsEmptyArray';
      DataType := 'Boolean';
      ReadAccess := 'GetIsEmptyArray';
    end;
    with Properties.Add do
    begin
      Name := 'HighBound';
      DataType := 'Integer';
      ReadAccess := 'GetHighBound';
    end;
    with Properties.Add do
    begin
      Name := 'LowBound';
      DataType := 'Integer';
      ReadAccess := 'GetLowBound';
    end;
    with Properties.Add do
    begin
      Name := 'ElementCount';
      DataType := 'Integer';
      ReadAccess := 'GetElementCount';
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TMembersSymbolTable'}
  with Classes.Add do
  begin
    Name := 'TMembersSymbolTable';
    Ancestor := 'TSymbolTable';
(*
    with Methods.Add do
    begin
      Name := 'Owner';
      ResultType := 'TCompositeTypeSymbol';
      OnEval := TMembersSymbolTableGetOwnerEval;
    end;
*)
  end;
  {$ENDREGION}

  {$REGION 'Definition for TClassConstSymbol'}
  with Classes.Add do
  begin
    Name := 'TClassConstSymbol';
    Ancestor := 'TConstSymbol';

    with Methods.Add do
    begin
      Name := 'GetOwnerSymbol';
      ResultType := 'TCompositeTypeSymbol';
      OnEval := TClassConstSymbolGetOwnerSymbolEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetVisibility';
      ResultType := 'TdwsVisibility';
      OnEval := TClassConstSymbolGetVisibilityEval;
    end;

    // Properties
    with Properties.Add do
    begin
      Name := 'OwnerSymbol';
      DataType := 'TCompositeTypeSymbol';
      ReadAccess := 'GetOwnerSymbol';
    end;
    with Properties.Add do
    begin
      Name := 'Visibility';
      DataType := 'TdwsVisibility';
      ReadAccess := 'GetVisibility';
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TClassVarSymbol'}
  with Classes.Add do
  begin
    Name := 'TClassVarSymbol';
    Ancestor := 'TDataSymbol';

    with Methods.Add do
    begin
      Name := 'GetOwnerSymbol';
      ResultType := 'TCompositeTypeSymbol';
      OnEval := TClassVarSymbolGetOwnerSymbolEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetVisibility';
      ResultType := 'TdwsVisibility';
      OnEval := TClassVarSymbolGetVisibilityEval;
    end;

    // Properties
    with Properties.Add do
    begin
      Name := 'OwnerSymbol';
      DataType := 'TCompositeTypeSymbol';
      ReadAccess := 'GetOwnerSymbol';
    end;
    with Properties.Add do
    begin
      Name := 'Visibility';
      DataType := 'TdwsVisibility';
      ReadAccess := 'GetVisibility';
    end;
  end;
  {$ENDREGION}

  with Forwards.Add do
  begin
    Name := 'TFieldSymbol';
  end;

  {$REGION 'Definition for TCompositeTypeSymbol'}
  with Classes.Add do
  begin
    Name := 'TCompositeTypeSymbol';
    Ancestor := 'TSymbol';

    with Methods.Add do
    begin
      Name := 'GetUnitSymbol';
      ResultType := 'TUnitSymbol';
      Visibility := cvPrivate;
      OnEval := TCompositeTypeSymbolGetUnitSymbolEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetParent';
      ResultType := 'TCompositeTypeSymbol';
      Visibility := cvPrivate;
      OnEval := TCompositeTypeSymbolGetParentEval;
    end;
    with Methods.Add do
    begin
      Name := 'AllowVirtualMembers';
      ResultType := 'Boolean';
      OnEval := TCompositeTypeSymbolAllowVirtualMembersEval;
    end;
    with Methods.Add do
    begin
      Name := 'AllowOverloads';
      ResultType := 'Boolean';
      OnEval := TCompositeTypeSymbolAllowOverloadsEval;
    end;
    with Methods.Add do
    begin
      Name := 'AllowDefaultProperty';
      ResultType := 'Boolean';
      OnEval := TCompositeTypeSymbolAllowDefaultPropertyEval;
    end;
    with Methods.Add do
    begin
      Name := 'AllowFields';
      ResultType := 'Boolean';
      OnEval := TCompositeTypeSymbolAllowFieldsEval;
    end;
    with Methods.Add do
    begin
      Name := 'AllowAnonymousMethods';
      ResultType := 'Boolean';
      OnEval := TCompositeTypeSymbolAllowAnonymousMethodsEval;
    end;
    with Methods.Add do
    begin
      Name := 'FirstField';
      ResultType := 'TFieldSymbol';
      OnEval := TCompositeTypeSymbolFirstFieldEval;
    end;
    with Methods.Add do
    begin
      Name := 'ExternalRoot';
      ResultType := 'TCompositeTypeSymbol';
      OnEval := TCompositeTypeSymbolExternalRootEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetMembers';
      ResultType := 'TMembersSymbolTable';
      OnEval := TCompositeTypeSymbolGetMembersEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetDefaultProperty';
      ResultType := 'TPropertySymbol';
      OnEval := TCompositeTypeSymbolGetDefaultPropertyEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsStatic';
      ResultType := 'Boolean';
      OnEval := TCompositeTypeSymbolGetIsStaticEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsPartial';
      ResultType := 'Boolean';
      OnEval := TCompositeTypeSymbolGetIsPartialEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsExternal';
      ResultType := 'Boolean';
      OnEval := TCompositeTypeSymbolGetIsExternalEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsExternalRooted';
      ResultType := 'Boolean';
      OnEval := TCompositeTypeSymbolGetIsExternalRootedEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetExternalName';
      ResultType := 'Boolean';
      OnEval := TCompositeTypeSymbolGetExternalNameEval;
    end;

    with Properties.Add do
    begin
      Name := 'UnitSymbol';
      DataType := 'TUnitSymbol';
      ReadAccess := 'GetUnitSymbol';
    end;
    with Properties.Add do
    begin
      Name := 'Parent';
      DataType := 'TCompositeTypeSymbol';
      ReadAccess := 'GetParent';
    end;
    with Properties.Add do
    begin
      Name := 'Members';
      DataType := 'TMembersSymbolTable';
      ReadAccess := 'GetMembers';
    end;
    with Properties.Add do
    begin
      Name := 'DefaultProperty';
      DataType := 'TPropertySymbol';
      ReadAccess := 'GetDefaultProperty';
    end;
    with Properties.Add do
    begin
      Name := 'IsStatic';
      DataType := 'Boolean';
      ReadAccess := 'GetIsStatic';
    end;
    with Properties.Add do
    begin
      Name := 'IsPartial';
      DataType := 'Boolean';
      ReadAccess := 'GetIsPartial';
    end;
    with Properties.Add do
    begin
      Name := 'IsExternal';
      DataType := 'Boolean';
      ReadAccess := 'GetIsExternal';
    end;
    with Properties.Add do
    begin
      Name := 'IsExternalRooted';
      DataType := 'Boolean';
      ReadAccess := 'GetIsExternalRooted';
    end;
    with Properties.Add do
    begin
      Name := 'ExternalName';
      DataType := 'Boolean';
      ReadAccess := 'GetExternalName';
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TStructuredTypeSymbol'}
  with Classes.Add do
  begin
    Name := 'TStructuredTypeSymbol';
    Ancestor := 'TCompositeTypeSymbol';

    with Methods.Add do
    begin
      Name := 'GetIsForwarded';
      ResultType := 'Boolean';
      OnEval := TStructuredTypeSymbolGetIsForwardedEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetMetaSymbol';
      ResultType := 'TStructuredTypeMetaSymbol';
      OnEval := TStructuredTypeSymbolGetMetaSymbolEval;
    end;

    // Properties
    with Properties.Add do
    begin
      Name := 'IsForwarded';
      DataType := 'Boolean';
      ReadAccess := 'GetIsForwarded';
    end;
    with Properties.Add do
    begin
      Name := 'MetaSymbol';
      DataType := 'TStructuredTypeMetaSymbol';
      ReadAccess := 'GetMetaSymbol';
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TStructuredTypeMetaSymbol'}
  with Classes.Add do
  begin
    Name := 'TStructuredTypeMetaSymbol';
    Ancestor := 'TTypeSymbol';
    with Methods.Add do
    begin
      Name := 'StructSymbol';
      ResultType := 'TStructuredTypeSymbol';
      OnEval := TStructuredTypeMetaSymbolStructSymbolEval;
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TFieldSymbol'}
  with Classes.Add do
  begin
    Name := 'TFieldSymbol';
    Ancestor := 'TValueSymbol';

    with Methods.Add do
    begin
      Name := 'StructSymbol';
      ResultType := 'TCompositeTypeSymbol';
      OnEval := TFieldSymbolStructSymbolEval;
    end;
    with Methods.Add do
    begin
      Name := 'Visibility';
      ResultType := 'TdwsVisibility';
      OnEval := TFieldSymbolVisibilityEval;
    end;
    with Methods.Add do
    begin
      Name := 'DefaultValue';
      ResultType := 'TData';
      OnEval := TFieldSymbolDefaultValueEval;
    end;
    with Methods.Add do
    begin
      Name := 'ExternalName';
      ResultType := 'String';
      OnEval := TFieldSymbolExternalNameEval;
    end;
    with Methods.Add do
    begin
      Name := 'NextField';
      ResultType := 'TFieldSymbol';
      OnEval := TFieldSymbolNextFieldEval;
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TRecordSymbol'}
  with Classes.Add do
  begin
    Name := 'TRecordSymbol';
    Ancestor := 'TStructuredTypeSymbol';
    with Methods.Add do
    begin
      Name := 'IsDynamic';
      ResultType := 'Boolean';
      OnEval := TRecordSymbolIsDynamicEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsFullyDefined';
      ResultType := 'Boolean';
      OnEval := TRecordSymbolIsFullyDefinedEval;
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TInterfaceSymbol'}
  with Classes.Add do
  begin
    Name := 'TInterfaceSymbol';
    Ancestor := 'TStructuredTypeSymbol';

    with Methods.Add do
    begin
      Name := 'GetParent';
      ResultType := 'TInterfaceSymbol';
      Visibility := cvPrivate;
      OnEval := TInterfaceSymbolGetParentEval;
    end;
    with Methods.Add do
    begin
      Name := 'MethodCount';
      ResultType := 'Integer';
      OnEval := TInterfaceSymbolMethodCountEval;
    end;
    with Properties.Add do
    begin
      Name := 'Parent';
      DataType := 'TInterfaceSymbol';
      ReadAccess := 'GetParent';
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TPropertySymbol'}
  with Classes.Add do
  begin
    Name := 'TPropertySymbol';
    Ancestor := 'TStructuredTypeSymbol';

    with Methods.Add do
    begin
      Name := 'OwnerSymbol';
      ResultType := 'TCompositeTypeSymbol';
      OnEval := TPropertySymbolOwnerSymbolEval;
    end;
    with Methods.Add do
    begin
      Name := 'Visibility';
      ResultType := 'TdwsVisibility';
      OnEval := TPropertySymbolVisibilityEval;
    end;
    with Methods.Add do
    begin
      Name := 'ArrayIndices';
      ResultType := 'TParamsSymbolTable';
      OnEval := TPropertySymbolArrayIndicesEval;
    end;
    with Methods.Add do
    begin
      Name := 'ReadSym';
      ResultType := 'TSymbol';
      OnEval := TPropertySymbolReadSymEval;
    end;
    with Methods.Add do
    begin
      Name := 'WriteSym';
      ResultType := 'TSymbol';
      OnEval := TPropertySymbolWriteSymEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsDefault';
      ResultType := 'Boolean';
      OnEval := TPropertySymbolIsDefaultEval;
    end;
    with Methods.Add do
    begin
      Name := 'IndexValue';
      ResultType := 'TData';
      OnEval := TPropertySymbolIndexValueEval;
    end;
    with Methods.Add do
    begin
      Name := 'IndexSym';
      ResultType := 'TTypeSymbol';
      OnEval := TPropertySymbolIndexSymEval;
    end;
    with Methods.Add do
    begin
      Name := 'DeprecatedMessage';
      ResultType := 'String';
      OnEval := TPropertySymbolDeprecatedMessageEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsDeprecated';
      ResultType := 'Boolean';
      OnEval := TPropertySymbolIsDeprecatedEval;
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TClassSymbol'}
  with Classes.Add do
  begin
    Name := 'TClassSymbol';
    Ancestor := 'TStructuredTypeSymbol';

    with Methods.Add do
    begin
      Name := 'ImplementsInterface';
      ResultType := 'Boolean';
      with Parameters.Add do
      begin
        Name := 'intfSym';
        DataType := 'TInterfaceSymbol';
      end;
      OnEval := TClassSymbolImplementsInterfaceEval;
    end;

    with Methods.Add do
    begin
      Name := 'GetParent';
      ResultType := 'TClassSymbol';
      Visibility := cvPrivate;
      OnEval := TClassSymbolGetParentEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsExplicitAbstract';
      ResultType := 'Boolean';
      Visibility := cvPrivate;
      OnEval := TClassSymbolGetIsExplicitAbstractEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsAbstract';
      ResultType := 'Boolean';
      Visibility := cvPrivate;
      OnEval := TClassSymbolGetIsAbstractEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsSealed';
      ResultType := 'Boolean';
      Visibility := cvPrivate;
      OnEval := TClassSymbolGetIsSealedEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsExternal';
      ResultType := 'Boolean';
      Visibility := cvPrivate;
      OnEval := TClassSymbolGetIsExternalEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetIsPartial';
      ResultType := 'Boolean';
      Visibility := cvPrivate;
      OnEval := TClassSymbolGetIsPartialEval;
    end;

    // properties
    with Properties.Add do
    begin
      Name := 'Parent';
      DataType := 'TClassSymbol';
      ReadAccess := 'GetParent';
    end;
    with Properties.Add do
    begin
      Name := 'IsExplicitAbstract';
      DataType := 'Boolean';
      ReadAccess := 'GetIsExplicitAbstract';
    end;
    with Properties.Add do
    begin
      Name := 'IsAbstract';
      DataType := 'Boolean';
      ReadAccess := 'GetIsAbstract';
    end;
    with Properties.Add do
    begin
      Name := 'IsSealed';
      DataType := 'Boolean';
      ReadAccess := 'GetIsSealed';
    end;
    with Properties.Add do
    begin
      Name := 'IsExternal';
      DataType := 'Boolean';
      ReadAccess := 'GetIsExternal';
    end;
    with Properties.Add do
    begin
      Name := 'IsPartial';
      DataType := 'Boolean';
      ReadAccess := 'GetIsPartial';
    end;
  end;
  {$ENDREGION}

  with Forwards.Add do
  begin
    Name := 'TEnumerationSymbol';
  end;

  {$REGION 'Definition for TElementSymbol'}
  with Classes.Add do
  begin
    Name := 'TElementSymbol';
    Ancestor := 'TConstSymbol';
    with Methods.Add do
    begin
      Name := 'Enumeration';
      ResultType := 'TEnumerationSymbol';
      OnEval := TElementSymbolEnumerationEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsUserDef';
      ResultType := 'TEnumerationSymbol';
      OnEval := TElementSymbolIsUserDefEval;
    end;
    with Methods.Add do
    begin
      Name := 'Value';
      ResultType := 'TEnumerationSymbol';
      OnEval := TElementSymbolValueEval;
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TEnumerationSymbol'}
  with Classes.Add do
  begin
    Name := 'TEnumerationSymbol';
    Ancestor := 'TTypeSymbol';
    with Methods.Add do
    begin
      Name := 'DefaultValue';
      ResultType := 'Integer';
      OnEval := TEnumerationSymbolDefaultValueEval;
    end;
    with Methods.Add do
    begin
      Name := 'Elements';
      ResultType := 'TSymbolTable';
      OnEval := TEnumerationSymbolElementsEval;
    end;
    with Methods.Add do
    begin
      Name := 'Style';
      ResultType := 'TEnumerationSymbolStyle';
      OnEval := TEnumerationSymbolStyleEval;
    end;
    with Methods.Add do
    begin
      Name := 'Continuous';
      ResultType := 'Boolean';
      OnEval := TEnumerationSymbolContinuousEval;
    end;
    with Methods.Add do
    begin
      Name := 'LowBound';
      ResultType := 'Integer';
      OnEval := TEnumerationSymbolLowBoundEval;
    end;
    with Methods.Add do
    begin
      Name := 'HighBound';
      ResultType := 'Integer';
      OnEval := TEnumerationSymbolHighBoundEval;
    end;
    with Methods.Add do
    begin
      Name := 'ShortDescription';
      ResultType := 'String';
      OnEval := TEnumerationSymbolShortDescriptionEval;
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TExternalVarSymbol'}
  with Classes.Add do
  begin
    Name := 'TExternalVarSymbol';
    Ancestor := 'TValueSymbol';
    with Methods.Add do
    begin
      Name := 'ReadFunc';
      ResultType := 'TFuncSymbol';
      OnEval := TExternalVarSymbolReadFuncEval;
    end;
    with Methods.Add do
    begin
      Name := 'WriteFunc';
      ResultType := 'TFuncSymbol';
      OnEval := TExternalVarSymbolWriteFuncEval;
    end;
  end;
  {$ENDREGION}

  {$REGION 'Definition for TUnitSymbol'}
  with Classes.Add do
  begin
    Name := 'TUnitSymbol';
    Ancestor := 'TSymbol';
  end;
  {$ENDREGION}

  // function definitions
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
  with Functions.Add do
  begin
    Name := 'FileName';
    ResultType := 'String';
    OnEval := GetFileName;
  end;


  FScriptObj := nil;
end;

function GetFileNameForSymbol(Symbol: TSymbol): TFileName;
begin
  Result := '';
{
  if Assigned(Symbol) then
  begin
    if Symbol is TUnitSymbol then
    begin
      Result := '.\' + TUnitSymbol(Symbol).Name + '\Index';
    end else
    if Symbol is TClassSymbol then
    begin
      Result := '.\' + TUnitSymbol(TClassSymbol(Symbol).UnitSymbol).Name;
      Result := Result + '\Classes\' + TClassSymbol(Symbol).Name + '\Index';
    end
    else if Symbol is TConstSymbol then
    begin
(*
      Result := '.\' + TUnitSymbol(TConstSymbol(Symbol).UnitSymbol).Name;
      Result := Result + '\Constants\' + TConstSymbol(Symbol).Name;
*)
    end
    else if Symbol is TFuncSymbol then
    begin
(*
      Result := '.\' + TUnitSymbol(TConstSymbol(Symbol).UnitSymbol).Name;
      Result := Result + '\Routines\' + TConstSymbol(Symbol).Name;
*)
    end
    else if Symbol is TTypeSymbol then
    begin
(*
      Result := '.\' + TUnitSymbol(TTypeSymbol(Symbol).UnitSymbol).Name;
      Result := Result + '\Types\' + TTypeSymbol(Symbol).Name;
*)
    end
  end;
}
end;


procedure TSymbolUnit.GetFileName(info: TProgramInfo);
begin
  info.ResultAsString := GetFileNameForSymbol(FSymbol);
end;

procedure TSymbolUnit.GetLevel(info: TProgramInfo);
begin
  info.ResultAsInteger := FLevel;
end;

procedure TSymbolUnit.GetSymbol(info: TProgramInfo);
begin
  if Assigned(FSymbol) then
  begin
    if not Assigned(FScriptObj) then
      FScriptObj := info.RegisterExternalObject(FSymbol, False, False);
    info.ResultAsVariant := FScriptObj;
  end
(*
  else
    info.ResultAsInteger := 0;
*)
end;

{$REGION 'Eval Implementation for TSymbolTable'}
procedure TSymbolUnit.TSymbolTableFindLocalEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(
    TSymbolTable(ExtObject).FindLocal(info.ParamAsString[0]));
end;

procedure TSymbolUnit.TSymbolTableFindTypeLocalEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(
    TSymbolTable(ExtObject).FindTypeLocal(info.ParamAsString[0]));
end;

procedure TSymbolUnit.TSymbolTableFindTypeSymbolEval(info: TProgramInfo;
  ExtObject: TObject);
var
  Sym: TSymbol;
begin
  Sym := TSymbolTable(ExtObject).FindSymbol(info.ParamAsString[0],
    TdwsVisibility(info.ParamAsInteger[1]));
  info.ResultAsVariant := info.RegisterExternalObject(Sym);
end;

procedure TSymbolUnit.TSymbolTableTypeSymbolEval(info: TProgramInfo;
  ExtObject: TObject);
var
  TypeSym: TTypeSymbol;
begin
  TypeSym := TSymbolTable(ExtObject).FindTypeSymbol(info.ParamAsString[0],
    TdwsVisibility(info.ParamAsInteger[1]));
  info.ResultAsVariant := info.RegisterExternalObject(TypeSym);
end;

procedure TSymbolUnit.TSymbolTableGetCountEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TSymbolTable(ExtObject).Count;
end;

procedure TSymbolUnit.TSymbolTableGetParentCountEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TSymbolTable(ExtObject).ParentCount;
end;

procedure TSymbolUnit.TSymbolTableGetParentEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(
    TSymbolTable(ExtObject).Parents[info.ParamAsInteger[0]]);
end;

procedure TSymbolUnit.TSymbolTableGetSymbolEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(
    TSymbolTable(ExtObject).Symbols[info.ParamAsInteger[0]]);
end;

procedure TSymbolUnit.TSymbolTableHasClassEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TSymbolTable(ExtObject).HasClass(TSymbolClass(
    info.Params[0].ExternalObject));
end;

procedure TSymbolUnit.TSymbolTableHasMethodsEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TSymbolTable(ExtObject).HasMethods;
end;

procedure TSymbolUnit.TSymbolTableHasOperatorsEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TSymbolTable(ExtObject).HasOperators;
end;

procedure TSymbolUnit.TSymbolTableHasSymbolEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TSymbolTable(ExtObject).HasSymbol(TSymbol(
    info.Params[0].ExternalObject));
end;

procedure TSymbolUnit.TSymbolTableIndexOfParentEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TSymbolTable(ExtObject).IndexOfParent(TSymbolTable(
    info.Params[0].ExternalObject));
end;

procedure TSymbolUnit.TSymbolTableIsUnitTableEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TSymbolTable(ExtObject).IsUnitTable;
end;
{$ENDREGION}

{$REGION 'Eval Implementation for TSetOfSymbol'}
procedure TSymbolUnit.TSetOfSymbolGetCountValueEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TSetOfSymbol(ExtObject).MinValue;
end;

procedure TSymbolUnit.TSetOfSymbolGetMaxValueEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TSetOfSymbol(ExtObject).MaxValue;
end;

procedure TSymbolUnit.TSetOfSymbolGetMinValueEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TSetOfSymbol(ExtObject).CountValue;
end;
{$ENDREGION}

{$REGION 'Eval Implementation for TStaticArraySymbol'}
procedure TSymbolUnit.TStaticArraySymbolGetElementCountEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TStaticArraySymbol(ExtObject).ElementCount;
end;

procedure TSymbolUnit.TStaticArraySymbolGetHighBoundEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TStaticArraySymbol(ExtObject).HighBound;
end;

procedure TSymbolUnit.TStaticArraySymbolGetIsEmptyArrayEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TStaticArraySymbol(ExtObject).IsEmptyArray;
end;

procedure TSymbolUnit.TStaticArraySymbolGetLowBoundEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TStaticArraySymbol(ExtObject).LowBound;
end;
{$ENDREGION}

{$REGION 'Eval Implementation for TStructuredTypeMetaSymbol'}
procedure TSymbolUnit.TStructuredTypeMetaSymbolStructSymbolEval(
  info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(
    TFieldSymbol(ExtObject).StructSymbol);
end;
{$ENDREGION}

{$REGION 'Eval Implementation for TStructuredTypeSymbol'}
procedure TSymbolUnit.TStructuredTypeSymbolGetIsForwardedEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TStructuredTypeSymbol(ExtObject).IsForwarded;
end;

procedure TSymbolUnit.TStructuredTypeSymbolGetMetaSymbolEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(
    TStructuredTypeSymbol(ExtObject).MetaSymbol);
end;
{$ENDREGION}

{$REGION 'Eval Implementation for TSymbol'}
procedure TSymbolUnit.TSymbolGetCaptionEval(info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsString := TSymbol(ExtObject).Caption;
end;

procedure TSymbolUnit.TSymbolGetDescriptionEval(info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsString := TSymbol(ExtObject).Description;
end;

procedure TSymbolUnit.TSymbolIsBaseTypeEval(info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(TSymbol(ExtObject).BaseType);
end;

procedure TSymbolUnit.TSymbolIsFuncSymbolEval(info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsBoolean := (TSymbol(ExtObject).AsFuncSymbol<>nil);
end;

procedure TSymbolUnit.TSymbolIsTypeEval(info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsBoolean := TSymbol(ExtObject).IsType;
end;

procedure TSymbolUnit.TSymbolGetNameEval(info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsString := TSymbol(ExtObject).Name;
end;

procedure TSymbolUnit.TSymbolGetQualifiedNameEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsString := TSymbol(ExtObject).QualifiedName;
end;

procedure TSymbolUnit.TSymbolGetTypeEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(TSymbol(ExtObject).Typ);
end;
{$ENDREGION}

{$REGION 'Eval Implementation for TFuncSymbol'}
procedure TSymbolUnit.TFuncSymbolGetParamTypeEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(
    TFuncSymbol(ExtObject).GetParamType(info.ParamAsInteger[0]));
end;

procedure TSymbolUnit.TFuncSymbolGetHasExternalNameEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TFuncSymbol(ExtObject).HasExternalName;
end;

procedure TSymbolUnit.TFuncSymbolHasParamEval(info: TProgramInfo;
  ExtObject: TObject);
var
  ParamSymbol: TParamSymbol;
begin
  ParamSymbol := TParamSymbol(info.Params[0].ExternalObject);
  info.ResultAsBoolean := TFuncSymbol(ExtObject).HasParam(ParamSymbol);
end;

procedure TSymbolUnit.TFuncSymbolGetIsDeprecatedEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TFuncSymbol(ExtObject).IsDeprecated;
end;

procedure TSymbolUnit.TFuncSymbolGetIsExternalEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TFuncSymbol(ExtObject).IsExternal;
end;

procedure TSymbolUnit.TFuncSymbolGetIsForwardedEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TFuncSymbol(ExtObject).IsForwarded;
end;

procedure TSymbolUnit.TFuncSymbolGetIsLambdaEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TFuncSymbol(ExtObject).IsLambda;
end;

procedure TSymbolUnit.TFuncSymbolGetIsOverloadedEval(info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsBoolean := TFuncSymbol(ExtObject).IsOverloaded;
end;

procedure TSymbolUnit.TFuncSymbolIsSameOverloadOfEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TFuncSymbol(ExtObject).IsOverloaded;
end;

procedure TSymbolUnit.TFuncSymbolGetIsStatelessEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TFuncSymbol(ExtObject).IsStateless;
end;

procedure TSymbolUnit.TFuncSymbolIsValidOverloadOfEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TFuncSymbol(ExtObject).IsValidOverloadOf(
    TFuncSymbol(Info.Params[0].ExternalObject));
end;

procedure TSymbolUnit.TFuncSymbolGetLevelEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TFuncSymbol(ExtObject).Level;
end;

procedure TSymbolUnit.TFuncSymbolGetParamsEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(
    TFuncSymbol(ExtObject).Params);
end;

procedure TSymbolUnit.TFuncSymbolGetResultEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(
    TFuncSymbol(ExtObject).Result);
end;

procedure TSymbolUnit.TFuncSymbolGetExternalNameEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsString := TFuncSymbol(ExtObject).ExternalName;
end;

procedure TSymbolUnit.TFuncSymbolGetKindEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := Integer(TFuncSymbol(ExtObject).Kind);
end;

procedure TSymbolUnit.TFuncSymbolGetParamsDescriptionEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsString := TFuncSymbol(ExtObject).ParamsDescription;
end;
{$ENDREGION}

{$REGION 'Eval Implementation for TInterfaceSymbol'}
procedure TSymbolUnit.TInterfaceSymbolMethodCountEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TInterfaceSymbol(ExtObject).MethodCount;
end;

procedure TSymbolUnit.TInterfaceSymbolGetParentEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(
    TInterfaceSymbol(ExtObject).Parent);
end;
{$ENDREGION}

{$REGION 'Eval Implementation for TMethodSymbol'}
procedure TSymbolUnit.TMethodSymbolGetParentMethEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(
    TMethodSymbol(ExtObject).ParentMeth);
end;

procedure TSymbolUnit.TMethodSymbolGetRootParentMethEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(
    TMethodSymbol(ExtObject).RootParentMeth);
end;

procedure TSymbolUnit.TMethodSymbolGetSelfSymEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(
    TMethodSymbol(ExtObject).SelfSym);
end;

procedure TSymbolUnit.TMethodSymbolGetStructSymbolEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(
    TMethodSymbol(ExtObject).StructSymbol);
end;

procedure TSymbolUnit.TMethodSymbolGetVisibilityEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := Integer(TMethodSymbol(ExtObject).Visibility);
end;

procedure TSymbolUnit.TMethodSymbolGetIsAbstractEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TMethodSymbol(ExtObject).IsAbstract;
end;

procedure TSymbolUnit.TMethodSymbolGetIsClassMethodEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TMethodSymbol(ExtObject).IsClassMethod;
end;

procedure TSymbolUnit.TMethodSymbolGetIsDefaultEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TMethodSymbol(ExtObject).IsDefault;
end;

procedure TSymbolUnit.TMethodSymbolGetIsFinalEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TMethodSymbol(ExtObject).IsFinal;
end;

procedure TSymbolUnit.TMethodSymbolGetIsInterfacedEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TMethodSymbol(ExtObject).IsInterfaced;
end;

procedure TSymbolUnit.TMethodSymbolGetIsOverlapEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TMethodSymbol(ExtObject).IsOverlap;
end;

procedure TSymbolUnit.TMethodSymbolGetIsOverrideEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TMethodSymbol(ExtObject).IsOverride;
end;

procedure TSymbolUnit.TMethodSymbolGetIsStaticEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TMethodSymbol(ExtObject).IsStatic;
end;

procedure TSymbolUnit.TMethodSymbolGetIsVirtualEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TMethodSymbol(ExtObject).IsVirtual;
end;
{$ENDREGION}

{$REGION 'Eval Implementation for TTypeSymbol'}
procedure TSymbolUnit.TTypeSymbolGetDeprecatedMessageEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsString := TTypeSymbol(ExtObject).DeprecatedMessage;
end;

procedure TSymbolUnit.TTypeSymbolGetDistanceToEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TTypeSymbol(ExtObject).DistanceTo(TTypeSymbol(info.Params[0].ExternalObject));
end;

procedure TSymbolUnit.TTypeSymbolHasMetaSymbolEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TTypeSymbol(ExtObject).HasMetaSymbol;
end;

procedure TSymbolUnit.TTypeSymbolIsCompatibleEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TTypeSymbol(ExtObject).IsCompatible(TTypeSymbol(info.Params[0].ExternalObject));
end;

procedure TSymbolUnit.TTypeSymbolGetIsDeprecatedEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TTypeSymbol(ExtObject).IsDeprecated;
end;

procedure TSymbolUnit.TTypeSymbolIsOfTypeEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TTypeSymbol(ExtObject).IsOfType(TTypeSymbol(info.Params[0].ExternalObject));
end;

procedure TSymbolUnit.TTypeSymbolGetSameTypeEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TTypeSymbol(ExtObject).SameType(TTypeSymbol(info.Params[0].ExternalObject));
end;

procedure TSymbolUnit.TTypeSymbolGetUnAliasedTypeEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(TTypeSymbol(ExtObject).UnAliasedType);
end;
{$ENDREGION}

{$REGION 'Eval Implementation for TFieldSymbol'}
procedure TSymbolUnit.TFieldSymbolDefaultValueEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := TFieldSymbol(ExtObject).DefaultValue;
end;

procedure TSymbolUnit.TFieldSymbolExternalNameEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsString := TFieldSymbol(ExtObject).ExternalName;
end;

procedure TSymbolUnit.TFieldSymbolNextFieldEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(
    TFieldSymbol(ExtObject).NextField);
end;

procedure TSymbolUnit.TFieldSymbolStructSymbolEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(
    TFieldSymbol(ExtObject).StructSymbol);
end;

procedure TSymbolUnit.TFieldSymbolVisibilityEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := Integer(TFieldSymbol(ExtObject).Visibility);
end;
{$ENDREGION}

{$REGION 'Eval Implementation for TParamsSymbolTable'}
procedure TSymbolUnit.TParamsSymbolTableGetSymbolEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(
    TParamsSymbolTable(ExtObject).Symbols[info.ParamAsInteger[0]]);
end;

procedure TSymbolUnit.TParamSymbolGetSameParamEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TParamSymbol(ExtObject).SameParam(TParamSymbol(info.Params[0].ExternalObject));
end;

procedure TSymbolUnit.TParamSymbolWithDefaultValueGetDefaultValueEval(
  info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsVariant := TParamSymbolWithDefaultValue(ExtObject).DefaultValue;
end;
{$ENDREGION}

{$REGION 'Eval Implementation for TPropertySymbol'}
procedure TSymbolUnit.TPropertySymbolArrayIndicesEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(TPropertySymbol(ExtObject).ArrayIndices);
end;

procedure TSymbolUnit.TPropertySymbolDeprecatedMessageEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsString := TPropertySymbol(ExtObject).DeprecatedMessage;
end;

procedure TSymbolUnit.TPropertySymbolIndexSymEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(TPropertySymbol(ExtObject).IndexSym);
end;

procedure TSymbolUnit.TPropertySymbolIndexValueEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := TPropertySymbol(ExtObject).IndexValue
end;

procedure TSymbolUnit.TPropertySymbolIsDefaultEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TPropertySymbol(ExtObject).IsDefault;
end;

procedure TSymbolUnit.TPropertySymbolIsDeprecatedEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TPropertySymbol(ExtObject).IsDeprecated;
end;

procedure TSymbolUnit.TPropertySymbolOwnerSymbolEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(TPropertySymbol(ExtObject).OwnerSymbol);
end;

procedure TSymbolUnit.TPropertySymbolReadSymEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(TPropertySymbol(ExtObject).ReadSym);
end;

procedure TSymbolUnit.TPropertySymbolVisibilityEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := Integer(TPropertySymbol(ExtObject).Visibility);
end;

procedure TSymbolUnit.TPropertySymbolWriteSymEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(TPropertySymbol(ExtObject).WriteSym);
end;
{$ENDREGION}

{$REGION 'Eval Implementation for TResourceStringSymbol'}
procedure TSymbolUnit.TRecordSymbolIsDynamicEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TRecordSymbol(ExtObject).IsDynamic;
end;

procedure TSymbolUnit.TRecordSymbolIsFullyDefinedEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TRecordSymbol(ExtObject).IsFullyDefined;
end;
{$ENDREGION}

{$REGION 'Eval Implementation for TResourceStringSymbol'}
procedure TSymbolUnit.TResourceStringSymbolGetIndexEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TResourceStringSymbol(ExtObject).Index;
end;

procedure TSymbolUnit.TResourceStringSymbolGetValueEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsString := TResourceStringSymbol(ExtObject).Value;
end;
{$ENDREGION}

{$REGION 'Eval Implementation for TArraySymbol'}
procedure TSymbolUnit.TArraySymbolGetIndexTypeEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(TArraySymbol(ExtObject).IndexType);
end;
{$ENDREGION}

{$REGION 'Eval Implementation for TClassConstSymbol'}
procedure TSymbolUnit.TClassConstSymbolGetOwnerSymbolEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(TClassConstSymbol(ExtObject).OwnerSymbol);
end;

procedure TSymbolUnit.TClassConstSymbolGetVisibilityEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := Integer(TClassConstSymbol(ExtObject).Visibility);
end;
{$ENDREGION}

{$REGION 'Eval Implementation for TClassSymbol'}
procedure TSymbolUnit.TClassSymbolImplementsInterfaceEval(info: TProgramInfo;
  ExtObject: TObject);
var
  intfSym: TInterfaceSymbol;
begin
  intfSym := TInterfaceSymbol(info.Params[0].ExternalObject);
  info.ResultAsBoolean := TClassSymbol(ExtObject).ImplementsInterface(intfSym);
end;

procedure TSymbolUnit.TClassSymbolGetIsAbstractEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TClassSymbol(ExtObject).IsAbstract;
end;

procedure TSymbolUnit.TClassSymbolGetIsExplicitAbstractEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TClassSymbol(ExtObject).IsExplicitAbstract;
end;

procedure TSymbolUnit.TClassSymbolGetIsExternalEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TClassSymbol(ExtObject).IsExternal;
end;

procedure TSymbolUnit.TClassSymbolGetIsPartialEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TClassSymbol(ExtObject).IsPartial;
end;

procedure TSymbolUnit.TClassSymbolGetIsSealedEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TClassSymbol(ExtObject).IsSealed;
end;

procedure TSymbolUnit.TClassSymbolGetParentEval(info: TProgramInfo;
  ExtObject: TObject);
var
  ParentClass: TClassSymbol;
begin
  ParentClass := TClassSymbol(ExtObject).Parent;
  info.ResultAsVariant := info.RegisterExternalObject(ParentClass);
end;
{$ENDREGION}

{$REGION 'Eval Implementation for TClassVarSymbol'}
procedure TSymbolUnit.TClassVarSymbolGetOwnerSymbolEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(TClassVarSymbol(ExtObject).OwnerSymbol);
end;

procedure TSymbolUnit.TClassVarSymbolGetVisibilityEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := Integer(TClassVarSymbol(ExtObject).Visibility);
end;
{$ENDREGION}

{$REGION 'Eval Implementation for TCompositeTypeSymbol'}
procedure TSymbolUnit.TCompositeTypeSymbolAllowAnonymousMethodsEval(
  info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsBoolean := TCompositeTypeSymbol(ExtObject).AllowAnonymousMethods;
end;

procedure TSymbolUnit.TCompositeTypeSymbolAllowDefaultPropertyEval(
  info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsBoolean := TCompositeTypeSymbol(ExtObject).AllowDefaultProperty;
end;

procedure TSymbolUnit.TCompositeTypeSymbolAllowFieldsEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TCompositeTypeSymbol(ExtObject).AllowFields;
end;

procedure TSymbolUnit.TCompositeTypeSymbolAllowOverloadsEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TCompositeTypeSymbol(ExtObject).AllowOverloads;
end;

procedure TSymbolUnit.TCompositeTypeSymbolAllowVirtualMembersEval(
  info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsBoolean := TCompositeTypeSymbol(ExtObject).AllowVirtualMembers;
end;

procedure TSymbolUnit.TCompositeTypeSymbolGetDefaultPropertyEval(
  info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(TCompositeTypeSymbol(ExtObject).DefaultProperty);
end;

procedure TSymbolUnit.TCompositeTypeSymbolGetExternalNameEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsString := TCompositeTypeSymbol(ExtObject).ExternalName;
end;

procedure TSymbolUnit.TCompositeTypeSymbolExternalRootEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(TCompositeTypeSymbol(ExtObject).ExternalRoot);
end;

procedure TSymbolUnit.TCompositeTypeSymbolFirstFieldEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(TCompositeTypeSymbol(ExtObject).FirstField);
end;

procedure TSymbolUnit.TCompositeTypeSymbolGetParentEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(TCompositeTypeSymbol(ExtObject).Parent);
end;

procedure TSymbolUnit.TCompositeTypeSymbolGetUnitSymbolEval(info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(TCompositeTypeSymbol(ExtObject).UnitSymbol);
end;

procedure TSymbolUnit.TCompositeTypeSymbolGetIsExternalEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TCompositeTypeSymbol(ExtObject).IsExternal;
end;

procedure TSymbolUnit.TCompositeTypeSymbolGetIsExternalRootedEval(
  info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsBoolean := TCompositeTypeSymbol(ExtObject).IsExternalRooted;
end;

procedure TSymbolUnit.TCompositeTypeSymbolGetIsPartialEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TCompositeTypeSymbol(ExtObject).IsPartial;
end;

procedure TSymbolUnit.TCompositeTypeSymbolGetIsStaticEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TCompositeTypeSymbol(ExtObject).IsStatic;
end;

procedure TSymbolUnit.TCompositeTypeSymbolGetMembersEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(TCompositeTypeSymbol(ExtObject).Members);
end;
{$ENDREGION}

{$REGION 'Eval Implementation for TConstSymbol'}
procedure TSymbolUnit.TConstSymbolGetDataEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := TConstSymbol(ExtObject).Data;
end;
{$ENDREGION}

{$REGION 'Eval Implementation for TDataSymbol'}
procedure TSymbolUnit.TDataSymbolGetExternalNameEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsString := TDataSymbol(ExtObject).ExternalName;
end;

procedure TSymbolUnit.TDataSymbolHasExternalNameEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TDataSymbol(ExtObject).HasExternalName;
end;

procedure TSymbolUnit.TDataSymbolGetLevelEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TDataSymbol(ExtObject).Level;
end;
{$ENDREGION}

{$REGION 'Eval Implementation for TElementSymbol'}
procedure TSymbolUnit.TElementSymbolEnumerationEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(
    TElementSymbol(ExtObject).Enumeration);
end;

procedure TSymbolUnit.TElementSymbolIsUserDefEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TElementSymbol(ExtObject).IsUserDef;
end;

procedure TSymbolUnit.TElementSymbolValueEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TElementSymbol(ExtObject).Value;
end;
{$ENDREGION}

{$REGION 'Eval Implementation for TEnumerationSymbol'}
procedure TSymbolUnit.TEnumerationSymbolContinuousEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TEnumerationSymbol(ExtObject).Continuous;
end;

procedure TSymbolUnit.TEnumerationSymbolDefaultValueEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TEnumerationSymbol(ExtObject).DefaultValue;
end;

procedure TSymbolUnit.TEnumerationSymbolElementsEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(TEnumerationSymbol(ExtObject).Elements);
end;

procedure TSymbolUnit.TEnumerationSymbolHighBoundEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TEnumerationSymbol(ExtObject).HighBound;
end;

procedure TSymbolUnit.TEnumerationSymbolLowBoundEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TEnumerationSymbol(ExtObject).LowBound;
end;

procedure TSymbolUnit.TEnumerationSymbolShortDescriptionEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsString := TEnumerationSymbol(ExtObject).ShortDescription;
end;

procedure TSymbolUnit.TEnumerationSymbolStyleEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := Integer(TEnumerationSymbol(ExtObject).Style);
end;
{$ENDREGION}

{$REGION 'Eval Implementation for TExternalVarSymbol'}
procedure TSymbolUnit.TExternalVarSymbolReadFuncEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(
    TExternalVarSymbol(ExtObject).ReadFunc);
end;

procedure TSymbolUnit.TExternalVarSymbolWriteFuncEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(
    TExternalVarSymbol(ExtObject).WriteFunc);
end;
{$ENDREGION}


{ TDocumentationBuilder }

constructor TDocumentationBuilder.Create(AProgram: IdwsProgram);
begin
  FProgram := AProgram;

  FCompiler := TDelphiWebScript.Create(nil);
  FHtmlFilter := TdwsHtmlFilter.Create(nil);
  FCompiler.Config.Filter := FHtmlFilter;
  FClassesLib := TdwsClassesLib.Create(nil);
  FClassesLib.Script := FCompiler;
  FSymbolsLib := TdwsSymbolsLib.Create(nil);
  FSymbolsLib.Script := FCompiler;

  GenerateTemplate;
end;

destructor TDocumentationBuilder.Destroy;
begin
  FreeAndNil(FClassesLib);
  FreeAndNil(FSymbolsLib);
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

function TDocumentationBuilder.BuildContent(const Template: string;
  Level: Integer; const Symbol: TSymbol): string;
var
  prog: IdwsProgram;
  exec: IdwsProgramExecution;
  SymbolUnit: TSymbolUnit;
begin
  SymbolUnit := TSymbolUnit.Create(Symbol, Level);
  try
    SymbolUnit.Script := FCompiler;
    prog := FCompiler.Compile(Template);
    try
      if prog.Msgs.HasErrors then
      begin
        Result := prog.Msgs.AsInfo;
        Exit;
      end;

      {$IFDEF UseDebugger}
      if Assigned(FDebugger) then
      begin
        exec := prog.CreateNewExecution;
        FDebugger.BeginDebug(exec);
        FDebugger.EndDebug;
        if (Exec.Msgs.Count = 0) then
          Result := exec.Result.ToString
        else
          Abort;
      end
      else
      {$ENDIF}
      begin
        exec := prog.Execute;
        Result := exec.Result.ToString;
      end;

      if exec.Msgs.HasErrors then
        OutputDebugString(PWideChar(exec.Msgs.AsInfo));
    finally
      prog := nil;
    end;
  finally
    FreeAndNil(SymbolUnit);
  end;
end;

procedure TDocumentationBuilder.Abort;
begin
  FAborted := True;
end;

procedure TDocumentationBuilder.BuildContentFile(const FileName: string;
  const Symbol: TSymbol; const Level: Integer = -1);
var
  Content: string;
begin
  // set content from template
  Content := FTemplateSource;

  // eventually call before build content
  if Assigned(FOnBeginBuildContent) then
    FOnBeginBuildContent(Self, FileName, Symbol, Content);

  // check if content is present at all
  if FAborted or (Content = '') then
    Exit;

  // build documentation content
  Content := BuildContent(Content, Level, Symbol);

  // eventually call after build content
  if Assigned(FOnAfterBuildContent) then
    FOnAfterBuildContent(Self, FileName, Symbol, Content);

  // check if content is present at all
  if FAborted or (Content = '') then
    Exit;

  // save content to file
  {$IFDEF DoNotWriteFile}
  SaveTextToUTF8File(FileName, Content);
  {$ENDIF}
end;

procedure TDocumentationBuilder.Build(const Directory: string);
var
  Symbol: TSymbol;
  UnitsDir: string;
begin
  FAborted := False;

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

  // check for abortion
  if FAborted then
    Exit;

  BuildContentFile(UnitsDir + 'index' + FFileExtension, nil, 1);
  BuildContentFile(Directory + 'index' + FFileExtension, nil, 0);
end;

procedure TDocumentationBuilder.BuildUnit(const Directory: string; UnitSymbol: TUnitSymbol);
var
  Symbol: TSymbol;
  UnitDir: string;

  procedure BuildContentFileMember(Directory: string);
  var
    FileName: TFileName;
  begin
    // check for abortion
    if FAborted then
      Exit;

    // create directoy
    Directory := UnitDir + Directory;
    if not DirectoryExists(Directory) then
      CreateDir(Directory);

    // build filename
    FileName := Directory + Symbol.Name + FFileExtension;

    // actually build content file
    BuildContentFile(FileName, Symbol, 3);
  end;

begin
  // check if unit is empty
  if (UnitSymbol.Table = nil) or (UnitSymbol.Table.Count = 0) then
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
      BuildContentFileMember('Constants\');
    end
    else if Symbol is TFuncSymbol then
    begin
      // build documentation for function symbol
      BuildContentFileMember('Routines\');
    end
    else if Symbol is TTypeSymbol then
    begin
      // build documentation for function symbol
      BuildContentFileMember('Types\');
    end;
  end;

  // check for abortion
  if FAborted then
    Exit;

  // build unit index
  BuildContentFile(UnitDir + 'index' + FFileExtension, UnitSymbol, 2);
end;

procedure TDocumentationBuilder.BuildClass(const Directory: string;
  ClassSymbol: TClassSymbol);
var
  MemberSymbol: TSymbol;

  procedure BuildContentFileMember(const Directory: string);
  var
    FileName: TFileName;
  begin
    // check for abortion
    if FAborted then
      Exit;

    // create directoy
    if not DirectoryExists(Directory) then
      CreateDir(Directory);

    // build filename
    FileName := Directory + MemberSymbol.Name + FFileExtension;

    BuildContentFile(FileName, MemberSymbol, 5);
  end;

var
  ClassDir: string;
begin
  ClassDir := Directory + ClassSymbol.Name + '\';

  // check if directory exists
  if not DirectoryExists(ClassDir) then
    CreateDir(ClassDir);

  for MemberSymbol in ClassSymbol.Members do
  begin
    if MemberSymbol is TMethodSymbol then
      BuildContentFileMember(ClassDir + 'Methods\')
    else if MemberSymbol is TPropertySymbol then
      BuildContentFileMember(ClassDir + 'Properties\');
  end;

  // check for abortion
  if FAborted then
    Exit;

  // build class index
  BuildContentFile(ClassDir + 'index' + FFileExtension, ClassSymbol, 4);
end;

end.
