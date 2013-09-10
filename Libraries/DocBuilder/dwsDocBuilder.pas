unit dwsDocBuilder;

interface

uses
  SysUtils, Classes, dwsUnitSymbols, dwsExprs, dwsCompiler,
  dwsComp, dwsHtmlFilter, dwsSymbols, dwsUtils, dwsXPlatform;

type
  TDocumentationBuilder = class;

  TBuildContent = procedure(Sender: TDocumentationBuilder;
                            const FileName : TFileName; const Symbol: TSymbol;
                            var Content: string) of object;

  TSymbolUnit = class(TdwsUnit)
  private
    FSymbol: TSymbol;
    FLevel: Integer;
    FScriptObj: IScriptObj;
    procedure GetSymbol(info: TProgramInfo);
    procedure GetLevel(info: TProgramInfo);
    procedure TSymbolGetCaptionEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolGetDescriptionEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolIsBaseTypeEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolIsFuncSymbolEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolIsTypeEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolGetNameEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolGetTypeEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSymbolGetQualifiedNameEval(info: TProgramInfo; ExtObject: TObject);
    procedure TResourceStringSymbolGetValueEval(info: TProgramInfo; ExtObject: TObject);
    procedure TResourceStringSymbolGetIndexEval(info: TProgramInfo; ExtObject: TObject);
    procedure TConstSymbolGetDataEval(info: TProgramInfo; ExtObject: TObject);
    procedure TParamSymbolGetSameParamEval(info: TProgramInfo; ExtObject: TObject);
    procedure TParamSymbolWithDefaultValueGetDefaultValueEval(info: TProgramInfo; ExtObject: TObject);
    procedure TDataSymbolHasExternalNameEval(info: TProgramInfo; ExtObject: TObject);
    procedure TDataSymbolGetExternalNameEval(info: TProgramInfo; ExtObject: TObject);
    procedure TDataSymbolGetLevelEval(info: TProgramInfo; ExtObject: TObject);
    procedure TTypeSymbolGetDeprecatedMessageEval(info: TProgramInfo; ExtObject: TObject);
    procedure TTypeSymbolIsDeprecatedEval(info: TProgramInfo; ExtObject: TObject);
    procedure TTypeSymbolHasMetaSymbolEval(info: TProgramInfo; ExtObject: TObject);
    procedure TTypeSymbolGetUnAliasedTypeEval(info: TProgramInfo; ExtObject: TObject);
    procedure TTypeSymbolIsOfTypeEval(info: TProgramInfo; ExtObject: TObject);
    procedure TTypeSymbolIsCompatibleEval(info: TProgramInfo; ExtObject: TObject);
    procedure TTypeSymbolGetDistanceToEval(info: TProgramInfo; ExtObject: TObject);
    procedure TTypeSymbolGetSameTypeEval(info: TProgramInfo; ExtObject: TObject);
    procedure TCompositeTypeSymbolGetParentEval(info: TProgramInfo; ExtObject: TObject);
    procedure TCompositeTypeSymbolGetUnitSymbolEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolIsOverloadedEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolGetParamsDescriptionEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolHasParamEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolGetParamTypeEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolIsDeprecatedEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolIsStatelessEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolIsForwardedEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolIsExternalEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolIsValidOverloadOfEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolIsSameOverloadOfEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolGetKindEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolGetExternalNameEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolHasExternalNameEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolIsLambdaEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolLevelEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolParamsEval(info: TProgramInfo; ExtObject: TObject);
    procedure TFuncSymbolResultEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolGetParentMethEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolGetRootParentMethEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolGetSelfSymEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolGetStructSymbolEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolGetVisibilityEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolIsAbstractEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolIsClassMethodEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolIsDefaultEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolIsFinalEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolIsInterfacedEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolIsOverlapEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolIsOverrideEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolIsStaticEval(info: TProgramInfo; ExtObject: TObject);
    procedure TMethodSymbolIsVirtualEval(info: TProgramInfo; ExtObject: TObject);
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
    procedure TParamsSymbolTableGetSymbolEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSetOfSymbolMinValueEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSetOfSymbolMaxValueEval(info: TProgramInfo; ExtObject: TObject);
    procedure TSetOfSymbolCountValueEval(info: TProgramInfo; ExtObject: TObject);
    procedure TStaticArraySymbolIsEmptyArrayEval(info: TProgramInfo; ExtObject: TObject);
    procedure TStaticArraySymbolHighBoundEval(info: TProgramInfo; ExtObject: TObject);
    procedure TStaticArraySymbolLowBoundEval(info: TProgramInfo; ExtObject: TObject);
    procedure TStaticArraySymbolElementCountEval(info: TProgramInfo; ExtObject: TObject);
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

  with Synonyms.Add do
  begin
    Name := 'TSymbolClass';
    DataType := 'TClass';
  end;

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

  // definition for TResourceStringSymbol
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

  // definition for TValueSymbol
  with Classes.Add do
  begin
    Name := 'TValueSymbol';
    Ancestor := 'TSymbol';
  end;

  // definition for TConstSymbol
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

  // definition for TDataSymbol
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

  // definition for TParamSymbol
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

  // definition for TParamSymbolWithDefaultValue
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

  // definition for TTypeSymbol
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
      Name := 'DeprecatedMessage';
      ResultType := 'String';
      OnEval := TTypeSymbolGetDeprecatedMessageEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsDeprecated';
      ResultType := 'Boolean';
      OnEval := TTypeSymbolIsDeprecatedEval;
    end;
  end;

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
(*
    function FindLocal(const aName : UnicodeString; ofClass : TSymbolClass = nil) : TSymbol; virtual;
    function FindTypeLocal(const aName : UnicodeString) : TTypeSymbol;
    function FindSymbolAtStackAddr(const stackAddr, level : Integer) : TDataSymbol;
    function FindSymbol(const aName : UnicodeString; minVisibility : TdwsVisibility;
      ofClass : TSymbolClass = nil) : TSymbol; virtual;
    function FindTypeSymbol(const aName : UnicodeString; minVisibility : TdwsVisibility) : TTypeSymbol;

    function HasSameLocalOperator(anOpSym : TOperatorSymbol) : Boolean; virtual;
*)
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
      Name := 'Count';
      ResultType := 'Integer';
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
      Name := 'ParentCount';
      ResultType := 'Integer';
      OnEval := TSymbolTableGetParentCountEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetParent';
      ResultType := 'TSymbolTable';
      with Parameters.Add do
      begin
        Name := 'Index';
        DataType := 'Integer';
      end;
      OnEval := TSymbolTableGetParentEval;
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

  with Classes.Add do
  begin
    Name := 'TUnSortedSymbolTable';
    Ancestor := 'TSymbolTable';
  end;

  with Classes.Add do
  begin
    Name := 'TParamsSymbolTable';
    Ancestor := 'TUnSortedSymbolTable';
    with Methods.Add do
     begin
       Name := 'GetParamSymbol';
       ResultType := 'TParamSymbol';
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

  // definition for TFuncSymbol
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
      Name := 'IsDeprecated';
      ResultType := 'Boolean';
      OnEval := TFuncSymbolIsDeprecatedEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsStateless';
      ResultType := 'Boolean';
      OnEval := TFuncSymbolIsStatelessEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsForwarded';
      ResultType := 'Boolean';
      OnEval := TFuncSymbolIsForwardedEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsOverloaded';
      ResultType := 'Boolean';
      OnEval := TFuncSymbolIsOverloadedEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsExternal';
      ResultType := 'Boolean';
      OnEval := TFuncSymbolIsExternalEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetKind';
      ResultType := 'TFuncKind';
      OnEval := TFuncSymbolGetKindEval;
    end;
    with Methods.Add do
    begin
      Name := 'GetExternalName';
      ResultType := 'String';
      OnEval := TFuncSymbolGetExternalNameEval;
    end;
    with Methods.Add do
    begin
      Name := 'HasExternalName';
      ResultType := 'Boolean';
      OnEval := TFuncSymbolHasExternalNameEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsLambda';
      ResultType := 'Boolean';
      OnEval := TFuncSymbolIsLambdaEval;
    end;
    with Methods.Add do
    begin
      Name := 'Level';
      ResultType := 'Integer';
      OnEval := TFuncSymbolLevelEval;
    end;
    with Methods.Add do
    begin
      Name := 'Params';
      ResultType := 'TParamsSymbolTable';
      OnEval := TFuncSymbolParamsEval;
    end;
    with Methods.Add do
    begin
      Name := 'Result';
      ResultType := 'TDataSymbol';
      OnEval := TFuncSymbolResultEval;
    end;
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
  end;

  // definition for TMethodSymbol
  with Classes.Add do
  begin
    Name := 'TMethodSymbol';
    Ancestor := 'TFuncSymbol';

    with Methods.Add do
    begin
      Name := 'StructSymbol';
      ResultType := 'TCompositeTypeSymbol';
      OnEval := TMethodSymbolGetStructSymbolEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsDefault';
      ResultType := 'Boolean';
      OnEval := TMethodSymbolIsDefaultEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsAbstract';
      ResultType := 'Boolean';
      OnEval := TMethodSymbolIsAbstractEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsVirtual';
      ResultType := 'Boolean';
      OnEval := TMethodSymbolIsVirtualEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsOverride';
      ResultType := 'Boolean';
      OnEval := TMethodSymbolIsOverrideEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsInterfaced';
      ResultType := 'Boolean';
      OnEval := TMethodSymbolIsInterfacedEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsFinal';
      ResultType := 'Boolean';
      OnEval := TMethodSymbolIsFinalEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsOverlap';
      ResultType := 'Boolean';
      OnEval := TMethodSymbolIsOverlapEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsClassMethod';
      ResultType := 'Boolean';
      OnEval := TMethodSymbolIsClassMethodEval;
    end;
    with Methods.Add do
    begin
      Name := 'IsStatic';
      ResultType := 'Boolean';
      OnEval := TMethodSymbolIsStaticEval;
    end;
    with Methods.Add do
    begin
      Name := 'ParentMeth';
      ResultType := 'TMethodSymbol';
      OnEval := TMethodSymbolGetParentMethEval;
    end;
    with Methods.Add do
    begin
      Name := 'RootParentMeth';
      ResultType := 'TMethodSymbol';
      OnEval := TMethodSymbolGetRootParentMethEval;
    end;
    with Methods.Add do
    begin
      Name := 'SelfSym';
      ResultType := 'TDataSymbol';
      OnEval := TMethodSymbolGetSelfSymEval;
    end;
    with Methods.Add do
    begin
      Name := 'Visibility';
      ResultType := 'TdwsVisibility';
      OnEval := TMethodSymbolGetVisibilityEval;
    end;
  end;

  // definition for TSetOfSymbol
  with Classes.Add do
  begin
    Name := 'TSetOfSymbol';
    Ancestor := 'TTypeSymbol';
    with Methods.Add do
    begin
      Name := 'MinValue';
      ResultType := 'Integer';
      OnEval := TSetOfSymbolMinValueEval;
    end;
    with Methods.Add do
    begin
      Name := 'MaxValue';
      ResultType := 'Integer';
      OnEval := TSetOfSymbolMaxValueEval;
    end;
    with Methods.Add do
    begin
      Name := 'CountValue';
      ResultType := 'Integer';
      OnEval := TSetOfSymbolCountValueEval;
    end;
  end;

  // definition for TSetOfSymbol
  with Classes.Add do
  begin
    Name := 'TStaticArraySymbol';
    Ancestor := 'TTypeSymbol';
    with Methods.Add do
    begin
      Name := 'IsEmptyArray';
      ResultType := 'Boolean';
      OnEval := TStaticArraySymbolIsEmptyArrayEval;
    end;
    with Methods.Add do
    begin
      Name := 'HighBound';
      ResultType := 'Integer';
      OnEval := TStaticArraySymbolHighBoundEval;
    end;
    with Methods.Add do
    begin
      Name := 'LowBound';
      ResultType := 'Integer';
      OnEval := TStaticArraySymbolLowBoundEval;
    end;
    with Methods.Add do
    begin
      Name := 'ElementCount';
      ResultType := 'Integer';
      OnEval := TStaticArraySymbolElementCountEval;
    end;
  end;

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

(*
  with Classes.Add do
  begin
    Name := 'TClassConstSymbol';
    Ancestor := 'TConstSymbol';

    with Methods.Add do
    begin
      Name := 'OwnerSymbol';
      ResultType := 'TCompositeTypeSymbol';
      OnEval := TClassConstSymbolGetOwnerSymbolEval;
    end;
    with Methods.Add do
    begin
      Name := 'Visibility';
      ResultType := 'TdwsVisibility';
      OnEval := TClassConstSymbolGetVisibilityEval;
    end;
  end;

  with Classes.Add do
  begin
    Name := 'TClassVarSymbol';
    Ancestor := 'TVarSymbol';

    with Methods.Add do
    begin
      Name := 'OwnerSymbol';
      ResultType := 'TCompositeTypeSymbol';
      OnEval := TClassConstSymbolGetOwnerSymbolEval;
    end;
    with Methods.Add do
    begin
      Name := 'Visibility';
      ResultType := 'TdwsVisibility';
      OnEval := TClassConstSymbolGetVisibilityEval;
    end;
  end;
*)


  // definition for TCompositeTypeSymbol
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

    with Properties.Add do
    begin
      Name := 'Name';
      DataType := 'TUnitSymbol';
      ReadAccess := 'GetUnitSymbol';
    end;
    with Properties.Add do
    begin
      Name := 'Name';
      DataType := 'TCompositeTypeSymbol';
      ReadAccess := 'GetParent';
    end;
  end;

  // definition for TStructuredTypeSymbol
  with Classes.Add do
  begin
    Name := 'TStructuredTypeSymbol';
    Ancestor := 'TCompositeTypeSymbol';
  end;

  // definition for TClassSymbol
  with Classes.Add do
  begin
    Name := 'TClassSymbol';
    Ancestor := 'TStructuredTypeSymbol';
  end;

  // definition for TUnitSymbol
  with Classes.Add do
  begin
    Name := 'TUnitSymbol';
    Ancestor := 'TSymbol';
  end;

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


  FScriptObj := nil;
end;

procedure TSymbolUnit.GetLevel(info: TProgramInfo);
begin
  info.ResultAsInteger := FLevel;
end;

procedure TSymbolUnit.GetSymbol(info: TProgramInfo);
begin
  if not Assigned(FScriptObj) then
    FScriptObj := info.RegisterExternalObject(FSymbol, False, False);

  info.ResultAsVariant := FScriptObj;
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
  info.ResultAsBoolean := TSymbol(ExtObject).IsFuncSymbol;
end;

procedure TSymbolUnit.TSymbolIsTypeEval(info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsBoolean := TSymbol(ExtObject).IsType;
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

procedure TSymbolUnit.TSetOfSymbolCountValueEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TSetOfSymbol(ExtObject).MinValue;
end;

procedure TSymbolUnit.TSetOfSymbolMaxValueEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TSetOfSymbol(ExtObject).MaxValue;
end;

procedure TSymbolUnit.TSetOfSymbolMinValueEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TSetOfSymbol(ExtObject).CountValue;
end;

procedure TSymbolUnit.TStaticArraySymbolElementCountEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TStaticArraySymbol(ExtObject).ElementCount;
end;

procedure TSymbolUnit.TStaticArraySymbolHighBoundEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TStaticArraySymbol(ExtObject).HighBound;
end;

procedure TSymbolUnit.TStaticArraySymbolIsEmptyArrayEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TStaticArraySymbol(ExtObject).IsEmptyArray;
end;

procedure TSymbolUnit.TStaticArraySymbolLowBoundEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TStaticArraySymbol(ExtObject).LowBound;
end;

procedure TSymbolUnit.TSymbolGetCaptionEval(info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsString := TSymbol(ExtObject).Caption;
end;

procedure TSymbolUnit.TFuncSymbolGetParamTypeEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(
    TFuncSymbol(ExtObject).GetParamType(info.ParamAsInteger[0]));
end;

procedure TSymbolUnit.TFuncSymbolHasExternalNameEval(info: TProgramInfo;
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

procedure TSymbolUnit.TFuncSymbolIsDeprecatedEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TFuncSymbol(ExtObject).IsDeprecated;
end;

procedure TSymbolUnit.TFuncSymbolIsExternalEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TFuncSymbol(ExtObject).IsExternal;
end;

procedure TSymbolUnit.TFuncSymbolIsForwardedEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TFuncSymbol(ExtObject).IsForwarded;
end;

procedure TSymbolUnit.TFuncSymbolIsLambdaEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TFuncSymbol(ExtObject).IsLambda;
end;

procedure TSymbolUnit.TFuncSymbolIsOverloadedEval(info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsBoolean := TFuncSymbol(ExtObject).IsOverloaded;
end;

procedure TSymbolUnit.TFuncSymbolIsSameOverloadOfEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TFuncSymbol(ExtObject).IsOverloaded;
end;

procedure TSymbolUnit.TFuncSymbolIsStatelessEval(info: TProgramInfo;
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

procedure TSymbolUnit.TFuncSymbolLevelEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsInteger := TFuncSymbol(ExtObject).Level;
end;

procedure TSymbolUnit.TFuncSymbolParamsEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(
    TFuncSymbol(ExtObject).Params);
end;

procedure TSymbolUnit.TFuncSymbolResultEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(
    TFuncSymbol(ExtObject).Result);
end;

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

procedure TSymbolUnit.TMethodSymbolIsAbstractEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TMethodSymbol(ExtObject).IsAbstract;
end;

procedure TSymbolUnit.TMethodSymbolIsClassMethodEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TMethodSymbol(ExtObject).IsClassMethod;
end;

procedure TSymbolUnit.TMethodSymbolIsDefaultEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TMethodSymbol(ExtObject).IsDefault;
end;

procedure TSymbolUnit.TMethodSymbolIsFinalEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TMethodSymbol(ExtObject).IsFinal;
end;

procedure TSymbolUnit.TMethodSymbolIsInterfacedEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TMethodSymbol(ExtObject).IsInterfaced;
end;

procedure TSymbolUnit.TMethodSymbolIsOverlapEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TMethodSymbol(ExtObject).IsOverlap;
end;

procedure TSymbolUnit.TMethodSymbolIsOverrideEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TMethodSymbol(ExtObject).IsOverride;
end;

procedure TSymbolUnit.TMethodSymbolIsStaticEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TMethodSymbol(ExtObject).IsStatic;
end;

procedure TSymbolUnit.TMethodSymbolIsVirtualEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsBoolean := TMethodSymbol(ExtObject).IsVirtual;
end;

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

procedure TSymbolUnit.TTypeSymbolIsDeprecatedEval(info: TProgramInfo;
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

procedure TSymbolUnit.TCompositeTypeSymbolGetParentEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(TCompositeTypeSymbol(ExtObject).Parent);
end;

procedure TSymbolUnit.TCompositeTypeSymbolGetUnitSymbolEval(info: TProgramInfo; ExtObject: TObject);
begin
  info.ResultAsVariant := info.RegisterExternalObject(TCompositeTypeSymbol(ExtObject).UnitSymbol);
end;

procedure TSymbolUnit.TConstSymbolGetDataEval(info: TProgramInfo;
  ExtObject: TObject);
begin
  info.ResultAsVariant := TConstSymbol(ExtObject).Data;
end;

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
