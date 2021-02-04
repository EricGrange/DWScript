object dwsSymbolsLib: TdwsSymbolsLib
  OldCreateOrder = False
  Height = 164
  Width = 177
  object dwsUnit: TdwsUnit
    Classes = <
      item
        Name = 'TSymbols'
        Constructors = <
          item
            Name = 'CreateMain'
            OnEval = dwsUnitClassesTSymbolsConstructorsCreateMainEval
          end
          item
            Name = 'CreateUnit'
            Parameters = <
              item
                Name = 'Name'
                DataType = 'String'
              end>
            OnEval = dwsUnitClassesTSymbolsConstructorsCreateUnitEval
          end
          item
            Name = 'CreateUid'
            Parameters = <
              item
                Name = 'Uid'
                DataType = 'String'
              end>
            OnEval = dwsUnitClassesTSymbolsConstructorsCreateUidEval
          end>
        Methods = <
          item
            Name = 'Destroy'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsUnitClassesTSymbolsMethodsDestroyEval
            Kind = mkDestructor
          end
          item
            Name = 'First'
            OnEval = dwsUnitClassesTSymbolsMethodsFirstEval
            Kind = mkProcedure
          end
          item
            Name = 'Last'
            OnEval = dwsUnitClassesTSymbolsMethodsLastEval
            Kind = mkProcedure
          end
          item
            Name = 'Next'
            OnEval = dwsUnitClassesTSymbolsMethodsNextEval
            Kind = mkProcedure
          end
          item
            Name = 'Previous'
            OnEval = dwsUnitClassesTSymbolsMethodsPreviousEval
            Kind = mkProcedure
          end
          item
            Name = 'Eof'
            ResultType = 'Boolean'
            OnEval = dwsUnitClassesTSymbolsMethodsEofEval
            Kind = mkFunction
          end
          item
            Name = 'Name'
            ResultType = 'String'
            OnEval = dwsUnitClassesTSymbolsMethodsNameEval
            Kind = mkFunction
          end
          item
            Name = 'Caption'
            ResultType = 'String'
            OnEval = dwsUnitClassesTSymbolsMethodsCaptionEval
            Kind = mkFunction
          end
          item
            Name = 'Description'
            ResultType = 'String'
            OnEval = dwsUnitClassesTSymbolsMethodsDescriptionEval
            Kind = mkFunction
          end
          item
            Name = 'SymbolType'
            ResultType = 'TSymbolType'
            OnEval = dwsUnitClassesTSymbolsMethodsSymbolTypeEval
            Kind = mkFunction
          end
          item
            Name = 'Visibility'
            ResultType = 'TSymbolVisibility'
            OnEval = dwsUnitClassesTSymbolsMethodsVisibilityEval
            Kind = mkFunction
          end
          item
            Name = 'GetMembers'
            ResultType = 'TSymbols'
            OnEval = dwsUnitClassesTSymbolsMethodsGetMembersEval
            Kind = mkFunction
          end
          item
            Name = 'GetParameters'
            ResultType = 'TSymbols'
            OnEval = dwsUnitClassesTSymbolsMethodsGetParametersEval
            Kind = mkFunction
          end
          item
            Name = 'GetSuperSymbol'
            ResultType = 'TSymbols'
            OnEval = dwsUnitClassesTSymbolsMethodsGetSuperClassEval
            Kind = mkFunction
          end
          item
            Name = 'Locate'
            Parameters = <
              item
                Name = 'Name'
                DataType = 'String'
              end>
            ResultType = 'Boolean'
            OnEval = dwsUnitClassesTSymbolsMethodsLocateEval
            Kind = mkFunction
          end
          item
            Name = 'QualifiedName'
            ResultType = 'String'
            OnEval = dwsUnitClassesTSymbolsMethodsQualifiedNameEval
            Kind = mkFunction
          end
          item
            Name = 'IsType'
            ResultType = 'Boolean'
            OnEval = dwsUnitClassesTSymbolsMethodsIsTypeEval
            Kind = mkFunction
          end
          item
            Name = 'GetMetaSymbol'
            ResultType = 'TSymbols'
            OnEval = dwsUnitClassesTSymbolsMethodsMetaSymbolEval
            Kind = mkFunction
          end
          item
            Name = 'InternalClassName'
            ResultType = 'String'
            OnEval = dwsUnitClassesTSymbolsMethodsInternalClassNameEval
            Kind = mkFunction
          end
          item
            Name = 'TypeName'
            ResultType = 'String'
            OnEval = dwsUnitClassesTSymbolsMethodsTypeNameEval
            Kind = mkFunction
          end
          item
            Name = 'GetType'
            ResultType = 'TSymbols'
            OnEval = dwsUnitClassesTSymbolsMethodsGetTypeEval
            Kind = mkFunction
          end>
      end>
    Enumerations = <
      item
        Name = 'TSymbolType'
        Elements = <
          item
            Name = 'stUnknown'
            UserDefValue = -1
            IsUserDef = True
          end
          item
            Name = 'stArray'
            IsUserDef = True
          end
          item
            Name = 'stAlias'
            UserDefValue = 1
            IsUserDef = True
          end
          item
            Name = 'stClass'
            UserDefValue = 2
            IsUserDef = True
          end
          item
            Name = 'stConstant'
            UserDefValue = 3
            IsUserDef = True
          end
          item
            Name = 'stField'
            UserDefValue = 4
            IsUserDef = True
          end
          item
            Name = 'stFunction'
            UserDefValue = 5
            IsUserDef = True
          end
          item
            Name = 'stParam'
            UserDefValue = 6
            IsUserDef = True
          end
          item
            Name = 'stProperty'
            UserDefValue = 7
            IsUserDef = True
          end
          item
            Name = 'stRecord'
            UserDefValue = 8
            IsUserDef = True
          end
          item
            Name = 'stUnit'
            UserDefValue = 9
            IsUserDef = True
          end
          item
            Name = 'stVariable'
            UserDefValue = 10
            IsUserDef = True
          end
          item
            Name = 'stInterface'
            UserDefValue = 11
            IsUserDef = True
          end
          item
            Name = 'stEnumeration'
            UserDefValue = 12
            IsUserDef = True
          end
          item
            Name = 'stMetaClass'
            UserDefValue = 13
            IsUserDef = True
          end>
      end
      item
        Name = 'TSymbolVisibility'
        Elements = <
          item
            Name = 'visUnknown'
          end
          item
            Name = 'visPrivate'
          end
          item
            Name = 'visProtected'
          end
          item
            Name = 'visPublic'
          end
          item
            Name = 'visPublished'
          end>
      end>
    UnitName = 'SystemSymbols'
    StaticSymbols = False
    Left = 52
    Top = 24
  end
end
