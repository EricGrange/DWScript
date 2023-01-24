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
            Kind = mkDestructor
            OnEval = dwsUnitClassesTSymbolsMethodsDestroyEval
          end
          item
            Name = 'First'
            Kind = mkProcedure
            OnEval = dwsUnitClassesTSymbolsMethodsFirstEval
          end
          item
            Name = 'Last'
            Kind = mkProcedure
            OnEval = dwsUnitClassesTSymbolsMethodsLastEval
          end
          item
            Name = 'Next'
            Kind = mkProcedure
            OnEval = dwsUnitClassesTSymbolsMethodsNextEval
          end
          item
            Name = 'Previous'
            Kind = mkProcedure
            OnEval = dwsUnitClassesTSymbolsMethodsPreviousEval
          end
          item
            Name = 'Eof'
            ResultType = 'Boolean'
            Kind = mkFunction
            OnEval = dwsUnitClassesTSymbolsMethodsEofEval
          end
          item
            Name = 'Name'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsUnitClassesTSymbolsMethodsNameEval
          end
          item
            Name = 'Caption'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsUnitClassesTSymbolsMethodsCaptionEval
          end
          item
            Name = 'Description'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsUnitClassesTSymbolsMethodsDescriptionEval
          end
          item
            Name = 'SymbolType'
            ResultType = 'TSymbolType'
            Kind = mkFunction
            OnEval = dwsUnitClassesTSymbolsMethodsSymbolTypeEval
          end
          item
            Name = 'Visibility'
            ResultType = 'TSymbolVisibility'
            Kind = mkFunction
            OnEval = dwsUnitClassesTSymbolsMethodsVisibilityEval
          end
          item
            Name = 'GetMembers'
            ResultType = 'TSymbols'
            Kind = mkFunction
            OnEval = dwsUnitClassesTSymbolsMethodsGetMembersEval
          end
          item
            Name = 'GetParameters'
            ResultType = 'TSymbols'
            Kind = mkFunction
            OnEval = dwsUnitClassesTSymbolsMethodsGetParametersEval
          end
          item
            Name = 'GetSuperSymbol'
            ResultType = 'TSymbols'
            Kind = mkFunction
            OnEval = dwsUnitClassesTSymbolsMethodsGetSuperClassEval
          end
          item
            Name = 'Locate'
            Parameters = <
              item
                Name = 'Name'
                DataType = 'String'
              end>
            ResultType = 'Boolean'
            Kind = mkFunction
            OnEval = dwsUnitClassesTSymbolsMethodsLocateEval
          end
          item
            Name = 'QualifiedName'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsUnitClassesTSymbolsMethodsQualifiedNameEval
          end
          item
            Name = 'IsType'
            ResultType = 'Boolean'
            Kind = mkFunction
            OnEval = dwsUnitClassesTSymbolsMethodsIsTypeEval
          end
          item
            Name = 'GetMetaSymbol'
            ResultType = 'TSymbols'
            Kind = mkFunction
            OnEval = dwsUnitClassesTSymbolsMethodsMetaSymbolEval
          end
          item
            Name = 'InternalClassName'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsUnitClassesTSymbolsMethodsInternalClassNameEval
          end
          item
            Name = 'TypeName'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsUnitClassesTSymbolsMethodsTypeNameEval
          end
          item
            Name = 'GetType'
            ResultType = 'TSymbols'
            Kind = mkFunction
            OnEval = dwsUnitClassesTSymbolsMethodsGetTypeEval
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
