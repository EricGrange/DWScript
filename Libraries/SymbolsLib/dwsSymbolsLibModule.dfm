object dwsSymbolsLib: TdwsSymbolsLib
  OldCreateOrder = False
  Left = 285
  Top = 161
  Height = 198
  Width = 193
  object dwsUnit1: TdwsUnit
    Classes = <
      item
        Name = 'TSymbols'
        IsSealed = False
        IsAbstract = False
        IsStatic = False
        Constructors = <
          item
            Name = 'CreateMain'
            OnEval = dwsUnit1ClassesTSymbolsConstructorsCreateMainEval
          end
          item
            Name = 'CreateUnit'
            Parameters = <
              item
                Name = 'Name'
                DataType = 'String'
              end>
            OnEval = dwsUnit1ClassesTSymbolsConstructorsCreateUnitEval
          end
          item
            Name = 'CreateUid'
            Parameters = <
              item
                Name = 'Uid'
                DataType = 'String'
              end>
            OnEval = dwsUnit1ClassesTSymbolsConstructorsCreateUidEval
          end>
        Fields = <>
        Methods = <
          item
            Name = 'Destroy'
            OnEval = dwsUnit1ClassesTSymbolsMethodsDestroyEval
            Attributes = [maOverride]
            Kind = mkDestructor
          end
          item
            Name = 'First'
            OnEval = dwsUnit1ClassesTSymbolsMethodsFirstEval
            Kind = mkProcedure
          end
          item
            Name = 'Last'
            OnEval = dwsUnit1ClassesTSymbolsMethodsLastEval
            Kind = mkProcedure
          end
          item
            Name = 'Next'
            OnEval = dwsUnit1ClassesTSymbolsMethodsNextEval
            Kind = mkProcedure
          end
          item
            Name = 'Previous'
            OnEval = dwsUnit1ClassesTSymbolsMethodsPreviousEval
            Kind = mkProcedure
          end
          item
            Name = 'Eof'
            ResultType = 'Boolean'
            OnEval = dwsUnit1ClassesTSymbolsMethodsEofEval
            Kind = mkFunction
          end
          item
            Name = 'Name'
            ResultType = 'String'
            OnEval = dwsUnit1ClassesTSymbolsMethodsNameEval
            Kind = mkFunction
          end
          item
            Name = 'Caption'
            ResultType = 'String'
            OnEval = dwsUnit1ClassesTSymbolsMethodsCaptionEval
            Kind = mkFunction
          end
          item
            Name = 'Description'
            ResultType = 'String'
            OnEval = dwsUnit1ClassesTSymbolsMethodsDescriptionEval
            Kind = mkFunction
          end
          item
            Name = 'SymbolType'
            ResultType = 'TSymbolType'
            OnEval = dwsUnit1ClassesTSymbolsMethodsSymbolTypeEval
            Kind = mkFunction
          end
          item
            Name = 'GetMembers'
            ResultType = 'TSymbols'
            OnEval = dwsUnit1ClassesTSymbolsMethodsGetMembersEval
            Kind = mkFunction
          end
          item
            Name = 'GetParameters'
            ResultType = 'TSymbols'
            OnEval = dwsUnit1ClassesTSymbolsMethodsGetParametersEval
            Kind = mkFunction
          end
          item
            Name = 'GetSuperClass'
            ResultType = 'TSymbols'
            OnEval = dwsUnit1ClassesTSymbolsMethodsGetSuperClassEval
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
            OnEval = dwsUnit1ClassesTSymbolsMethodsLocateEval
            Kind = mkFunction
          end>
        Operators = <>
        Constants = <>
        Properties = <>
      end>
    Dependencies.Strings = (
      'Classes')
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
            UserDefValue = 0
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
            Name = 'stMember'
            UserDefValue = 6
            IsUserDef = True
          end
          item
            Name = 'stParam'
            UserDefValue = 7
            IsUserDef = True
          end
          item
            Name = 'stProperty'
            UserDefValue = 8
            IsUserDef = True
          end
          item
            Name = 'stRecord'
            UserDefValue = 9
            IsUserDef = True
          end
          item
            Name = 'stUnit'
            UserDefValue = 10
            IsUserDef = True
          end
          item
            Name = 'stVariable'
            UserDefValue = 11
            IsUserDef = True
          end>
      end>
    UnitName = 'Symbols'
    StaticSymbols = False
    Left = 60
    Top = 40
  end
end
