object dwsCryptoLib: TdwsCryptoLib
  OldCreateOrder = False
  Left = 810
  Top = 86
  Height = 172
  Width = 195
  object dwsCrypto: TdwsUnit
    Classes = <
      item
        Name = 'HashAlgorithm'
        IsAbstract = True
        Methods = <
          item
            Name = 'HashData'
            Parameters = <
              item
                Name = 'data'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual]
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'SHA256'
        Ancestor = 'HashAlgorithm'
        Methods = <
          item
            Name = 'HashData'
            Parameters = <
              item
                Name = 'data'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsCryptoClassesSHA256MethodsHashDataEval
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'SHA1'
        Ancestor = 'HashAlgorithm'
        IsStatic = True
        Methods = <
          item
            Name = 'HashData'
            Parameters = <
              item
                Name = 'data'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsCryptoClassesSHA1MethodsHashDataEval
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'MD5'
        Ancestor = 'HashAlgorithm'
        IsStatic = True
        Methods = <
          item
            Name = 'HashData'
            Parameters = <
              item
                Name = 'data'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsCryptoClassesMD5MethodsHashDataEval
            Kind = mkClassFunction
          end>
      end>
    UnitName = 'System.Crypto'
    StaticSymbols = True
    Left = 72
    Top = 32
  end
end
