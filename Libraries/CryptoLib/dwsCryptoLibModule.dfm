object dwsCryptoLib: TdwsCryptoLib
  OldCreateOrder = False
  Left = 810
  Top = 86
  Height = 215
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
            Attributes = [maVirtual, maAbstract]
            Kind = mkClassFunction
          end
          item
            Name = 'HMAC'
            Parameters = <
              item
                Name = 'key'
                DataType = 'String'
              end
              item
                Name = 'message'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maAbstract]
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'HashSHA256'
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
          end
          item
            Name = 'HMAC'
            Parameters = <
              item
                Name = 'key'
                DataType = 'String'
              end
              item
                Name = 'message'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsCryptoClassesHashSHA256MethodsHMACEval
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'HashSHA1'
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
          end
          item
            Name = 'HMAC'
            Parameters = <
              item
                Name = 'key'
                DataType = 'String'
              end
              item
                Name = 'message'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsCryptoClassesHashSHA1MethodsHMACEval
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'HashMD5'
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
          end
          item
            Name = 'HMAC'
            Parameters = <
              item
                Name = 'key'
                DataType = 'String'
              end
              item
                Name = 'message'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsCryptoClassesHashMD5MethodsHMACEval
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'HashRIPEMD160'
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
            OnEval = dwsCryptoClassesHashRIPEMD160MethodsHashDataEval
            Kind = mkClassFunction
          end
          item
            Name = 'HMAC'
            Parameters = <
              item
                Name = 'key'
                DataType = 'String'
              end
              item
                Name = 'message'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsCryptoClassesHashRIPEMD160MethodsHMACEval
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'HashCRC32'
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
            OnEval = dwsCryptoClassesHashCRC32MethodsHashDataEval
            Kind = mkClassFunction
          end
          item
            Name = 'HMAC'
            Parameters = <
              item
                Name = 'key'
                DataType = 'String'
              end
              item
                Name = 'message'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsCryptoClassesHashCRC32MethodsHMACEval
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'HashSHA3_256'
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
            OnEval = dwsCryptoClassesHashSHA3_256MethodsHashDataEval
            Kind = mkClassFunction
          end
          item
            Name = 'HMAC'
            Parameters = <
              item
                Name = 'key'
                DataType = 'String'
              end
              item
                Name = 'message'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsCryptoClassesHashSHA3_256MethodsHMACEval
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'EncryptionAlgorithm'
        IsAbstract = True
        Methods = <
          item
            Name = 'EncryptData'
            Parameters = <
              item
                Name = 'data'
                DataType = 'String'
              end
              item
                Name = 'key'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maAbstract]
            Kind = mkClassFunction
          end
          item
            Name = 'DecryptData'
            Parameters = <
              item
                Name = 'data'
                DataType = 'String'
              end
              item
                Name = 'key'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maAbstract]
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'EncryptionAESSHA256Full'
        Ancestor = 'EncryptionAlgorithm'
        Methods = <
          item
            Name = 'EncryptData'
            Parameters = <
              item
                Name = 'data'
                DataType = 'String'
              end
              item
                Name = 'key'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsCryptoClassesEncryptionAESSHA256FullMethodsEncryptDataEval
            Kind = mkClassFunction
          end
          item
            Name = 'DecryptData'
            Parameters = <
              item
                Name = 'data'
                DataType = 'String'
              end
              item
                Name = 'key'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsCryptoClassesEncryptionAESSHA256FullMethodsDecryptDataEval
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'EncryptionCryptProtect'
        Ancestor = 'EncryptionAlgorithm'
        Methods = <
          item
            Name = 'EncryptData'
            Parameters = <
              item
                Name = 'data'
                DataType = 'String'
              end
              item
                Name = 'key'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsCryptoClassesEncryptionCryptProtectMethodsEncryptDataEval
            Kind = mkClassFunction
          end
          item
            Name = 'DecryptData'
            Parameters = <
              item
                Name = 'data'
                DataType = 'String'
              end
              item
                Name = 'key'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsCryptoClassesEncryptionCryptProtectMethodsDecryptDataEval
            Kind = mkClassFunction
          end>
      end>
    Functions = <
      item
        Name = 'CryptographicRandom'
        Parameters = <
          item
            Name = 'nbBytes'
            DataType = 'Integer'
          end>
        ResultType = 'String'
        OnEval = dwsCryptoFunctionsCryptographicRandomEval
      end
      item
        Name = 'ProcessUniqueRandom'
        ResultType = 'String'
        OnEval = dwsCryptoFunctionsProcessUniqueRandomEval
      end>
    UnitName = 'System.Crypto'
    StaticSymbols = True
    Left = 64
    Top = 32
  end
end
