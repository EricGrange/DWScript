object dwsCryptoLib: TdwsCryptoLib
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 215
  Width = 195
  object dwsCrypto: TdwsUnit
    Classes = <
      item
        Name = 'HashAlgorithm'
        IsAbstract = True
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
        Name = 'HashSHA512'
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
            OnEval = dwsCryptoClassesHashSHA512MethodsHashDataEval
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
            OnEval = dwsCryptoClassesHashSHA512MethodsHMACEval
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
        IsStatic = True
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
        IsStatic = True
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
        Name = 'EncryptionAESSHA3CTR'
        Ancestor = 'EncryptionAlgorithm'
        IsStatic = True
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
            OnEval = dwsCryptoClassesEncryptionAESSHA3CTRMethodsEncryptDataEval
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
            OnEval = dwsCryptoClassesEncryptionAESSHA3CTRMethodsDecryptDataEval
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'EncryptionCryptProtect'
        Ancestor = 'EncryptionAlgorithm'
        IsStatic = True
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
      end
      item
        Name = 'Nonces'
        IsStatic = True
        Methods = <
          item
            Name = 'Register'
            Parameters = <
              item
                Name = 'nonce'
                DataType = 'String'
              end
              item
                Name = 'millisecondsUntilExpiration'
                DataType = 'Integer'
              end
              item
                Name = 'data'
                DataType = 'String'
                HasDefaultValue = True
                DefaultValue = ''
              end>
            Attributes = [maStatic]
            OnFastEvalNoResult = dwsCryptoClassesNoncesMethodsRegisterFastEvalNoResult
            Kind = mkClassProcedure
          end
          item
            Name = 'Generate'
            Parameters = <
              item
                Name = 'millisecondsUntilExpiration'
                DataType = 'Integer'
              end
              item
                Name = 'data'
                DataType = 'String'
                HasDefaultValue = True
                DefaultValue = ''
              end>
            ResultType = 'String'
            Attributes = [maStatic]
            OnFastEvalString = dwsCryptoClassesNoncesMethodsGenerateFastEvalString
            Kind = mkClassFunction
          end
          item
            Name = 'CheckAndKeep'
            Parameters = <
              item
                Name = 'nonce'
                DataType = 'String'
              end
              item
                Name = 'data'
                DataType = 'String'
                HasDefaultValue = True
                DefaultValue = ''
              end>
            ResultType = 'Boolean'
            Attributes = [maStatic]
            OnFastEvalBoolean = dwsCryptoClassesNoncesMethodsCheckAndKeepFastEvalBoolean
            Kind = mkClassFunction
          end
          item
            Name = 'CheckAndRemove'
            Parameters = <
              item
                Name = 'nonce'
                DataType = 'String'
              end
              item
                Name = 'data'
                DataType = 'String'
                HasDefaultValue = True
                DefaultValue = ''
              end>
            ResultType = 'Boolean'
            Attributes = [maStatic]
            OnFastEvalBoolean = dwsCryptoClassesNoncesMethodsCheckAndRemoveFastEvalBoolean
            Kind = mkClassFunction
          end
          item
            Name = 'GetData'
            Parameters = <
              item
                Name = 'nonce'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maStatic]
            OnFastEvalString = dwsCryptoClassesNoncesMethodsGetDataFastEvalString
            Kind = mkClassFunction
          end
          item
            Name = 'Remove'
            Parameters = <
              item
                Name = 'nonce'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            OnFastEvalNoResult = dwsCryptoClassesNoncesMethodsRemoveFastEvalNoResult
            Kind = mkClassProcedure
          end
          item
            Name = 'RemoveByData'
            Parameters = <
              item
                Name = 'data'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            OnFastEvalNoResult = dwsCryptoClassesNoncesMethodsRemoveByDataFastEvalNoResult
            Kind = mkClassProcedure
          end
          item
            Name = 'Clear'
            Attributes = [maStatic]
            OnFastEvalNoResult = dwsCryptoClassesNoncesMethodsClearFastEvalNoResult
            Kind = mkClassProcedure
          end
          item
            Name = 'Collect'
            Attributes = [maStatic]
            OnFastEvalNoResult = dwsCryptoClassesNoncesMethodsCollectFastEvalNoResult
            Kind = mkClassProcedure
          end>
      end
      item
        Name = 'ECCsecp256r1'
        IsStatic = True
        Methods = <
          item
            Name = 'MakeKey'
            Parameters = <
              item
                Name = 'pubKey'
                DataType = 'String'
                IsVarParam = True
              end
              item
                Name = 'privKey'
                DataType = 'String'
                IsVarParam = True
              end>
            ResultType = 'Boolean'
            Attributes = [maStatic]
            OnEval = dwsCryptoClassesECCsecp256r1MethodsMakeKeyEval
            Kind = mkClassMethod
          end
          item
            Name = 'ECDHSharedSecret'
            Parameters = <
              item
                Name = 'pubKey'
                DataType = 'String'
              end
              item
                Name = 'privKey'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsCryptoClassesECCsecp256r1MethodsECDHSharedSecretEval
            Kind = mkClassFunction
          end
          item
            Name = 'ECDSASign'
            Parameters = <
              item
                Name = 'privKey'
                DataType = 'String'
              end
              item
                Name = 'hash256hex'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maStatic]
            OnEval = dwsCryptoClassesECCsecp256r1MethodsECDSASignEval
            Kind = mkClassFunction
          end
          item
            Name = 'ECDSAVerify'
            Parameters = <
              item
                Name = 'pubKey'
                DataType = 'String'
              end
              item
                Name = 'hash256hex'
                DataType = 'String'
              end
              item
                Name = 'signature'
                DataType = 'String'
              end>
            ResultType = 'Boolean'
            Attributes = [maStatic]
            OnEval = dwsCryptoClassesECCsecp256r1MethodsECDSAVerifyEval
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'TRSAKey'
        Constructors = <
          item
            Name = 'Generate'
            Parameters = <
              item
                Name = 'bitSize'
                DataType = 'Integer'
              end>
            OnEval = dwsCryptoClassesTRSAKeyConstructorsGenerateEval
          end
          item
            Name = 'ImportJSON'
            Parameters = <
              item
                Name = 'jsonData'
                DataType = 'String'
              end>
            OnEval = dwsCryptoClassesTRSAKeyConstructorsImportJSONEval
          end>
        Methods = <
          item
            Name = 'DestroyKey'
            OnEval = dwsCryptoClassesTRSAKeyMethodsDestroyKeyEval
            Kind = mkProcedure
          end
          item
            Name = 'SignHash'
            Parameters = <
              item
                Name = 'paddingAlgorithm'
                DataType = 'String'
              end
              item
                Name = 'hashHex'
                DataType = 'String'
              end>
            ResultType = 'String'
            OnEval = dwsCryptoClassesTRSAKeyMethodsSignHashEval
            Kind = mkFunction
          end
          item
            Name = 'VerifyHash'
            Parameters = <
              item
                Name = 'paddingAlgorithm'
                DataType = 'String'
              end
              item
                Name = 'hashHex'
                DataType = 'String'
              end
              item
                Name = 'signature'
                DataType = 'String'
              end>
            ResultType = 'Boolean'
            OnEval = dwsCryptoClassesTRSAKeyMethodsVerifyHashEval
            Kind = mkFunction
          end
          item
            Name = 'ExportJSON'
            ResultType = 'String'
            OnEval = dwsCryptoClassesTRSAKeyMethodsExportJSONEval
            Kind = mkFunction
          end
          item
            Name = 'Encrypt'
            Parameters = <
              item
                Name = 'data'
                DataType = 'String'
              end
              item
                Name = 'paddingAlgorithm'
                DataType = 'String'
              end
              item
                Name = 'initializationVector'
                DataType = 'String'
              end>
            ResultType = 'String'
            OnEval = dwsCryptoClassesTRSAKeyMethodsEncryptEval
            Kind = mkFunction
          end
          item
            Name = 'Decrypt'
            Parameters = <
              item
                Name = 'data'
                DataType = 'String'
              end
              item
                Name = 'paddingAlgorithm'
                DataType = 'String'
              end
              item
                Name = 'initializationVector'
                DataType = 'String'
              end>
            ResultType = 'String'
            OnEval = dwsCryptoClassesTRSAKeyMethodsDecryptEval
            Kind = mkFunction
          end
          item
            Name = 'BlockLength'
            ResultType = 'Integer'
            OnEval = dwsCryptoClassesTRSAKeyMethodsBlockLengthEval
            Kind = mkFunction
          end>
        OnCleanUp = dwsCryptoClassesTRSAKeyCleanUp
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
        OnFastEval = dwsCryptoFunctionsCryptographicRandomFastEval
      end
      item
        Name = 'ProcessUniqueRandom'
        ResultType = 'String'
        OnFastEval = dwsCryptoFunctionsProcessUniqueRandomFastEval
      end
      item
        Name = 'CryptographicToken'
        Parameters = <
          item
            Name = 'bitStrength'
            DataType = 'Integer'
            HasDefaultValue = True
            DefaultValue = 120
          end>
        ResultType = 'String'
        OnFastEval = dwsCryptoFunctionsCryptographicTokenFastEval
      end
      item
        Name = 'CompilationUniqueRandom'
        ResultType = 'String'
        OnFastEval = dwsCryptoFunctionsCompilationUniqueRandomFastEval
      end>
    UnitName = 'System.Crypto'
    StaticSymbols = True
    Left = 64
    Top = 32
  end
end
