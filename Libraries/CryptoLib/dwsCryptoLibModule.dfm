object dwsCryptoLib: TdwsCryptoLib
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
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesSHA256MethodsHashDataEval
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
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesHashSHA256MethodsHMACEval
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
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesHashSHA512MethodsHashDataEval
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
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesHashSHA512MethodsHMACEval
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
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesSHA1MethodsHashDataEval
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
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesHashSHA1MethodsHMACEval
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
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesMD5MethodsHashDataEval
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
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesHashMD5MethodsHMACEval
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
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesHashRIPEMD160MethodsHashDataEval
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
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesHashRIPEMD160MethodsHMACEval
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
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesHashCRC32MethodsHashDataEval
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
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesHashCRC32MethodsHMACEval
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
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesHashSHA3_256MethodsHashDataEval
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
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesHashSHA3_256MethodsHMACEval
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
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesEncryptionAESSHA256FullMethodsEncryptDataEval
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
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesEncryptionAESSHA256FullMethodsDecryptDataEval
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
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesEncryptionAESSHA3CTRMethodsEncryptDataEval
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
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesEncryptionAESSHA3CTRMethodsDecryptDataEval
          end>
      end
      item
        Name = 'EncryptionAESnistCTR'
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
              end
              item
                Name = 'iv'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesEncryptionAESnistCTRMethodsEncryptDataEval
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
              end
              item
                Name = 'iv'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesEncryptionAESnistCTRMethodsDecryptDataEval
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
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesEncryptionCryptProtectMethodsEncryptDataEval
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
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesEncryptionCryptProtectMethodsDecryptDataEval
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
            Kind = mkClassProcedure
            OnFastEvalNoResult = dwsCryptoClassesNoncesMethodsRegisterFastEvalNoResult
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
            Kind = mkClassFunction
            OnFastEvalString = dwsCryptoClassesNoncesMethodsGenerateFastEvalString
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
            Kind = mkClassFunction
            OnFastEvalBoolean = dwsCryptoClassesNoncesMethodsCheckAndKeepFastEvalBoolean
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
            Kind = mkClassFunction
            OnFastEvalBoolean = dwsCryptoClassesNoncesMethodsCheckAndRemoveFastEvalBoolean
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
            Kind = mkClassFunction
            OnFastEvalString = dwsCryptoClassesNoncesMethodsGetDataFastEvalString
          end
          item
            Name = 'Remove'
            Parameters = <
              item
                Name = 'nonce'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnFastEvalNoResult = dwsCryptoClassesNoncesMethodsRemoveFastEvalNoResult
          end
          item
            Name = 'RemoveByData'
            Parameters = <
              item
                Name = 'data'
                DataType = 'String'
              end>
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnFastEvalNoResult = dwsCryptoClassesNoncesMethodsRemoveByDataFastEvalNoResult
          end
          item
            Name = 'Clear'
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnFastEvalNoResult = dwsCryptoClassesNoncesMethodsClearFastEvalNoResult
          end
          item
            Name = 'Collect'
            Attributes = [maStatic]
            Kind = mkClassProcedure
            OnFastEvalNoResult = dwsCryptoClassesNoncesMethodsCollectFastEvalNoResult
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
            Kind = mkClassMethod
            OnEval = dwsCryptoClassesECCsecp256r1MethodsMakeKeyEval
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
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesECCsecp256r1MethodsECDHSharedSecretEval
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
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesECCsecp256r1MethodsECDSASignEval
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
            Kind = mkClassFunction
            OnEval = dwsCryptoClassesECCsecp256r1MethodsECDSAVerifyEval
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
            Kind = mkProcedure
            OnEval = dwsCryptoClassesTRSAKeyMethodsDestroyKeyEval
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
            Kind = mkFunction
            OnEval = dwsCryptoClassesTRSAKeyMethodsSignHashEval
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
            Kind = mkFunction
            OnEval = dwsCryptoClassesTRSAKeyMethodsVerifyHashEval
          end
          item
            Name = 'ExportJSON'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsCryptoClassesTRSAKeyMethodsExportJSONEval
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
            Kind = mkFunction
            OnEval = dwsCryptoClassesTRSAKeyMethodsEncryptEval
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
            Kind = mkFunction
            OnEval = dwsCryptoClassesTRSAKeyMethodsDecryptEval
          end
          item
            Name = 'BlockLength'
            ResultType = 'Integer'
            Kind = mkFunction
            OnEval = dwsCryptoClassesTRSAKeyMethodsBlockLengthEval
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
      end
      item
        Name = 'PBKDF2_HMAC_SHA256'
        Parameters = <
          item
            Name = 'password'
            DataType = 'String'
          end
          item
            Name = 'salt'
            DataType = 'String'
          end
          item
            Name = 'iterations'
            DataType = 'Integer'
          end>
        ResultType = 'String'
        OnFastEval = dwsCryptoFunctionsPBKDF2_HMAC_SHA256FastEval
      end>
    UnitName = 'System.Crypto'
    StaticSymbols = True
    Left = 64
    Top = 32
  end
end
