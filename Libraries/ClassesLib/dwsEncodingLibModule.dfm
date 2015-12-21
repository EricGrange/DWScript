object dwsEncodingLib: TdwsEncodingLib
  OldCreateOrder = False
  Left = 695
  Top = 86
  Height = 150
  Width = 215
  object dwsEncoding: TdwsUnit
    Classes = <
      item
        Name = 'Encoder'
        IsAbstract = True
        Methods = <
          item
            Name = 'Encode'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maAbstract]
            Kind = mkClassFunction
          end
          item
            Name = 'Decode'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maAbstract]
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'UTF8Encoder'
        Ancestor = 'Encoder'
        Methods = <
          item
            Name = 'Encode'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsEncodingClassesUTF8EncoderMethodsEncodeEval
            Kind = mkClassFunction
          end
          item
            Name = 'Decode'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsEncodingClassesUTF8EncoderMethodsDecodeEval
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'Base64Encoder'
        Ancestor = 'Encoder'
        Methods = <
          item
            Name = 'Encode'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsEncodingClassesBase64EncoderMethodsEncodeEval
            Kind = mkClassFunction
          end
          item
            Name = 'Decode'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsEncodingClassesBase64EncoderMethodsDecodeEval
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'Base58Encoder'
        Ancestor = 'Encoder'
        Methods = <
          item
            Name = 'Encode'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsEncodingClassesBase58EncoderMethodsEncodeEval
            Kind = mkClassFunction
          end
          item
            Name = 'Decode'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsEncodingClassesBase58EncoderMethodsDecodeEval
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'Base32Encoder'
        Ancestor = 'Encoder'
        Methods = <
          item
            Name = 'Encode'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsEncodingClassesBase32EncoderMethodsEncodeEval
            Kind = mkClassFunction
          end
          item
            Name = 'Decode'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsEncodingClassesBase32EncoderMethodsDecodeEval
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'HexadecimalEncoder'
        Ancestor = 'Encoder'
        Methods = <
          item
            Name = 'Encode'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsEncodingClassesHexadecimalEncoderMethodsEncodeEval
            Kind = mkClassFunction
          end
          item
            Name = 'Decode'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsEncodingClassesHexadecimalEncoderMethodsDecodeEval
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'URLEncodedEncoder'
        Ancestor = 'Encoder'
        Methods = <
          item
            Name = 'Encode'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsEncodingClassesURLEncodedEncoderMethodsEncodeEval
            Kind = mkClassFunction
          end
          item
            Name = 'Decode'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsEncodingClassesURLEncodedEncoderMethodsDecodeEval
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'HTMLTextEncoder'
        Ancestor = 'Encoder'
        Methods = <
          item
            Name = 'Encode'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsEncodingClassesHTMLTextEncoderMethodsEncodeEval
            Kind = mkClassFunction
          end
          item
            Name = 'Decode'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsEncodingClassesHTMLTextEncoderMethodsDecodeEval
            Kind = mkClassFunction
          end>
      end
      item
        Name = 'HTMLAttributeEncoder'
        Ancestor = 'Encoder'
        Methods = <
          item
            Name = 'Encode'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsEncodingClassesHTMLAttributeEncoderMethodsEncodeEval
            Kind = mkClassFunction
          end
          item
            Name = 'Decode'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsEncodingClassesHTMLAttributeEncoderMethodsDecodeEval
            Kind = mkClassFunction
          end>
      end>
    UnitName = 'System.Encoding'
    StaticSymbols = True
    Left = 72
    Top = 32
  end
end
