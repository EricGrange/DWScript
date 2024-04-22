object dwsEncodingLib: TdwsEncodingLib
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesUTF8EncoderMethodsEncodeEval
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesUTF8EncoderMethodsDecodeEval
          end
          item
            Name = 'IsValidUTF8'
            Parameters = <
              item
                Name = 'data'
                DataType = 'String'
              end>
            ResultType = 'Boolean'
            Attributes = [maStatic]
            Kind = mkClassFunction
            OnFastEvalBoolean = dwsEncodingClassesUTF8EncoderMethodsIsValidUTF8FastEvalBoolean
          end>
      end
      item
        Name = 'UTF16BigEndianEncoder'
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesUTF16BigEndianEncoderMethodsEncodeEval
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesUTF16BigEndianEncoderMethodsDecodeEval
          end>
      end
      item
        Name = 'UTF16LittleEndianEncoder'
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesUTF16LittleEndianEncoderMethodsEncodeEval
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesUTF16LittleEndianEncoderMethodsDecodeEval
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesBase64EncoderMethodsEncodeEval
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesBase64EncoderMethodsDecodeEval
          end
          item
            Name = 'EncodeMIME'
            Parameters = <
              item
                Name = 'v'
                DataType = 'String'
              end>
            ResultType = 'String'
            Attributes = [maClassMethod, maStatic]
            Kind = mkClassFunction
            OnFastEvalString = dwsEncodingClassesBase64EncoderMethodsEncodeMIMEFastEvalString
          end>
      end
      item
        Name = 'Base64URIEncoder'
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesBase64URLEncoderMethodsEncodeEval
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesBase64URLEncoderMethodsDecodeEval
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesBase58EncoderMethodsEncodeEval
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesBase58EncoderMethodsDecodeEval
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesBase32EncoderMethodsEncodeEval
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesBase32EncoderMethodsDecodeEval
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesHexadecimalEncoderMethodsEncodeEval
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesHexadecimalEncoderMethodsDecodeEval
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesURLEncodedEncoderMethodsEncodeEval
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesURLEncodedEncoderMethodsDecodeEval
          end>
      end
      item
        Name = 'MIMEEncodedWordEncoder'
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesMIMEEncodedWordEncoderMethodsEncodeEval
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesMIMEEncodedWordEncoderMethodsDecodeEval
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesHTMLTextEncoderMethodsEncodeEval
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesHTMLTextEncoderMethodsDecodeEval
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesHTMLAttributeEncoderMethodsEncodeEval
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesHTMLAttributeEncoderMethodsDecodeEval
          end>
      end
      item
        Name = 'XMLTextEncoder'
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesXMLTextEncoderMethodsEncodeEval
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
            Kind = mkClassFunction
            OnEval = dwsEncodingClassesXMLTextEncoderMethodsDecodeEval
          end>
      end>
    UnitName = 'System.Encoding'
    StaticSymbols = True
    Left = 72
    Top = 32
  end
end
