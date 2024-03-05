object dwsIniFileLib: TdwsIniFileLib
  Height = 150
  Width = 215
  object dwsIniFile: TdwsUnit
    Classes = <
      item
        Name = 'TIniFile'
        Constructors = <
          item
            Name = 'Create'
            Parameters = <
              item
                Name = 'fileName'
                DataType = 'String'
              end>
            OnEval = dwsIniFileClassesTIniFileConstructorsCreateEval
          end
          item
            Name = 'CreateInMemory'
            Parameters = <
              item
                Name = 'content'
                DataType = 'String'
              end>
            OnEval = dwsIniFileClassesTIniFileConstructorsCreateInMemoryEval
          end>
        Methods = <
          item
            Name = 'FileName'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsIniFileClassesTIniFileMethodsFileNameEval
          end
          item
            Name = 'EraseSection'
            Parameters = <
              item
                Name = 'section'
                DataType = 'String'
              end>
            Kind = mkProcedure
            OnEval = dwsIniFileClassesTIniFileMethodsEraseSectionEval
          end
          item
            Name = 'DeleteKey'
            Parameters = <
              item
                Name = 'section'
                DataType = 'String'
              end
              item
                Name = 'name'
                DataType = 'String'
              end>
            Kind = mkProcedure
            OnEval = dwsIniFileClassesTIniFileMethodsDeleteKeyEval
          end
          item
            Name = 'ReadString'
            Parameters = <
              item
                Name = 'section'
                DataType = 'String'
              end
              item
                Name = 'name'
                DataType = 'String'
              end
              item
                Name = 'default'
                DataType = 'String'
                HasDefaultValue = True
                DefaultValue = ''
              end>
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsIniFileClassesTIniFileMethodsReadStringEval
          end
          item
            Name = 'WriteString'
            Parameters = <
              item
                Name = 'section'
                DataType = 'String'
              end
              item
                Name = 'name'
                DataType = 'String'
              end
              item
                Name = 'value'
                DataType = 'String'
              end>
            Kind = mkProcedure
            OnEval = dwsIniFileClassesTIniFileMethodsWriteStringEval
          end
          item
            Name = 'ReadSections'
            ResultType = 'array of String'
            Kind = mkFunction
            OnEval = dwsIniFileClassesTIniFileMethodsReadSectionsEval
          end
          item
            Name = 'ReadSectionNames'
            Parameters = <
              item
                Name = 'section'
                DataType = 'String'
              end>
            ResultType = 'array of String'
            Kind = mkFunction
            OnEval = dwsIniFileClassesTIniFileMethodsReadSectionNamesEval
          end
          item
            Name = 'GetEncoding'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsIniFileClassesTIniFileMethodsGetEncodingEval
          end
          item
            Name = 'SetEncoding'
            Parameters = <
              item
                Name = 'n'
                DataType = 'String'
              end>
            Kind = mkProcedure
            OnEval = dwsIniFileClassesTIniFileMethodsSetEncodingEval
          end
          item
            Name = 'ToString'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsIniFileClassesTIniFileMethodsToStringEval
          end>
        Properties = <
          item
            Name = 'Encoding'
            DataType = 'String'
            ReadAccess = 'GetEncoding'
            WriteAccess = 'SetEncoding'
          end>
        OnCleanUp = dwsIniFileClassesTIniFileCleanUp
      end>
    UnitName = 'System.IniFiles'
    StaticSymbols = True
    Left = 72
    Top = 32
  end
end
