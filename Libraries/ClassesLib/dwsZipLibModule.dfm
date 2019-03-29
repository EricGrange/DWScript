object dwsZipLib: TdwsZipLib
  OldCreateOrder = False
  Height = 150
  Width = 215
  object dwsZip: TdwsUnit
    Classes = <
      item
        Name = 'TZipReader'
        Constructors = <
          item
            Name = 'Create'
            Parameters = <
              item
                Name = 'fileName'
                DataType = 'String'
              end>
            OnEval = dwsZipClassesTZipReaderConstructorsCreateEval
          end
          item
            Name = 'FromData'
            Parameters = <
              item
                Name = 'data'
                DataType = 'String'
              end>
            OnEval = dwsZipClassesTZipReaderConstructorsFromDataEval
          end>
        Methods = <
          item
            Name = 'Count'
            ResultType = 'Integer'
            OnEval = dwsZipClassesTZipReaderMethodsCountEval
            Kind = mkFunction
          end
          item
            Name = 'GetName'
            Parameters = <
              item
                Name = 'index'
                DataType = 'Integer'
              end>
            ResultType = 'String'
            OnEval = dwsZipClassesTZipReaderMethodsGetNameEval
            Kind = mkFunction
          end
          item
            Name = 'GetData'
            Parameters = <
              item
                Name = 'index'
                DataType = 'Integer'
              end>
            ResultType = 'String'
            OnEval = dwsZipClassesTZipReaderMethodsGetDataEval
            Kind = mkFunction
          end
          item
            Name = 'GetFullSize'
            Parameters = <
              item
                Name = 'index'
                DataType = 'Integer'
              end>
            ResultType = 'Integer'
            OnEval = dwsZipClassesTZipReaderMethodsGetFullSizeEval
            Kind = mkFunction
          end
          item
            Name = 'GetZipSize'
            Parameters = <
              item
                Name = 'index'
                DataType = 'Integer'
              end>
            ResultType = 'Integer'
            OnEval = dwsZipClassesTZipReaderMethodsGetZipSizeEval
            Kind = mkFunction
          end
          item
            Name = 'IndexOf'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
            ResultType = 'Integer'
            OnEval = dwsZipClassesTZipReaderMethodsIndexOfEval
            Kind = mkFunction
          end
          item
            Name = 'Unzip'
            Parameters = <
              item
                Name = 'index'
                DataType = 'Integer'
              end
              item
                Name = 'toFileName'
                DataType = 'String'
              end>
            ResultType = 'Boolean'
            OnEval = dwsZipClassesTZipReaderMethodsUnzipEval
            Kind = mkFunction
          end
          item
            Name = 'Close'
            Kind = mkDestructor
          end>
        Properties = <
          item
            Name = 'Names'
            DataType = 'String'
            ReadAccess = 'GetName'
            Parameters = <
              item
                Name = 'index'
                DataType = 'Integer'
              end>
          end
          item
            Name = 'Data'
            DataType = 'String'
            ReadAccess = 'GetData'
            Parameters = <
              item
                Name = 'index'
                DataType = 'Integer'
              end>
          end
          item
            Name = 'FullSize'
            DataType = 'Integer'
            ReadAccess = 'GetFullSize'
            Parameters = <
              item
                Name = 'index'
                DataType = 'Integer'
              end>
          end
          item
            Name = 'ZipSize'
            DataType = 'Integer'
            ReadAccess = 'GetZipSize'
            Parameters = <
              item
                Name = 'index'
                DataType = 'Integer'
              end>
          end>
        OnCleanUp = dwsZipClassesTZipReaderCleanUp
      end
      item
        Name = 'TZipWriter'
        Constructors = <
          item
            Name = 'Create'
            Parameters = <
              item
                Name = 'fileName'
                DataType = 'String'
              end>
            OnEval = dwsZipClassesTZipWriterConstructorsCreateEval
          end>
        Methods = <
          item
            Name = 'AddFile'
            Parameters = <
              item
                Name = 'fileName'
                DataType = 'String'
              end
              item
                Name = 'compression'
                DataType = 'Integer'
                HasDefaultValue = True
                DefaultValue = 7
              end
              item
                Name = 'nameInZip'
                DataType = 'String'
                HasDefaultValue = True
                DefaultValue = ''
              end>
            OnEval = dwsZipClassesTZipWriterMethodsAddFileEval
            Kind = mkProcedure
          end
          item
            Name = 'AddData'
            Parameters = <
              item
                Name = 'data'
                DataType = 'String'
              end
              item
                Name = 'compression'
                DataType = 'Integer'
              end
              item
                Name = 'nameInZip'
                DataType = 'String'
              end>
            OnEval = dwsZipClassesTZipWriterMethodsAddDataEval
            Kind = mkProcedure
          end
          item
            Name = 'AddFromZip'
            Parameters = <
              item
                Name = 'reader'
                DataType = 'TZipReader'
              end
              item
                Name = 'index'
                DataType = 'Integer'
              end>
            OnEval = dwsZipClassesTZipWriterMethodsAddFromZipEval
            Kind = mkProcedure
          end
          item
            Name = 'Close'
            Kind = mkDestructor
          end>
        OnCleanUp = dwsZipClassesTZipWriterCleanUp
      end>
    UnitName = 'System.Zip'
    StaticSymbols = True
    Left = 72
    Top = 32
  end
end
