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
            Kind = mkFunction
            OnEval = dwsZipClassesTZipReaderMethodsCountEval
          end
          item
            Name = 'GetName'
            Parameters = <
              item
                Name = 'index'
                DataType = 'Integer'
              end>
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsZipClassesTZipReaderMethodsGetNameEval
          end
          item
            Name = 'GetData'
            Parameters = <
              item
                Name = 'index'
                DataType = 'Integer'
              end>
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsZipClassesTZipReaderMethodsGetDataEval
          end
          item
            Name = 'GetFullSize'
            Parameters = <
              item
                Name = 'index'
                DataType = 'Integer'
              end>
            ResultType = 'Integer'
            Kind = mkFunction
            OnEval = dwsZipClassesTZipReaderMethodsGetFullSizeEval
          end
          item
            Name = 'GetZipSize'
            Parameters = <
              item
                Name = 'index'
                DataType = 'Integer'
              end>
            ResultType = 'Integer'
            Kind = mkFunction
            OnEval = dwsZipClassesTZipReaderMethodsGetZipSizeEval
          end
          item
            Name = 'IndexOf'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
            ResultType = 'Integer'
            Kind = mkFunction
            OnEval = dwsZipClassesTZipReaderMethodsIndexOfEval
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
            Kind = mkFunction
            OnEval = dwsZipClassesTZipReaderMethodsUnzipEval
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
          end
          item
            Name = 'CreateInMemory'
            OnEval = dwsZipClassesTZipWriterConstructorsCreateInMemoryEval
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
            Kind = mkProcedure
            OnEval = dwsZipClassesTZipWriterMethodsAddFileEval
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
            Kind = mkProcedure
            OnEval = dwsZipClassesTZipWriterMethodsAddDataEval
          end
          item
            Name = 'AddDeflatedData'
            Parameters = <
              item
                Name = 'deflated'
                DataType = 'TDeflateCompressor'
              end
              item
                Name = 'nameInZip'
                DataType = 'String'
              end>
            Kind = mkProcedure
            OnEval = dwsZipClassesTZipWriterMethodsAddDeflatedDataEval
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
            Kind = mkProcedure
            OnEval = dwsZipClassesTZipWriterMethodsAddFromZipEval
          end
          item
            Name = 'Close'
            Kind = mkDestructor
          end
          item
            Name = 'CloseInMemory'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsZipClassesTZipWriterMethodsCloseInMemoryEval
          end>
        OnCleanUp = dwsZipClassesTZipWriterCleanUp
      end
      item
        Name = 'TDeflateCompressor'
        Constructors = <
          item
            Name = 'Create'
            Parameters = <
              item
                Name = 'compression'
                DataType = 'Integer'
                HasDefaultValue = True
                DefaultValue = 7
              end>
            OnEval = dwsZipClassesTDeflateCompressorConstructorsCreateEval
          end>
        Methods = <
          item
            Name = 'WriteData'
            Parameters = <
              item
                Name = 'data'
                DataType = 'String'
              end>
            Kind = mkProcedure
            OnFastEval = dwsZipClassesTDeflateCompressorMethodsWriteDataFastEval
          end
          item
            Name = 'Flush'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsZipClassesTDeflateCompressorMethodsFlushEval
          end
          item
            Name = 'CRC32'
            ResultType = 'Integer'
            Kind = mkFunction
            OnEval = dwsZipClassesTDeflateCompressorMethodsCRC32Eval
          end
          item
            Name = 'SizeIn'
            ResultType = 'Integer'
            Kind = mkFunction
            OnEval = dwsZipClassesTDeflateCompressorMethodsSizeInEval
          end
          item
            Name = 'SizeOut'
            ResultType = 'Integer'
            Kind = mkFunction
            OnEval = dwsZipClassesTDeflateCompressorMethodsSizeOutEval
          end>
        OnCleanUp = dwsZipClassesTDeflateCompressorCleanUp
      end>
    UnitName = 'System.Zip'
    StaticSymbols = True
    Left = 72
    Top = 32
  end
end
