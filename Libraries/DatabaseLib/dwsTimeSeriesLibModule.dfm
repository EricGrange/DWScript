object dwsTimeSeriesLib: TdwsTimeSeriesLib
  OldCreateOrder = False
  Height = 150
  Width = 215
  object dwsTimeSeries: TdwsUnit
    Classes = <
      item
        Name = 'TimeSeries'
        Constructors = <
          item
            Name = 'Create'
            OnEval = dwsTimeSeriesClassesTimeSeriesConstructorsCreateEval
          end
          item
            Name = 'Connect'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
            OnEval = dwsTimeSeriesClassesTimeSeriesConstructorsConnectEval
          end>
        Methods = <
          item
            Name = 'Disconnect'
            Kind = mkDestructor
            OnEval = dwsTimeSeriesClassesTimeSeriesMethodsDisconnectEval
          end
          item
            Name = 'AddSequence'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end
              item
                Name = 'decimals'
                DataType = 'Integer'
              end>
            ResultType = 'Integer'
            Kind = mkFunction
            OnEval = dwsTimeSeriesClassesTimeSeriesMethodsAddSequenceEval
          end
          item
            Name = 'SequenceCount'
            ResultType = 'Integer'
            Kind = mkFunction
            OnEval = dwsTimeSeriesClassesTimeSeriesMethodsSequenceCountEval
          end
          item
            Name = 'GetSequenceName'
            Parameters = <
              item
                Name = 'index'
                DataType = 'Integer'
              end>
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsTimeSeriesClassesTimeSeriesMethodsGetSequenceNameEval
          end
          item
            Name = 'StoreSample'
            Parameters = <
              item
                Name = 'sequenceName'
                DataType = 'String'
              end
              item
                Name = 'timestamp'
                DataType = 'Integer'
              end
              item
                Name = 'value'
                DataType = 'Float'
              end>
            Kind = mkProcedure
            OnEval = dwsTimeSeriesClassesTimeSeriesMethodsStoreSampleEval
          end
          item
            Name = 'StoreSamples'
            Parameters = <
              item
                Name = 'sequenceName'
                DataType = 'String'
              end
              item
                Name = 'timestamps'
                DataType = 'array of Integer'
              end
              item
                Name = 'values'
                DataType = 'array of Float'
              end>
            Kind = mkProcedure
            OnEval = dwsTimeSeriesClassesTimeSeriesMethodsStoreSamplesEval
          end
          item
            Name = 'Optimize'
            Kind = mkProcedure
            OnEval = dwsTimeSeriesClassesTimeSeriesMethodsOptimizeEval
          end
          item
            Name = 'ExtractSamples'
            Parameters = <
              item
                Name = 'sequenceName'
                DataType = 'String'
              end
              item
                Name = 'fromTimestamp'
                DataType = 'Integer'
              end
              item
                Name = 'toTimestamp'
                DataType = 'Integer'
              end
              item
                Name = 'timestamps'
                DataType = 'array of Integer'
              end
              item
                Name = 'values'
                DataType = 'array of Float'
              end
              item
                Name = 'options'
                DataType = 'TimeSeriesExtractionOptions'
              end>
            ResultType = 'Integer'
            Kind = mkFunction
            OnEval = dwsTimeSeriesClassesTimeSeriesMethodsExtractSamplesEval
          end
          item
            Name = 'ClearSamples'
            Kind = mkProcedure
            OnEval = dwsTimeSeriesClassesTimeSeriesMethodsClearSamplesEval
          end>
        Properties = <
          item
            Name = 'SequenceNames'
            DataType = 'String'
            ReadAccess = 'GetSequenceName'
            Parameters = <
              item
                Name = 'index'
                DataType = 'String'
              end>
          end>
        OnCleanUp = dwsTimeSeriesClassesTimeSeriesCleanUp
      end>
    Enumerations = <
      item
        Name = 'TimeSeriesExtractionOption'
        Elements = <
          item
            Name = 'tseoIgnoreNulls'
            UserDefValue = 1
            IsUserDef = True
          end>
      end>
    Sets = <
      item
        Name = 'TimeSeriesExtractionOptions'
        BaseType = 'TimeSeriesExtractionOption'
      end>
    UnitName = 'System.Data.TimeSeries'
    StaticSymbols = True
    Left = 80
    Top = 24
  end
end
