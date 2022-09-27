object dwsTabularLib: TdwsTabularLib
  OldCreateOrder = False
  Height = 150
  Width = 215
  object dwsTabular: TdwsUnit
    Classes = <
      item
        Name = 'TabularData'
        Constructors = <
          item
            Name = 'CreateFromDataSet'
            Parameters = <
              item
                Name = 'ds'
                DataType = 'DataSet'
              end
              item
                Name = 'options'
                DataType = 'array of String'
                HasDefaultValue = True
                DefaultValue = Null
              end>
            OnEval = dwsTabularClassesTabularDataConstructorsCreateFromDataSetEval
          end
          item
            Name = 'Create'
            Parameters = <
              item
                Name = 'options'
                DataType = 'array of String'
                HasDefaultValue = True
                DefaultValue = Null
              end>
            OnEval = dwsTabularClassesTabularDataConstructorsCreateEval
          end
          item
            Name = 'ConnectToShared'
            Parameters = <
              item
                Name = 'sharedName'
                DataType = 'String'
              end
              item
                Name = 'options'
                DataType = 'array of String'
                HasDefaultValue = True
                DefaultValue = Null
              end>
            OnEval = dwsTabularClassesTabularDataConstructorsConnectToSharedEval
          end>
        Methods = <
          item
            Name = 'EvaluateAggregate'
            Parameters = <
              item
                Name = 'aggregateFunc'
                DataType = 'String'
              end
              item
                Name = 'opcodes'
                DataType = 'array of Variant'
              end
              item
                Name = 'fromIndex'
                DataType = 'Integer'
                HasDefaultValue = True
                DefaultValue = 0
              end
              item
                Name = 'toIndex'
                DataType = 'Integer'
                HasDefaultValue = True
                DefaultValue = -1
              end>
            ResultType = 'Float'
            Kind = mkFunction
            OnEval = dwsTabularClassesTabularDataMethodsEvaluateAggregateEval
          end
          item
            Name = 'EvaluateNewColumn'
            Parameters = <
              item
                Name = 'columnName'
                DataType = 'String'
              end
              item
                Name = 'opcodes'
                DataType = 'array of Variant'
              end>
            ResultType = 'Float'
            Kind = mkFunction
            OnEval = dwsTabularClassesTabularDataMethodsEvaluateNewColumnEval
          end
          item
            Name = 'Evaluate'
            Parameters = <
              item
                Name = 'opcodes'
                DataType = 'array of Variant'
              end>
            ResultType = 'array of Float'
            Kind = mkFunction
            OnEval = dwsTabularClassesTabularDataMethodsEvaluateEval
          end
          item
            Name = 'ExportToSeparated'
            Parameters = <
              item
                Name = 'columnNames'
                DataType = 'array of String'
              end
              item
                Name = 'separator'
                DataType = 'String'
                HasDefaultValue = True
                DefaultValue = ','
              end
              item
                Name = 'quoteChar'
                DataType = 'String'
                HasDefaultValue = True
                DefaultValue = '"'
              end>
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsTabularClassesTabularDataMethodsExportToSeparatedEval
          end
          item
            Name = 'DropColumn'
            Parameters = <
              item
                Name = 'columnName'
                DataType = 'String'
              end>
            ResultType = 'Boolean'
            Kind = mkFunction
            OnEval = dwsTabularClassesTabularDataMethodsDropColumnEval
          end
          item
            Name = 'AddColumn'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end
              item
                Name = 'values'
                DataType = 'array of Float'
              end>
            Kind = mkProcedure
            OnEval = dwsTabularClassesTabularDataMethodsAddColumnEval
          end
          item
            Name = 'LockAndShare'
            Parameters = <
              item
                Name = 'sharedName'
                DataType = 'String'
              end>
            Kind = mkProcedure
            OnEval = dwsTabularClassesTabularDataMethodsLockAndShareEval
          end
          item
            Name = 'ColumnNames'
            ResultType = 'array of String'
            Kind = mkFunction
            OnEval = dwsTabularClassesTabularDataMethodsColumnNamesEval
          end
          item
            Name = 'ColumnStrings'
            Parameters = <
              item
                Name = 'columnName'
                DataType = 'String'
              end>
            ResultType = 'array of String'
            Kind = mkFunction
            OnEval = dwsTabularClassesTabularDataMethodsColumnStringsEval
          end>
        OnCleanUp = dwsTabularClassesTabularDataCleanUp
      end>
    Dependencies.Strings = (
      'System.Data')
    UnitName = 'System.Data.Tabular'
    StaticSymbols = True
    Left = 80
    Top = 24
  end
end
