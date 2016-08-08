object dwsDatabaseLib: TdwsDatabaseLib
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 646
  Top = 86
  Height = 150
  Width = 215
  object dwsDatabase: TdwsUnit
    Arrays = <
      item
        Name = 'VariantDynArray'
        DataType = 'Variant'
        IsDynamic = True
      end
      item
        Name = 'DataFieldsDynArray'
        DataType = 'DataField'
        IsDynamic = True
      end>
    Classes = <
      item
        Name = 'DataBase'
        Constructors = <
          item
            Name = 'Create'
            Parameters = <
              item
                Name = 'driver'
                DataType = 'String'
              end
              item
                Name = 'parameters'
                DataType = 'array of string'
                IsWritable = False
                HasDefaultValue = True
                DefaultValue = Null
              end>
            OnEval = dwsDatabaseClassesDataBaseConstructorsCreateEval
          end>
        Methods = <
          item
            Name = 'Close'
            Kind = mkDestructor
          end
          item
            Name = 'BeginTransaction'
            OnEval = dwsDatabaseClassesDataBaseMethodsBeginTransactionEval
            Kind = mkProcedure
          end
          item
            Name = 'StartTransaction'
            OnEval = dwsDatabaseClassesDataBaseMethodsBeginTransactionEval
            Kind = mkProcedure
          end
          item
            Name = 'Commit'
            OnEval = dwsDatabaseClassesDataBaseMethodsCommitEval
            Kind = mkProcedure
          end
          item
            Name = 'Rollback'
            OnEval = dwsDatabaseClassesDataBaseMethodsRollbackEval
            Kind = mkProcedure
          end
          item
            Name = 'InTransaction'
            ResultType = 'Boolean'
            OnEval = dwsDatabaseClassesDataBaseMethodsInTransactionEval
            Kind = mkFunction
          end
          item
            Name = 'Exec'
            Parameters = <
              item
                Name = 'sql'
                DataType = 'String'
              end
              item
                Name = 'parameters'
                DataType = 'VariantDynArray'
                IsWritable = False
                HasDefaultValue = True
                DefaultValue = Null
              end>
            OnEval = dwsDatabaseClassesDataBaseMethodsExecEval
            Kind = mkProcedure
          end
          item
            Name = 'Query'
            Parameters = <
              item
                Name = 'query'
                DataType = 'String'
              end
              item
                Name = 'parameters'
                DataType = 'VariantDynArray'
                IsWritable = False
                HasDefaultValue = True
                DefaultValue = Null
              end>
            ResultType = 'DataSet'
            OnEval = dwsDatabaseClassesDataBaseMethodsQueryEval
            Kind = mkFunction
          end
          item
            Name = 'VersionInfoText'
            ResultType = 'String'
            OnEval = dwsDatabaseClassesDataBaseMethodsVersionInfoTextEval
            Kind = mkFunction
          end
          item
            Name = 'SetLowerCaseStringify'
            Parameters = <
              item
                Name = 'value'
                DataType = 'Boolean'
              end>
            OnEval = dwsDatabaseClassesDataBaseMethodsLowerCaseStringifyEval
            Kind = mkProcedure
          end>
        OnCleanUp = dwsDatabaseClassesDataBaseCleanUp
      end
      item
        Name = 'DataSet'
        Fields = <
          item
            Name = 'FFields'
            DataType = 'DataFieldsDynArray'
            Visibility = cvPrivate
          end>
        Methods = <
          item
            Name = 'Step'
            ResultType = 'Boolean'
            OnEval = dwsDatabaseClassesDataSetMethodsStepEval
            Kind = mkFunction
          end
          item
            Name = 'Eof'
            ResultType = 'Boolean'
            OnEval = dwsDatabaseClassesDataSetMethodsEofEval
            Kind = mkFunction
          end
          item
            Name = 'Next'
            OnEval = dwsDatabaseClassesDataSetMethodsNextEval
            Kind = mkProcedure
          end
          item
            Name = 'Close'
            Kind = mkDestructor
          end
          item
            Name = 'FieldCount'
            ResultType = 'Integer'
            OnEval = dwsDatabaseClassesDataSetMethodsFieldCountEval
            Kind = mkFunction
          end
          item
            Name = 'GetField'
            Parameters = <
              item
                Name = 'index'
                DataType = 'Integer'
              end>
            ResultType = 'DataField'
            OnEval = dwsDatabaseClassesDataSetMethodsGetFieldEval
            Visibility = cvProtected
            Kind = mkFunction
          end
          item
            Name = 'IndexOfField'
            Parameters = <
              item
                Name = 'fieldName'
                DataType = 'String'
              end>
            ResultType = 'Integer'
            OnEval = dwsDatabaseClassesDataSetMethodsIndexOfFieldEval
            Kind = mkFunction
          end
          item
            Name = 'FieldByName'
            Parameters = <
              item
                Name = 'fieldName'
                DataType = 'String'
              end>
            ResultType = 'DataField'
            OnEval = dwsDatabaseClassesDataSetMethodsFieldByNameEval
            Kind = mkFunction
          end
          item
            Name = 'FindField'
            Parameters = <
              item
                Name = 'fieldName'
                DataType = 'String'
              end>
            ResultType = 'DataField'
            OnEval = dwsDatabaseClassesDataSetMethodsFindFieldEval
            Kind = mkFunction
          end
          item
            Name = 'AsString'
            Parameters = <
              item
                Name = 'fieldName'
                DataType = 'String'
              end>
            ResultType = 'String'
            Overloaded = True
            OnEval = dwsDatabaseClassesDataSetMethodsAsStringByNameEval
            Kind = mkFunction
          end
          item
            Name = 'AsString'
            Parameters = <
              item
                Name = 'index'
                DataType = 'Integer'
              end>
            ResultType = 'String'
            Overloaded = True
            OnEval = dwsDatabaseClassesDataSetMethodsAsStringByIndexEval
            Kind = mkFunction
          end
          item
            Name = 'AsInteger'
            Parameters = <
              item
                Name = 'fieldName'
                DataType = 'String'
              end>
            ResultType = 'Integer'
            Overloaded = True
            OnEval = dwsDatabaseClassesDataSetMethodsAsIntegerByNameEval
            Kind = mkFunction
          end
          item
            Name = 'AsInteger'
            Parameters = <
              item
                Name = 'index'
                DataType = 'Integer'
              end>
            ResultType = 'Integer'
            Overloaded = True
            OnEval = dwsDatabaseClassesDataSetMethodsAsIntegerByIndexEval
            Kind = mkFunction
          end
          item
            Name = 'AsFloat'
            Parameters = <
              item
                Name = 'fieldName'
                DataType = 'String'
              end>
            ResultType = 'Float'
            Overloaded = True
            OnEval = dwsDatabaseClassesDataSetMethodsAsFloatByNameEval
            Kind = mkFunction
          end
          item
            Name = 'AsFloat'
            Parameters = <
              item
                Name = 'index'
                DataType = 'Integer'
              end>
            ResultType = 'Float'
            Overloaded = True
            OnEval = dwsDatabaseClassesDataSetMethodsAsFloatByIndexEval
            Kind = mkFunction
          end
          item
            Name = 'AsBlob'
            Parameters = <
              item
                Name = 'fieldName'
                DataType = 'String'
              end>
            ResultType = 'String'
            Overloaded = True
            OnEval = dwsDatabaseClassesDataSetMethodsAsBlobByNameEval
            Kind = mkFunction
          end
          item
            Name = 'AsBlob'
            Parameters = <
              item
                Name = 'index'
                DataType = 'Integer'
              end>
            ResultType = 'String'
            Overloaded = True
            OnEval = dwsDatabaseClassesDataSetMethodsAsBlobByIndexEval
            Kind = mkFunction
          end
          item
            Name = 'IsNull'
            Parameters = <
              item
                Name = 'fieldName'
                DataType = 'String'
              end>
            ResultType = 'Boolean'
            Overloaded = True
            OnEval = dwsDatabaseClassesDataSetMethodsIsNullByNameEval
            Kind = mkFunction
          end
          item
            Name = 'IsNull'
            Parameters = <
              item
                Name = 'index'
                DataType = 'Integer'
              end>
            ResultType = 'Boolean'
            Overloaded = True
            OnEval = dwsDatabaseClassesDataSetMethodsIsNullByIndexEval
            Kind = mkFunction
          end
          item
            Name = 'Stringify'
            ResultType = 'String'
            OnEval = dwsDatabaseClassesDataSetMethodsStringifyEval
            Kind = mkFunction
          end
          item
            Name = 'StringifyAll'
            Parameters = <
              item
                Name = 'maxRows'
                DataType = 'Integer'
                HasDefaultValue = True
                DefaultValue = 0
              end>
            ResultType = 'String'
            OnEval = dwsDatabaseClassesDataSetMethodsStringifyAllEval
            Kind = mkFunction
          end
          item
            Name = 'StringifyMap'
            Parameters = <
              item
                Name = 'maxRows'
                DataType = 'Integer'
                HasDefaultValue = True
                DefaultValue = 0
              end>
            ResultType = 'String'
            OnEval = dwsDatabaseClassesDataSetMethodsStringifyMapEval
            Kind = mkFunction
          end>
        Properties = <
          item
            Name = 'Fields'
            DataType = 'DataField'
            ReadAccess = 'GetField'
            Parameters = <
              item
                Name = 'index'
                DataType = 'Integer'
              end>
          end>
        OnCleanUp = dwsDatabaseClassesDataBaseCleanUp
      end
      item
        Name = 'DataField'
        Methods = <
          item
            Name = 'Name'
            ResultType = 'String'
            OnEval = dwsDatabaseClassesDataFieldMethodsNameEval
            Kind = mkFunction
          end
          item
            Name = 'DataType'
            ResultType = 'DataFieldType'
            OnEval = dwsDatabaseClassesDataFieldMethodsDataTypeEval
            Kind = mkFunction
          end
          item
            Name = 'DeclaredType'
            ResultType = 'String'
            OnEval = dwsDatabaseClassesDataFieldMethodsDeclaredTypeEval
            Kind = mkFunction
          end
          item
            Name = 'IsNull'
            ResultType = 'Boolean'
            OnEval = dwsDatabaseClassesDataFieldMethodsIsNullEval
            Kind = mkFunction
          end
          item
            Name = 'AsString'
            ResultType = 'String'
            OnEval = dwsDatabaseClassesDataFieldMethodsAsStringEval
            Kind = mkFunction
          end
          item
            Name = 'AsInteger'
            ResultType = 'Integer'
            OnEval = dwsDatabaseClassesDataFieldMethodsAsIntegerEval
            Kind = mkFunction
          end
          item
            Name = 'AsFloat'
            ResultType = 'Float'
            OnEval = dwsDatabaseClassesDataFieldMethodsAsFloatEval
            Kind = mkFunction
          end
          item
            Name = 'AsBoolean'
            ResultType = 'Boolean'
            OnEval = dwsDatabaseClassesDataFieldMethodsAsBooleanEval
            Kind = mkFunction
          end
          item
            Name = 'AsBlob'
            ResultType = 'String'
            OnEval = dwsDatabaseClassesDataFieldMethodsAsBlobEval
            Kind = mkFunction
          end>
        OnCleanUp = dwsDatabaseClassesDataBaseCleanUp
      end
      item
        Name = 'EDBException'
        Ancestor = 'Exception'
      end
      item
        Name = 'DataBasePool'
        IsStatic = True
        Methods = <
          item
            Name = 'Acquire'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
            ResultType = 'DataBase'
            Attributes = [maStatic]
            OnEval = dwsDatabaseClassesDataBasePoolMethodsAcquireEval
            Kind = mkClassFunction
          end
          item
            Name = 'Release'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end
              item
                Name = 'db'
                DataType = 'DataBase'
                IsVarParam = True
              end
              item
                Name = 'poolSize'
                DataType = 'Integer'
                HasDefaultValue = True
                DefaultValue = 3
              end>
            Attributes = [maStatic]
            OnEval = dwsDatabaseClassesDataBasePoolMethodsReleaseEval
            Kind = mkClassProcedure
          end
          item
            Name = 'Cleanup'
            Parameters = <
              item
                Name = 'filter'
                DataType = 'String'
                HasDefaultValue = True
                DefaultValue = '*'
              end>
            Attributes = [maStatic]
            OnEval = dwsDatabaseClassesDataBasePoolMethodsCleanupEval
            Kind = mkClassProcedure
          end
          item
            Name = 'Count'
            Parameters = <
              item
                Name = 'filter'
                DataType = 'String'
                HasDefaultValue = True
                DefaultValue = '*'
              end>
            ResultType = 'Integer'
            Attributes = [maStatic]
            OnEval = dwsDatabaseClassesDataBasePoolMethodsCountEval
            Kind = mkClassFunction
          end>
      end>
    Enumerations = <
      item
        Name = 'DataFieldType'
        Elements = <
          item
            Name = 'Unknown'
          end
          item
            Name = 'Null'
          end
          item
            Name = 'Integer'
          end
          item
            Name = 'Float'
          end
          item
            Name = 'String'
          end
          item
            Name = 'Boolean'
          end
          item
            Name = 'DateTime'
          end
          item
            Name = 'Blob'
          end>
        Style = enumScoped
      end>
    Functions = <
      item
        Name = 'BlobParameter'
        Parameters = <
          item
            Name = 'data'
            DataType = 'String'
          end>
        ResultType = 'Variant'
        OnFastEval = dwsDatabaseFunctionsBlobParameterFastEval
      end
      item
        Name = 'BlobHexParameter'
        Parameters = <
          item
            Name = 'hexData'
            DataType = 'String'
          end>
        ResultType = 'Variant'
        OnFastEval = dwsDatabaseFunctionsBlobHexParameterFastEval
      end
      item
        Name = 'DateParameter'
        Parameters = <
          item
            Name = 'dt'
            DataType = 'Float'
          end>
        ResultType = 'Variant'
        OnFastEval = dwsDatabaseFunctionsDateParameterFastEval
      end>
    UnitName = 'System.Data'
    StaticSymbols = True
    Left = 72
    Top = 16
  end
end
