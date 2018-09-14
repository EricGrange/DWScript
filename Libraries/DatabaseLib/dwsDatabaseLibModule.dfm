object dwsDatabaseLib: TdwsDatabaseLib
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
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
            OnFastEvalNoResult = dwsDatabaseClassesDataBaseMethodsBeginTransactionFastEvalNoResult
            Kind = mkProcedure
          end
          item
            Name = 'StartTransaction'
            OnFastEvalNoResult = dwsDatabaseClassesDataBaseMethodsBeginTransactionFastEvalNoResult
            Kind = mkProcedure
          end
          item
            Name = 'Commit'
            OnFastEvalNoResult = dwsDatabaseClassesDataBaseMethodsCommitFastEvalNoResult
            Kind = mkProcedure
          end
          item
            Name = 'Rollback'
            OnFastEvalNoResult = dwsDatabaseClassesDataBaseMethodsRollbackFastEvalNoResult
            Kind = mkProcedure
          end
          item
            Name = 'InTransaction'
            ResultType = 'Boolean'
            OnFastEvalBolean = dwsDatabaseClassesDataBaseMethodsInTransactionFastEvalBolean
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
          end
          item
            Name = 'GetOption'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
            ResultType = 'String'
            OnEval = dwsDatabaseClassesDataBaseMethodsGetOptionEval
            Kind = mkFunction
          end
          item
            Name = 'SetOption'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end
              item
                Name = 'value'
                DataType = 'String'
              end>
            OnEval = dwsDatabaseClassesDataBaseMethodsSetOptionEval
            Kind = mkProcedure
          end
          item
            Name = 'OptionList'
            ResultType = 'array of String'
            OnEval = dwsDatabaseClassesDataBaseMethodsOptionListEval
            Kind = mkFunction
          end>
        Properties = <
          item
            Name = 'Options'
            DataType = 'String'
            ReadAccess = 'GetOption'
            WriteAccess = 'SetOption'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
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
            OnFastEvalBolean = dwsDatabaseClassesDataSetMethodsStepFastEvalBolean
            Kind = mkFunction
          end
          item
            Name = 'Eof'
            ResultType = 'Boolean'
            OnFastEvalBolean = dwsDatabaseClassesDataSetMethodsEofFastEvalBolean
            Kind = mkFunction
          end
          item
            Name = 'Next'
            OnFastEvalNoResult = dwsDatabaseClassesDataSetMethodsNextFastEvalNoResult
            Kind = mkProcedure
          end
          item
            Name = 'Close'
            Kind = mkDestructor
          end
          item
            Name = 'FieldCount'
            ResultType = 'Integer'
            OnFastEvalInteger = dwsDatabaseClassesDataSetMethodsFieldCountFastEvalInteger
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
            OnFastEvalInteger = dwsDatabaseClassesDataSetMethodsIndexOfFieldFastEvalInteger
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
            OnFastEvalString = dwsDatabaseClassesDataSetMethodsAsString_String_FastEvalString
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
            OnFastEvalString = dwsDatabaseClassesDataSetMethodsAsString_Integer_FastEvalString
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
            OnFastEvalInteger = dwsDatabaseClassesDataSetMethodsAsInteger_String_FastEvalInteger
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
            OnFastEvalInteger = dwsDatabaseClassesDataSetMethodsAsInteger_Integer_FastEvalInteger
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
            OnFastEvalFloat = dwsDatabaseClassesDataSetMethodsAsFloat_String_FastEvalFloat
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
            OnFastEvalFloat = dwsDatabaseClassesDataSetMethodsAsFloat_Integer_FastEvalFloat
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
            OnFastEvalString = dwsDatabaseClassesDataSetMethodsAsBlob_String_FastEvalString
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
            OnFastEvalString = dwsDatabaseClassesDataSetMethodsAsBlob_Integer_FastEvalString
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
            OnFastEvalBolean = dwsDatabaseClassesDataSetMethodsIsNull_String_FastEvalBolean
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
            OnFastEvalBolean = dwsDatabaseClassesDataSetMethodsIsNull_Integer_FastEvalBolean
            Kind = mkFunction
          end
          item
            Name = 'Stringify'
            ResultType = 'String'
            OnFastEvalString = dwsDatabaseClassesDataSetMethodsStringifyFastEvalString
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
            OnFastEvalString = dwsDatabaseClassesDataSetMethodsStringifyAllFastEvalString
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
            OnFastEvalString = dwsDatabaseClassesDataSetMethodsStringifyMapFastEvalString
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
            OnFastEvalString = dwsDatabaseClassesDataFieldMethodsNameFastEvalString
            Kind = mkFunction
          end
          item
            Name = 'DataType'
            ResultType = 'DataFieldType'
            OnFastEvalInteger = dwsDatabaseClassesDataFieldMethodsDataTypeFastEvalInteger
            Kind = mkFunction
          end
          item
            Name = 'DeclaredType'
            ResultType = 'String'
            OnFastEvalString = dwsDatabaseClassesDataFieldMethodsDeclaredTypeFastEvalString
            Kind = mkFunction
          end
          item
            Name = 'IsNull'
            ResultType = 'Boolean'
            OnFastEvalBolean = dwsDatabaseClassesDataFieldMethodsIsNullFastEvalBolean
            Kind = mkFunction
          end
          item
            Name = 'AsString'
            ResultType = 'String'
            OnFastEvalString = dwsDatabaseClassesDataFieldMethodsAsStringFastEvalString
            Kind = mkFunction
          end
          item
            Name = 'AsInteger'
            ResultType = 'Integer'
            OnFastEvalInteger = dwsDatabaseClassesDataFieldMethodsAsIntegerFastEvalInteger
            Kind = mkFunction
          end
          item
            Name = 'AsFloat'
            ResultType = 'Float'
            OnFastEvalFloat = dwsDatabaseClassesDataFieldMethodsAsFloatFastEvalFloat
            Kind = mkFunction
          end
          item
            Name = 'AsBoolean'
            ResultType = 'Boolean'
            OnFastEvalBolean = dwsDatabaseClassesDataFieldMethodsAsBooleanFastEvalBolean
            Kind = mkFunction
          end
          item
            Name = 'AsBlob'
            ResultType = 'String'
            OnFastEvalString = dwsDatabaseClassesDataFieldMethodsAsBlobFastEvalString
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
            OnFastEval = dwsDatabaseClassesDataBasePoolMethodsAcquireFastEval
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
            OnFastEvalNoResult = dwsDatabaseClassesDataBasePoolMethodsCleanupFastEvalNoResult
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
            OnFastEvalInteger = dwsDatabaseClassesDataBasePoolMethodsCountFastEvalInteger
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
        Name = 'BlobHexParameterDef'
        Parameters = <
          item
            Name = 'hexData'
            DataType = 'String'
          end
          item
            Name = 'default'
            DataType = 'String'
          end>
        ResultType = 'Variant'
        OnFastEval = dwsDatabaseFunctionsBlobHexParameterDefFastEval
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
