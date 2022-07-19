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
            Kind = mkProcedure
            OnFastEvalNoResult = dwsDatabaseClassesDataBaseMethodsBeginTransactionFastEvalNoResult
          end
          item
            Name = 'StartTransaction'
            Kind = mkProcedure
            OnFastEvalNoResult = dwsDatabaseClassesDataBaseMethodsBeginTransactionFastEvalNoResult
          end
          item
            Name = 'Commit'
            Kind = mkProcedure
            OnFastEvalNoResult = dwsDatabaseClassesDataBaseMethodsCommitFastEvalNoResult
          end
          item
            Name = 'Rollback'
            Kind = mkProcedure
            OnFastEvalNoResult = dwsDatabaseClassesDataBaseMethodsRollbackFastEvalNoResult
          end
          item
            Name = 'InTransaction'
            ResultType = 'Boolean'
            Kind = mkFunction
            OnFastEvalBoolean = dwsDatabaseClassesDataBaseMethodsInTransactionFastEvalBoolean
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
            Kind = mkProcedure
            OnFastEvalNoResult = dwsDatabaseClassesDataBaseMethodsExecFastEvalNoResult
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
            Kind = mkFunction
            OnFastEvalScriptObj = dwsDatabaseClassesDataBaseMethodsQueryFastEvalScriptObj
          end
          item
            Name = 'VersionInfoText'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsDatabaseClassesDataBaseMethodsVersionInfoTextEval
          end
          item
            Name = 'SetLowerCaseStringify'
            Parameters = <
              item
                Name = 'value'
                DataType = 'Boolean'
              end>
            Kind = mkProcedure
            OnEval = dwsDatabaseClassesDataBaseMethodsLowerCaseStringifyEval
          end
          item
            Name = 'GetOption'
            Parameters = <
              item
                Name = 'name'
                DataType = 'String'
              end>
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsDatabaseClassesDataBaseMethodsGetOptionEval
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
            Kind = mkProcedure
            OnEval = dwsDatabaseClassesDataBaseMethodsSetOptionEval
          end
          item
            Name = 'OptionList'
            ResultType = 'array of String'
            Kind = mkFunction
            OnEval = dwsDatabaseClassesDataBaseMethodsOptionListEval
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
            Kind = mkFunction
            OnFastEvalBoolean = dwsDatabaseClassesDataSetMethodsStepFastEvalBoolean
          end
          item
            Name = 'Eof'
            ResultType = 'Boolean'
            Kind = mkFunction
            OnFastEvalBoolean = dwsDatabaseClassesDataSetMethodsEofFastEvalBoolean
          end
          item
            Name = 'Next'
            Kind = mkProcedure
            OnFastEvalNoResult = dwsDatabaseClassesDataSetMethodsNextFastEvalNoResult
          end
          item
            Name = 'Close'
            Kind = mkDestructor
          end
          item
            Name = 'FieldCount'
            ResultType = 'Integer'
            Kind = mkFunction
            OnFastEvalInteger = dwsDatabaseClassesDataSetMethodsFieldCountFastEvalInteger
          end
          item
            Name = 'GetField'
            Parameters = <
              item
                Name = 'index'
                DataType = 'Integer'
              end>
            ResultType = 'DataField'
            Visibility = cvProtected
            Kind = mkFunction
            OnEval = dwsDatabaseClassesDataSetMethodsGetFieldEval
          end
          item
            Name = 'IndexOfField'
            Parameters = <
              item
                Name = 'fieldName'
                DataType = 'String'
              end>
            ResultType = 'Integer'
            Kind = mkFunction
            OnFastEvalInteger = dwsDatabaseClassesDataSetMethodsIndexOfFieldFastEvalInteger
          end
          item
            Name = 'FieldByName'
            Parameters = <
              item
                Name = 'fieldName'
                DataType = 'String'
              end>
            ResultType = 'DataField'
            Kind = mkFunction
            OnEval = dwsDatabaseClassesDataSetMethodsFieldByNameEval
          end
          item
            Name = 'FindField'
            Parameters = <
              item
                Name = 'fieldName'
                DataType = 'String'
              end>
            ResultType = 'DataField'
            Kind = mkFunction
            OnEval = dwsDatabaseClassesDataSetMethodsFindFieldEval
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
            Kind = mkFunction
            OnFastEvalString = dwsDatabaseClassesDataSetMethodsAsString_String_FastEvalString
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
            Kind = mkFunction
            OnFastEvalString = dwsDatabaseClassesDataSetMethodsAsString_Integer_FastEvalString
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
            Kind = mkFunction
            OnFastEvalInteger = dwsDatabaseClassesDataSetMethodsAsInteger_String_FastEvalInteger
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
            Kind = mkFunction
            OnFastEvalInteger = dwsDatabaseClassesDataSetMethodsAsInteger_Integer_FastEvalInteger
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
            Kind = mkFunction
            OnFastEvalFloat = dwsDatabaseClassesDataSetMethodsAsFloat_String_FastEvalFloat
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
            Kind = mkFunction
            OnFastEvalFloat = dwsDatabaseClassesDataSetMethodsAsFloat_Integer_FastEvalFloat
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
            Kind = mkFunction
            OnFastEvalString = dwsDatabaseClassesDataSetMethodsAsBlob_String_FastEvalString
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
            Kind = mkFunction
            OnFastEvalString = dwsDatabaseClassesDataSetMethodsAsBlob_Integer_FastEvalString
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
            Kind = mkFunction
            OnFastEvalBoolean = dwsDatabaseClassesDataSetMethodsIsNull_String_FastEvalBoolean
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
            Kind = mkFunction
            OnFastEvalBoolean = dwsDatabaseClassesDataSetMethodsIsNull_Integer_FastEvalBoolean
          end
          item
            Name = 'Stringify'
            ResultType = 'String'
            Kind = mkFunction
            OnFastEvalString = dwsDatabaseClassesDataSetMethodsStringifyFastEvalString
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
            Kind = mkFunction
            OnFastEvalString = dwsDatabaseClassesDataSetMethodsStringifyAllFastEvalString
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
            Kind = mkFunction
            OnFastEvalString = dwsDatabaseClassesDataSetMethodsStringifyMapFastEvalString
          end
          item
            Name = 'ToSeparated'
            Parameters = <
              item
                Name = 'maxRows'
                DataType = 'Integer'
                HasDefaultValue = True
                DefaultValue = 0
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
            OnFastEvalString = dwsDatabaseClassesDataSetMethodsToSeparatedFastEvalString
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
            Kind = mkFunction
            OnFastEvalString = dwsDatabaseClassesDataFieldMethodsNameFastEvalString
          end
          item
            Name = 'DataType'
            ResultType = 'DataFieldType'
            Kind = mkFunction
            OnFastEvalInteger = dwsDatabaseClassesDataFieldMethodsDataTypeFastEvalInteger
          end
          item
            Name = 'DeclaredType'
            ResultType = 'String'
            Kind = mkFunction
            OnFastEvalString = dwsDatabaseClassesDataFieldMethodsDeclaredTypeFastEvalString
          end
          item
            Name = 'IsNull'
            ResultType = 'Boolean'
            Kind = mkFunction
            OnFastEvalBoolean = dwsDatabaseClassesDataFieldMethodsIsNullFastEvalBoolean
          end
          item
            Name = 'AsString'
            ResultType = 'String'
            Kind = mkFunction
            OnFastEvalString = dwsDatabaseClassesDataFieldMethodsAsStringFastEvalString
          end
          item
            Name = 'AsInteger'
            ResultType = 'Integer'
            Kind = mkFunction
            OnFastEvalInteger = dwsDatabaseClassesDataFieldMethodsAsIntegerFastEvalInteger
          end
          item
            Name = 'AsFloat'
            ResultType = 'Float'
            Kind = mkFunction
            OnFastEvalFloat = dwsDatabaseClassesDataFieldMethodsAsFloatFastEvalFloat
          end
          item
            Name = 'AsBoolean'
            ResultType = 'Boolean'
            Kind = mkFunction
            OnFastEvalBoolean = dwsDatabaseClassesDataFieldMethodsAsBooleanFastEvalBoolean
          end
          item
            Name = 'AsBlob'
            ResultType = 'String'
            Kind = mkFunction
            OnFastEvalString = dwsDatabaseClassesDataFieldMethodsAsBlobFastEvalString
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
            Kind = mkClassFunction
            OnFastEval = dwsDatabaseClassesDataBasePoolMethodsAcquireFastEval
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
            Kind = mkClassProcedure
            OnEval = dwsDatabaseClassesDataBasePoolMethodsReleaseEval
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
            Kind = mkClassProcedure
            OnFastEvalNoResult = dwsDatabaseClassesDataBasePoolMethodsCleanupFastEvalNoResult
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
            Kind = mkClassFunction
            OnFastEvalInteger = dwsDatabaseClassesDataBasePoolMethodsCountFastEvalInteger
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
