object dwsClassesLib: TdwsClassesLib
  OldCreateOrder = False
  Left = 464
  Top = 260
  Height = 135
  Width = 258
  object dwsUnit: TdwsUnit
    Classes = <
      item
        Name = 'TList'
        Constructors = <
          item
            Name = 'Create'
            OnEval = dwsUnitClassesTListConstructorsCreateEval
          end>
        Methods = <
          item
            Name = 'Destroy'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsUnitClassesTListMethodsDestroyEval
            Kind = mkDestructor
          end
          item
            Name = 'Add'
            Parameters = <
              item
                Name = 'Obj'
                DataType = 'TObject'
                IsWritable = False
              end>
            ResultType = 'Integer'
            OnEval = dwsUnitClassesTListMethodsAddEval
            Kind = mkFunction
          end
          item
            Name = 'Clear'
            OnEval = dwsUnitClassesTListMethodsClearEval
            Kind = mkProcedure
          end
          item
            Name = 'Count'
            ResultType = 'Integer'
            OnEval = dwsUnitClassesTListMethodsCountEval
            Kind = mkFunction
          end
          item
            Name = 'Delete'
            Parameters = <
              item
                Name = 'Index'
                DataType = 'Integer'
                IsWritable = False
              end>
            OnEval = dwsUnitClassesTListMethodsDeleteEval
            Kind = mkProcedure
          end
          item
            Name = 'GetItems'
            Parameters = <
              item
                Name = 'Index'
                DataType = 'Integer'
                IsWritable = False
              end>
            ResultType = 'TObject'
            OnEval = dwsUnitClassesTListMethodsGetItemsEval
            Kind = mkFunction
          end
          item
            Name = 'IndexOf'
            Parameters = <
              item
                Name = 'Obj'
                DataType = 'TObject'
                IsWritable = False
              end>
            ResultType = 'Integer'
            OnEval = dwsUnitClassesTListMethodsIndexOfEval
            Kind = mkFunction
          end
          item
            Name = 'Insert'
            Parameters = <
              item
                Name = 'Index'
                DataType = 'Integer'
                IsWritable = False
              end
              item
                Name = 'Obj'
                DataType = 'TObject'
                IsWritable = False
              end>
            OnEval = dwsUnitClassesTListMethodsInsertEval
            Kind = mkProcedure
          end
          item
            Name = 'Remove'
            Parameters = <
              item
                Name = 'Obj'
                DataType = 'TObject'
                IsWritable = False
              end>
            ResultType = 'Integer'
            OnEval = dwsUnitClassesTListMethodsRemoveEval
            Kind = mkFunction
          end
          item
            Name = 'SetItems'
            Parameters = <
              item
                Name = 'Index'
                DataType = 'Integer'
                IsWritable = False
              end
              item
                Name = 'Value'
                DataType = 'TObject'
                IsWritable = False
              end>
            OnEval = dwsUnitClassesTListMethodsSetItemsEval
            Kind = mkProcedure
          end
          item
            Name = 'Contains'
            Parameters = <
              item
                Name = 'obj'
                DataType = 'TObject'
              end>
            ResultType = 'Boolean'
            OnEval = dwsUnitClassesTListMethodsContainsEval
            Kind = mkFunction
          end>
        Operators = <
          item
            DataType = 'TObject'
            Operator = ttPLUS_ASSIGN
            UsesAccess = 'Add'
          end
          item
            DataType = 'TObject'
            Operator = ttMINUS_ASSIGN
            UsesAccess = 'Remove'
          end
          item
            DataType = 'TObject'
            Operator = ttIN
            UsesAccess = 'Contains'
          end>
        Properties = <
          item
            Name = 'Items'
            DataType = 'TObject'
            ReadAccess = 'GetItems'
            WriteAccess = 'SetItems'
            Parameters = <
              item
                Name = 'x'
                DataType = 'Integer'
                IsWritable = False
              end>
            IsDefault = True
          end>
      end
      item
        Name = 'TStrings'
        Constructors = <
          item
            Name = 'Create'
            OnEval = dwsUnitClassesTStringsConstructorsCreateEval
          end>
        Methods = <
          item
            Name = 'Destroy'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsUnitClassesTStringsMethodsDestroyEval
            Kind = mkDestructor
          end
          item
            Name = 'Add'
            Parameters = <
              item
                Name = 'Str'
                DataType = 'String'
                IsWritable = False
              end>
            ResultType = 'Integer'
            OnEval = dwsUnitClassesTStringsMethodsAddEval
            Kind = mkFunction
          end
          item
            Name = 'AddObject'
            Parameters = <
              item
                Name = 'S'
                DataType = 'String'
                IsWritable = False
              end
              item
                Name = 'AObject'
                DataType = 'TObject'
                IsWritable = False
              end>
            ResultType = 'Integer'
            OnEval = dwsUnitClassesTStringsMethodsAddObjectEval
            Kind = mkFunction
          end
          item
            Name = 'AddStrings'
            Parameters = <
              item
                Name = 'Strings'
                DataType = 'TStringList'
                IsWritable = False
              end>
            OnEval = dwsUnitClassesTStringsMethodsAddStringsEval
            Kind = mkProcedure
          end
          item
            Name = 'Clear'
            OnEval = dwsUnitClassesTStringsMethodsClearEval
            Kind = mkProcedure
          end
          item
            Name = 'Delete'
            Parameters = <
              item
                Name = 'Index'
                DataType = 'Integer'
                IsWritable = False
              end>
            OnEval = dwsUnitClassesTStringsMethodsDeleteEval
            Kind = mkProcedure
          end
          item
            Name = 'Exchange'
            Parameters = <
              item
                Name = 'Index1'
                DataType = 'Integer'
                IsWritable = False
              end
              item
                Name = 'Index2'
                DataType = 'Integer'
                IsWritable = False
              end>
            OnEval = dwsUnitClassesTStringsMethodsExchangeEval
            Kind = mkProcedure
          end
          item
            Name = 'Get'
            Parameters = <
              item
                Name = 'Index'
                DataType = 'Integer'
                IsWritable = False
              end>
            ResultType = 'String'
            OnEval = dwsUnitClassesTStringsMethodsGetEval
            Kind = mkFunction
          end
          item
            Name = 'GetCommaText'
            ResultType = 'String'
            OnEval = dwsUnitClassesTStringsMethodsGetCommaTextEval
            Kind = mkFunction
          end
          item
            Name = 'GetCount'
            ResultType = 'Integer'
            OnEval = dwsUnitClassesTStringsMethodsGetCountEval
            Kind = mkFunction
          end
          item
            Name = 'GetNames'
            Parameters = <
              item
                Name = 'Index'
                DataType = 'Integer'
                IsWritable = False
              end>
            ResultType = 'String'
            OnEval = dwsUnitClassesTStringsMethodsGetNamesEval
            Kind = mkFunction
          end
          item
            Name = 'GetObjects'
            Parameters = <
              item
                Name = 'Index'
                DataType = 'Integer'
                IsWritable = False
              end>
            ResultType = 'TObject'
            OnEval = dwsUnitClassesTStringsMethodsGetObjectsEval
            Kind = mkFunction
          end
          item
            Name = 'GetStrings'
            Parameters = <
              item
                Name = 'Index'
                DataType = 'Integer'
                IsWritable = False
              end>
            ResultType = 'String'
            OnEval = dwsUnitClassesTStringsMethodsGetStringsEval
            Kind = mkFunction
          end
          item
            Name = 'GetText'
            ResultType = 'String'
            OnEval = dwsUnitClassesTStringsMethodsGetTextEval
            Kind = mkFunction
          end
          item
            Name = 'GetValues'
            Parameters = <
              item
                Name = 'Str'
                DataType = 'String'
                IsWritable = False
              end>
            ResultType = 'String'
            OnEval = dwsUnitClassesTStringsMethodsGetValuesEval
            Kind = mkFunction
          end
          item
            Name = 'GetValueFromIndex'
            Parameters = <
              item
                Name = 'Index'
                DataType = 'Integer'
                IsWritable = False
              end>
            ResultType = 'String'
            OnEval = dwsUnitClassesTStringsMethodsGetValueFromIndexEval
            Kind = mkFunction
          end
          item
            Name = 'InsertObject'
            Parameters = <
              item
                Name = 'Index'
                DataType = 'Integer'
                IsWritable = False
              end
              item
                Name = 'S'
                DataType = 'String'
                IsWritable = False
              end
              item
                Name = 'AObject'
                DataType = 'TObject'
                IsWritable = False
              end>
            OnEval = dwsUnitClassesTStringsMethodsInsertObjectEval
            Kind = mkProcedure
          end
          item
            Name = 'IndexOf'
            Parameters = <
              item
                Name = 'Str'
                DataType = 'String'
                IsWritable = False
              end>
            ResultType = 'Integer'
            OnEval = dwsUnitClassesTStringsMethodsIndexOfEval
            Kind = mkFunction
          end
          item
            Name = 'IndexOfName'
            Parameters = <
              item
                Name = 'Str'
                DataType = 'String'
                IsWritable = False
              end>
            ResultType = 'Integer'
            OnEval = dwsUnitClassesTStringsMethodsIndexOfNameEval
            Kind = mkFunction
          end
          item
            Name = 'IndexOfObject'
            Parameters = <
              item
                Name = 'AObject'
                DataType = 'TObject'
                IsWritable = False
              end>
            ResultType = 'Integer'
            OnEval = dwsUnitClassesTStringsMethodsIndexOfObjectEval
            Kind = mkFunction
          end
          item
            Name = 'Insert'
            Parameters = <
              item
                Name = 'Index'
                DataType = 'Integer'
                IsWritable = False
              end
              item
                Name = 'Str'
                DataType = 'String'
                IsWritable = False
              end>
            OnEval = dwsUnitClassesTStringsMethodsInsertEval
            Kind = mkProcedure
          end
          item
            Name = 'LoadFromFile'
            Parameters = <
              item
                Name = 'FileName'
                DataType = 'String'
                IsWritable = False
              end>
            OnEval = dwsUnitClassesTStringsMethodsLoadFromFileEval
            Kind = mkProcedure
          end
          item
            Name = 'Move'
            Parameters = <
              item
                Name = 'CurIndex'
                DataType = 'Integer'
                IsWritable = False
              end
              item
                Name = 'NewIndex'
                DataType = 'Integer'
                IsWritable = False
              end>
            OnEval = dwsUnitClassesTStringsMethodsMoveEval
            Kind = mkProcedure
          end
          item
            Name = 'SaveToFile'
            Parameters = <
              item
                Name = 'FileName'
                DataType = 'String'
                IsWritable = False
              end>
            OnEval = dwsUnitClassesTStringsMethodsSaveToFileEval
            Kind = mkProcedure
          end
          item
            Name = 'SetCommaText'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
                IsWritable = False
              end>
            OnEval = dwsUnitClassesTStringsMethodsSetCommaTextEval
            Kind = mkProcedure
          end
          item
            Name = 'SetObjects'
            Parameters = <
              item
                Name = 'Index'
                DataType = 'Integer'
                IsWritable = False
              end
              item
                Name = 'Value'
                DataType = 'TObject'
                IsWritable = False
              end>
            OnEval = dwsUnitClassesTStringsMethodsSetObjectsEval
            Kind = mkProcedure
          end
          item
            Name = 'SetStrings'
            Parameters = <
              item
                Name = 'Index'
                DataType = 'Integer'
                IsWritable = False
              end
              item
                Name = 'Value'
                DataType = 'String'
                IsWritable = False
              end>
            OnEval = dwsUnitClassesTStringsMethodsSetStringsEval
            Kind = mkProcedure
          end
          item
            Name = 'SetText'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
                IsWritable = False
              end>
            OnEval = dwsUnitClassesTStringsMethodsSetTextEval
            Kind = mkProcedure
          end
          item
            Name = 'SetValues'
            Parameters = <
              item
                Name = 'Str'
                DataType = 'String'
                IsWritable = False
              end
              item
                Name = 'Value'
                DataType = 'String'
                IsWritable = False
              end>
            OnEval = dwsUnitClassesTStringsMethodsSetValuesEval
            Kind = mkProcedure
          end
          item
            Name = 'SetValueFromIndex'
            Parameters = <
              item
                Name = 'Index'
                DataType = 'Integer'
                IsWritable = False
              end
              item
                Name = 'Value'
                DataType = 'String'
                IsWritable = False
              end>
            OnEval = dwsUnitClassesTStringsMethodsSetValueFromIndexEval
            Kind = mkProcedure
          end
          item
            Name = 'Remove'
            Parameters = <
              item
                Name = 'str'
                DataType = 'String'
              end>
            OnEval = dwsUnitClassesTStringsMethodsRemoveEval
            Kind = mkProcedure
          end
          item
            Name = 'Contains'
            Parameters = <
              item
                Name = 'str'
                DataType = 'String'
              end>
            ResultType = 'Boolean'
            OnEval = dwsUnitClassesTStringsMethodsContainsEval
            Kind = mkFunction
          end>
        Operators = <
          item
            DataType = 'String'
            Operator = ttPLUS_ASSIGN
            UsesAccess = 'Add'
          end
          item
            DataType = 'String'
            Operator = ttMINUS_ASSIGN
            UsesAccess = 'Remove'
          end
          item
            DataType = 'String'
            Operator = ttIN
            UsesAccess = 'Contains'
          end>
        Properties = <
          item
            Name = 'Text'
            DataType = 'String'
            ReadAccess = 'GetText'
            WriteAccess = 'SetText'
          end
          item
            Name = 'Count'
            DataType = 'Integer'
            ReadAccess = 'GetCount'
          end
          item
            Name = 'CommaText'
            DataType = 'String'
            ReadAccess = 'GetCommaText'
            WriteAccess = 'SetCommaText'
          end
          item
            Name = 'Strings'
            DataType = 'String'
            ReadAccess = 'GetStrings'
            WriteAccess = 'SetStrings'
            Parameters = <
              item
                Name = 'x'
                DataType = 'Integer'
                IsWritable = False
              end>
            IsDefault = True
          end
          item
            Name = 'Objects'
            DataType = 'TObject'
            ReadAccess = 'GetObjects'
            WriteAccess = 'SetObjects'
            Parameters = <
              item
                Name = 'x'
                DataType = 'Integer'
                IsWritable = False
              end>
          end
          item
            Name = 'Names'
            DataType = 'String'
            ReadAccess = 'GetNames'
            Parameters = <
              item
                Name = 'x'
                DataType = 'Integer'
                IsWritable = False
              end>
          end
          item
            Name = 'Values'
            DataType = 'String'
            ReadAccess = 'GetValues'
            WriteAccess = 'SetValues'
            Parameters = <
              item
                Name = 's'
                DataType = 'String'
                IsWritable = False
              end>
          end
          item
            Name = 'ValueFromIndex'
            DataType = 'String'
            ReadAccess = 'GetValueFromIndex'
            WriteAccess = 'SetValueFromIndex'
            Parameters = <
              item
                Name = 'x'
                DataType = 'Integer'
                IsWritable = False
              end>
          end>
      end
      item
        Name = 'TStringList'
        Ancestor = 'TStrings'
        Methods = <
          item
            Name = 'Sort'
            OnEval = dwsUnitClassesTStringListMethodsSortEval
            Kind = mkProcedure
          end
          item
            Name = 'Find'
            Parameters = <
              item
                Name = 'S'
                DataType = 'String'
                IsWritable = False
              end
              item
                Name = 'Index'
                DataType = 'Integer'
                IsVarParam = True
              end>
            ResultType = 'Boolean'
            OnEval = dwsUnitClassesTStringListMethodsFindEval
            Kind = mkFunction
          end
          item
            Name = 'GetDuplicates'
            ResultType = 'Integer'
            OnEval = dwsUnitClassesTStringListMethodsGetDuplicatesEval
            Kind = mkFunction
          end
          item
            Name = 'SetDuplicates'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Integer'
                IsWritable = False
              end>
            OnEval = dwsUnitClassesTStringListMethodsSetDuplicatesEval
            Kind = mkProcedure
          end
          item
            Name = 'GetSorted'
            ResultType = 'Boolean'
            OnEval = dwsUnitClassesTStringListMethodsGetSortedEval
            Kind = mkFunction
          end
          item
            Name = 'SetSorted'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
                IsWritable = False
              end>
            OnEval = dwsUnitClassesTStringListMethodsSetSortedEval
            Kind = mkProcedure
          end
          item
            Name = 'GetCaseSensitive'
            ResultType = 'Boolean'
            OnEval = dwsUnitClassesTStringListMethodsGetCaseSensitiveEval
            Kind = mkFunction
          end
          item
            Name = 'SetCaseSensitive'
            Parameters = <
              item
                Name = 'value'
                DataType = 'Boolean'
              end>
            OnEval = dwsUnitClassesTStringListMethodsSetCaseSensitiveEval
            Kind = mkProcedure
          end>
        Properties = <
          item
            Name = 'Duplicates'
            DataType = 'Integer'
            ReadAccess = 'GetDuplicates'
            WriteAccess = 'SetDuplicates'
          end
          item
            Name = 'Sorted'
            DataType = 'Boolean'
            ReadAccess = 'GetSorted'
            WriteAccess = 'SetSorted'
          end
          item
            Name = 'CaseSensitive'
            DataType = 'Boolean'
            ReadAccess = 'GetCaseSensitive'
            WriteAccess = 'SetCaseSensitive'
          end>
      end
      item
        Name = 'THashtable'
        Methods = <
          item
            Name = 'Size'
            ResultType = 'Integer'
            OnEval = dwsUnitClassesTHashtableMethodsSizeEval
            Kind = mkFunction
          end
          item
            Name = 'Capacity'
            ResultType = 'Integer'
            OnEval = dwsUnitClassesTHashtableMethodsCapacityEval
            Kind = mkFunction
          end
          item
            Name = 'Clear'
            OnEval = dwsUnitClassesTHashtableMethodsClearEval
            Kind = mkProcedure
          end>
      end
      item
        Name = 'TIntegerHashtable'
        Ancestor = 'THashtable'
        Constructors = <
          item
            Name = 'Create'
            OnEval = dwsUnitClassesTIntegerHashtableConstructorsCreateEval
          end>
        Methods = <
          item
            Name = 'Destroy'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsUnitClassesTIntegerHashtableMethodsDestroyEval
            Kind = mkDestructor
          end
          item
            Name = 'Put'
            Parameters = <
              item
                Name = 'Key'
                DataType = 'Integer'
                IsWritable = False
              end
              item
                Name = 'Value'
                DataType = 'TObject'
                IsWritable = False
              end>
            OnEval = dwsUnitClassesTIntegerHashtableMethodsPutEval
            Kind = mkProcedure
          end
          item
            Name = 'Get'
            Parameters = <
              item
                Name = 'Key'
                DataType = 'Integer'
                IsWritable = False
              end>
            ResultType = 'TObject'
            OnEval = dwsUnitClassesTIntegerHashtableMethodsGetEval
            Kind = mkFunction
          end
          item
            Name = 'HasKey'
            Parameters = <
              item
                Name = 'Key'
                DataType = 'Integer'
                IsWritable = False
              end>
            ResultType = 'Boolean'
            OnEval = dwsUnitClassesTIntegerHashtableMethodsHasKeyEval
            Kind = mkFunction
          end
          item
            Name = 'RemoveKey'
            Parameters = <
              item
                Name = 'Key'
                DataType = 'Integer'
                IsWritable = False
              end>
            ResultType = 'TObject'
            OnEval = dwsUnitClassesTIntegerHashtableMethodsRemoveKeyEval
            Kind = mkFunction
          end>
        Operators = <
          item
            DataType = 'Integer'
            Operator = ttIN
            UsesAccess = 'HasKey'
          end>
      end
      item
        Name = 'TStringHashtable'
        Ancestor = 'THashtable'
        Constructors = <
          item
            Name = 'Create'
            OnEval = dwsUnitClassesTStringHashtableConstructorsCreateEval
          end>
        Methods = <
          item
            Name = 'Destroy'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsUnitClassesTStringHashtableMethodsDestroyEval
            Kind = mkDestructor
          end
          item
            Name = 'Put'
            Parameters = <
              item
                Name = 'Key'
                DataType = 'String'
                IsWritable = False
              end
              item
                Name = 'Value'
                DataType = 'TObject'
                IsWritable = False
              end>
            OnEval = dwsUnitClassesTStringHashtableMethodsPutEval
            Kind = mkProcedure
          end
          item
            Name = 'Get'
            Parameters = <
              item
                Name = 'Key'
                DataType = 'String'
                IsWritable = False
              end>
            ResultType = 'TObject'
            OnEval = dwsUnitClassesTStringHashtableMethodsGetEval
            Kind = mkFunction
          end
          item
            Name = 'HasKey'
            Parameters = <
              item
                Name = 'Key'
                DataType = 'String'
                IsWritable = False
              end>
            ResultType = 'Boolean'
            OnEval = dwsUnitClassesTStringHashtableMethodsHasKeyEval
            Kind = mkFunction
          end
          item
            Name = 'RemoveKey'
            Parameters = <
              item
                Name = 'Key'
                DataType = 'String'
                IsWritable = False
              end>
            ResultType = 'TObject'
            OnEval = dwsUnitClassesTStringHashtableMethodsRemoveKeyEval
            Kind = mkFunction
          end>
        Operators = <
          item
            DataType = 'String'
            Operator = ttIN
            UsesAccess = 'HasKey'
          end>
      end
      item
        Name = 'TStack'
        Constructors = <
          item
            Name = 'Create'
            OnEval = dwsUnitClassesTStackConstructorsCreateEval
          end>
        Methods = <
          item
            Name = 'Destroy'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsUnitClassesTStackMethodsDestroyEval
            Kind = mkDestructor
          end
          item
            Name = 'Push'
            Parameters = <
              item
                Name = 'Obj'
                DataType = 'TObject'
                IsWritable = False
              end>
            OnEval = dwsUnitClassesTStackMethodsPushEval
            Kind = mkProcedure
          end
          item
            Name = 'Pop'
            ResultType = 'TObject'
            OnEval = dwsUnitClassesTStackMethodsPopEval
            Kind = mkFunction
          end
          item
            Name = 'Peek'
            ResultType = 'TObject'
            OnEval = dwsUnitClassesTStackMethodsPeekEval
            Kind = mkFunction
          end
          item
            Name = 'Count'
            ResultType = 'Integer'
            OnEval = dwsUnitClassesTStackMethodsCountEval
            Kind = mkFunction
          end>
      end
      item
        Name = 'TQueue'
        Constructors = <
          item
            Name = 'Create'
            OnEval = dwsUnitClassesTQueueConstructorsCreateEval
          end>
        Methods = <
          item
            Name = 'Destroy'
            Attributes = [maVirtual, maOverride]
            OnEval = dwsUnitClassesTQueueMethodsDestroyEval
            Kind = mkDestructor
          end
          item
            Name = 'Push'
            Parameters = <
              item
                Name = 'Obj'
                DataType = 'TObject'
                IsWritable = False
              end>
            OnEval = dwsUnitClassesTQueueMethodsPushEval
            Kind = mkProcedure
          end
          item
            Name = 'Pop'
            ResultType = 'TObject'
            OnEval = dwsUnitClassesTQueueMethodsPopEval
            Kind = mkFunction
          end
          item
            Name = 'Peek'
            ResultType = 'TObject'
            OnEval = dwsUnitClassesTQueueMethodsPeekEval
            Kind = mkFunction
          end
          item
            Name = 'Count'
            ResultType = 'Integer'
            OnEval = dwsUnitClassesTQueueMethodsCountEval
            Kind = mkFunction
          end>
      end
      item
        Name = 'TStringBuilder'
        Constructors = <
          item
            Name = 'Create'
            OnEval = dwsUnitClassesTStringBuilderConstructorsCreateEval
          end>
        Methods = <
          item
            Name = 'Append'
            Parameters = <
              item
                Name = 'value'
                DataType = 'Variant'
              end>
            ResultType = 'TStringBuilder'
            OnEval = dwsUnitClassesTStringBuilderMethodsAppendEval
            Kind = mkFunction
          end
          item
            Name = 'Length'
            ResultType = 'Integer'
            OnEval = dwsUnitClassesTStringBuilderMethodsLengthEval
            Kind = mkFunction
          end
          item
            Name = 'Clear'
            OnEval = dwsUnitClassesTStringBuilderMethodsClearEval
            Kind = mkProcedure
          end
          item
            Name = 'ToString'
            ResultType = 'String'
            OnEval = dwsUnitClassesTStringBuilderMethodsToStringEval
            Kind = mkFunction
          end>
        Operators = <
          item
            DataType = 'Variant'
            Operator = ttPLUS_ASSIGN
            UsesAccess = 'Append'
          end>
        OnCleanUp = dwsUnitClassesTStringBuilderCleanUp
      end>
    Constants = <
      item
        Name = 'dupIgnore'
        DataType = 'Integer'
        Value = 0
      end
      item
        Name = 'dupError'
        DataType = 'Integer'
        Value = 1
      end
      item
        Name = 'dupAccept'
        DataType = 'Integer'
        Value = 2
      end>
    UnitName = 'Classes'
    StaticSymbols = False
    Left = 44
    Top = 24
  end
end
