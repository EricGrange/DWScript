object dwsClassesLib: TdwsClassesLib
  OldCreateOrder = False
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
            Kind = mkDestructor
            OnEval = dwsUnitClassesTListMethodsDestroyEval
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
            Kind = mkFunction
            OnEval = dwsUnitClassesTListMethodsAddEval
          end
          item
            Name = 'Clear'
            Kind = mkProcedure
            OnEval = dwsUnitClassesTListMethodsClearEval
          end
          item
            Name = 'Count'
            ResultType = 'Integer'
            Kind = mkFunction
            OnEval = dwsUnitClassesTListMethodsCountEval
          end
          item
            Name = 'Delete'
            Parameters = <
              item
                Name = 'Index'
                DataType = 'Integer'
                IsWritable = False
              end>
            Kind = mkProcedure
            OnEval = dwsUnitClassesTListMethodsDeleteEval
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
            Kind = mkFunction
            OnEval = dwsUnitClassesTListMethodsGetItemsEval
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
            Kind = mkFunction
            OnEval = dwsUnitClassesTListMethodsIndexOfEval
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
            Kind = mkProcedure
            OnEval = dwsUnitClassesTListMethodsInsertEval
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
            Kind = mkFunction
            OnEval = dwsUnitClassesTListMethodsRemoveEval
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
            Kind = mkProcedure
            OnEval = dwsUnitClassesTListMethodsSetItemsEval
          end
          item
            Name = 'Contains'
            Parameters = <
              item
                Name = 'obj'
                DataType = 'TObject'
              end>
            ResultType = 'Boolean'
            Kind = mkFunction
            OnEval = dwsUnitClassesTListMethodsContainsEval
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
            Kind = mkDestructor
            OnEval = dwsUnitClassesTStringsMethodsDestroyEval
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
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringsMethodsAddEval
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
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringsMethodsAddObjectEval
          end
          item
            Name = 'AddStrings'
            Parameters = <
              item
                Name = 'Strings'
                DataType = 'TStringList'
                IsWritable = False
              end>
            Kind = mkProcedure
            OnEval = dwsUnitClassesTStringsMethodsAddStringsEval
          end
          item
            Name = 'Clear'
            Kind = mkProcedure
            OnEval = dwsUnitClassesTStringsMethodsClearEval
          end
          item
            Name = 'Delete'
            Parameters = <
              item
                Name = 'Index'
                DataType = 'Integer'
                IsWritable = False
              end>
            Kind = mkProcedure
            OnEval = dwsUnitClassesTStringsMethodsDeleteEval
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
            Kind = mkProcedure
            OnEval = dwsUnitClassesTStringsMethodsExchangeEval
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
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringsMethodsGetEval
          end
          item
            Name = 'GetCommaText'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringsMethodsGetCommaTextEval
          end
          item
            Name = 'GetCount'
            ResultType = 'Integer'
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringsMethodsGetCountEval
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
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringsMethodsGetNamesEval
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
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringsMethodsGetObjectsEval
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
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringsMethodsGetStringsEval
          end
          item
            Name = 'GetText'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringsMethodsGetTextEval
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
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringsMethodsGetValuesEval
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
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringsMethodsGetValueFromIndexEval
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
            Kind = mkProcedure
            OnEval = dwsUnitClassesTStringsMethodsInsertObjectEval
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
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringsMethodsIndexOfEval
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
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringsMethodsIndexOfNameEval
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
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringsMethodsIndexOfObjectEval
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
            Kind = mkProcedure
            OnEval = dwsUnitClassesTStringsMethodsInsertEval
          end
          item
            Name = 'LoadFromFile'
            Parameters = <
              item
                Name = 'FileName'
                DataType = 'String'
                IsWritable = False
              end>
            Kind = mkProcedure
            OnEval = dwsUnitClassesTStringsMethodsLoadFromFileEval
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
            Kind = mkProcedure
            OnEval = dwsUnitClassesTStringsMethodsMoveEval
          end
          item
            Name = 'SaveToFile'
            Parameters = <
              item
                Name = 'FileName'
                DataType = 'String'
                IsWritable = False
              end>
            Kind = mkProcedure
            OnEval = dwsUnitClassesTStringsMethodsSaveToFileEval
          end
          item
            Name = 'SetCommaText'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
                IsWritable = False
              end>
            Kind = mkProcedure
            OnEval = dwsUnitClassesTStringsMethodsSetCommaTextEval
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
            Kind = mkProcedure
            OnEval = dwsUnitClassesTStringsMethodsSetObjectsEval
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
            Kind = mkProcedure
            OnEval = dwsUnitClassesTStringsMethodsSetStringsEval
          end
          item
            Name = 'SetText'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
                IsWritable = False
              end>
            Kind = mkProcedure
            OnEval = dwsUnitClassesTStringsMethodsSetTextEval
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
            Kind = mkProcedure
            OnEval = dwsUnitClassesTStringsMethodsSetValuesEval
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
            Kind = mkProcedure
            OnEval = dwsUnitClassesTStringsMethodsSetValueFromIndexEval
          end
          item
            Name = 'Remove'
            Parameters = <
              item
                Name = 'str'
                DataType = 'String'
              end>
            Kind = mkProcedure
            OnEval = dwsUnitClassesTStringsMethodsRemoveEval
          end
          item
            Name = 'Contains'
            Parameters = <
              item
                Name = 'str'
                DataType = 'String'
              end>
            ResultType = 'Boolean'
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringsMethodsContainsEval
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
            Kind = mkProcedure
            OnEval = dwsUnitClassesTStringListMethodsSortEval
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
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringListMethodsFindEval
          end
          item
            Name = 'GetDuplicates'
            ResultType = 'Integer'
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringListMethodsGetDuplicatesEval
          end
          item
            Name = 'SetDuplicates'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Integer'
                IsWritable = False
              end>
            Kind = mkProcedure
            OnEval = dwsUnitClassesTStringListMethodsSetDuplicatesEval
          end
          item
            Name = 'GetSorted'
            ResultType = 'Boolean'
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringListMethodsGetSortedEval
          end
          item
            Name = 'SetSorted'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Boolean'
                IsWritable = False
              end>
            Kind = mkProcedure
            OnEval = dwsUnitClassesTStringListMethodsSetSortedEval
          end
          item
            Name = 'GetCaseSensitive'
            ResultType = 'Boolean'
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringListMethodsGetCaseSensitiveEval
          end
          item
            Name = 'SetCaseSensitive'
            Parameters = <
              item
                Name = 'value'
                DataType = 'Boolean'
              end>
            Kind = mkProcedure
            OnEval = dwsUnitClassesTStringListMethodsSetCaseSensitiveEval
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
            Kind = mkFunction
            OnEval = dwsUnitClassesTHashtableMethodsSizeEval
          end
          item
            Name = 'Capacity'
            ResultType = 'Integer'
            Kind = mkFunction
            OnEval = dwsUnitClassesTHashtableMethodsCapacityEval
          end
          item
            Name = 'Clear'
            Kind = mkProcedure
            OnEval = dwsUnitClassesTHashtableMethodsClearEval
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
            Kind = mkDestructor
            OnEval = dwsUnitClassesTIntegerHashtableMethodsDestroyEval
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
            Kind = mkProcedure
            OnEval = dwsUnitClassesTIntegerHashtableMethodsPutEval
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
            Kind = mkFunction
            OnEval = dwsUnitClassesTIntegerHashtableMethodsGetEval
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
            Kind = mkFunction
            OnEval = dwsUnitClassesTIntegerHashtableMethodsHasKeyEval
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
            Kind = mkFunction
            OnEval = dwsUnitClassesTIntegerHashtableMethodsRemoveKeyEval
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
            Kind = mkDestructor
            OnEval = dwsUnitClassesTStringHashtableMethodsDestroyEval
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
            Kind = mkProcedure
            OnEval = dwsUnitClassesTStringHashtableMethodsPutEval
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
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringHashtableMethodsGetEval
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
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringHashtableMethodsHasKeyEval
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
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringHashtableMethodsRemoveKeyEval
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
            Kind = mkDestructor
            OnEval = dwsUnitClassesTStackMethodsDestroyEval
          end
          item
            Name = 'Push'
            Parameters = <
              item
                Name = 'Obj'
                DataType = 'TObject'
                IsWritable = False
              end>
            Kind = mkProcedure
            OnEval = dwsUnitClassesTStackMethodsPushEval
          end
          item
            Name = 'Pop'
            ResultType = 'TObject'
            Kind = mkFunction
            OnEval = dwsUnitClassesTStackMethodsPopEval
          end
          item
            Name = 'Peek'
            ResultType = 'TObject'
            Kind = mkFunction
            OnEval = dwsUnitClassesTStackMethodsPeekEval
          end
          item
            Name = 'Count'
            ResultType = 'Integer'
            Kind = mkFunction
            OnEval = dwsUnitClassesTStackMethodsCountEval
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
            Kind = mkDestructor
            OnEval = dwsUnitClassesTQueueMethodsDestroyEval
          end
          item
            Name = 'Push'
            Parameters = <
              item
                Name = 'Obj'
                DataType = 'TObject'
                IsWritable = False
              end>
            Kind = mkProcedure
            OnEval = dwsUnitClassesTQueueMethodsPushEval
          end
          item
            Name = 'Pop'
            ResultType = 'TObject'
            Kind = mkFunction
            OnEval = dwsUnitClassesTQueueMethodsPopEval
          end
          item
            Name = 'Peek'
            ResultType = 'TObject'
            Kind = mkFunction
            OnEval = dwsUnitClassesTQueueMethodsPeekEval
          end
          item
            Name = 'Count'
            ResultType = 'Integer'
            Kind = mkFunction
            OnEval = dwsUnitClassesTQueueMethodsCountEval
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
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringBuilderMethodsAppendEval
          end
          item
            Name = 'Length'
            ResultType = 'Integer'
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringBuilderMethodsLengthEval
          end
          item
            Name = 'Clear'
            Kind = mkProcedure
            OnEval = dwsUnitClassesTStringBuilderMethodsClearEval
          end
          item
            Name = 'ToString'
            ResultType = 'String'
            Kind = mkFunction
            OnEval = dwsUnitClassesTStringBuilderMethodsToStringEval
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
