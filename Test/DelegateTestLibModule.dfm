object DelegateTestLib: TDelegateTestLib
  OldCreateOrder = False
  Left = 461
  Top = 182
  Height = 150
  Width = 215
  object dwsUnitDelegateTest: TdwsUnit
    Classes = <
      item
        Name = 'TFoo'
        Ancestor = 'TObject'
        Constructors = <
          item
            Name = 'Create'
            OnEval = dwsUnitDelegateTestClassesTFooConstructorsCreateEval
          end>
        Fields = <
          item
            Name = 'FOnTest'
            DataType = 'TNotifyEvent'
            Visibility = cvPrivate
          end>
        Methods = <
          item
            Name = 'SetOnTest'
            Parameters = <
              item
                Name = 'value'
                DataType = 'TNotifyEvent'
                IsVarParam = True
                IsWritable = False
              end>
            OnEval = dwsUnitDelegateTestClassesTFooMethodsSetOnTestEval
            Visibility = cvPrivate
            Kind = mkProcedure
          end
          item
            Name = 'Test'
            OnEval = dwsUnitDelegateTestClassesTFooMethodsTestEval
            Kind = mkProcedure
          end>
        Properties = <
          item
            Name = 'OnTest'
            DataType = 'TNotifyEvent'
            ReadAccess = 'FOnTest'
            WriteAccess = 'SetOnTest'
          end>
        OnCleanUp = dwsUnitDelegateTestClassesTFooCleanUp
      end>
    Functions = <
      item
        Name = 'CallMe'
        Parameters = <
          item
            Name = 'name'
            DataType = 'String'
          end
          item
            Name = 'value'
            DataType = 'Integer'
          end>
        ResultType = 'String'
        OnEval = dwsUnitDelegateTestFunctionsCallMeEval
      end
      item
        Name = 'CallFunc'
        Parameters = <
          item
            Name = 'func'
            DataType = 'TFunc'
          end
          item
            Name = 'value'
            DataType = 'Integer'
          end>
        ResultType = 'String'
        OnEval = dwsUnitDelegateTestFunctionsCallFuncEval
      end
      item
        Name = 'CallEvent'
        Parameters = <
          item
            Name = 'event'
            DataType = 'TNotifyEvent'
          end
          item
            Name = 'obj'
            DataType = 'TObject'
          end>
        OnEval = dwsUnitDelegateTestFunctionsCallEventEval
      end>
    Delegates = <
      item
        Name = 'TNotifyEvent'
        Parameters = <
          item
            Name = 'Sender'
            DataType = 'TObject'
          end>
      end
      item
        Name = 'TFunc'
        Parameters = <
          item
            Name = 'a'
            DataType = 'Integer'
          end>
        ResultType = 'String'
      end>
    UnitName = 'Test.Delegate'
    StaticSymbols = False
    Left = 80
    Top = 24
  end
end
