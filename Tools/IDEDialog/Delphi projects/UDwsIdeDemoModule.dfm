object DwsIdeDemoModule: TDwsIdeDemoModule
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 150
  Width = 215
  object DelphiWebScript: TDelphiWebScript
    Config.CompilerOptions = [coOptimize, coSymbolDictionary, coContextMap, coAssertions]
    Config.ScriptPaths.Strings = (
      'c:\scratch')
    Config.OnNeedUnit = DelphiWebScriptNeedUnit
    Left = 32
    Top = 48
  end
  object DemoUnit: TdwsUnit
    Script = DelphiWebScript
    Classes = <
      item
        Name = 'TDemoUnitObj'
        Constructors = <
          item
            Name = 'Create'
            OnEval = DemoUnitClassesTDemoUnitObjConstructorsCreateEval
          end>
        Methods = <
          item
            Name = 'GetOne'
            ResultType = 'integer'
            OnEval = DemoUnitClassesTDemoUnitObjMethodsGetOneEval
            Kind = mkFunction
          end
          item
            Name = 'GetSubObj1'
            ResultType = 'TSubObj1'
            OnEval = DemoUnitClassesTDemoUnitObjMethodsGetSubObj1Eval
            Kind = mkFunction
          end>
        OnCleanUp = DemoUnitClassesTDemoUnitObjCleanUp
      end
      item
        Name = 'TSubObj1'
        Constructors = <
          item
            Name = 'Create'
            OnEval = DemoUnitClassesTSubObj1ConstructorsCreateEval
          end>
        Methods = <
          item
            Name = 'GetOne'
            ResultType = 'integer'
            OnEval = DemoUnitClassesTSubObj1MethodsGetOneEval
            Kind = mkFunction
          end>
        OnCleanUp = DemoUnitClassesTSubObj1CleanUp
      end>
    Functions = <
      item
        Name = 'DemoUnitRec'
        ResultType = 'TDemoUnitRec'
        OnEval = dwsUnit1FunctionsMyUnitRecEval
      end>
    Instances = <
      item
        Name = 'DemoUnitObj'
        DataType = 'TDemoUnitObj'
        OnInstantiate = DemoUnitInstancesDemoUnitObjInstantiate
      end>
    Records = <
      item
        Name = 'TDemoUnitRec'
        Members = <
          item
            Name = 'One'
            DataType = 'integer'
          end
          item
            Name = 'Two'
            DataType = 'integer'
          end>
        Properties = <>
      end>
    UnitName = 'uDemoUnit'
    StaticSymbols = False
    Left = 104
    Top = 48
  end
end
