object DwsIdeDemoForm: TDwsIdeDemoForm
  Left = 0
  Top = 0
  Caption = 'DwsIdeDemoForm'
  ClientHeight = 127
  ClientWidth = 205
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonOpenIDE: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Open IDE'
    TabOrder = 0
    OnClick = ButtonOpenIDEClick
  end
  object DelphiWebScript: TDelphiWebScript
    Config.CompilerOptions = [coOptimize, coSymbolDictionary, coContextMap, coAssertions]
    Config.ScriptPaths.Strings = (
      'c:\scratch')
    Config.OnNeedUnit = DelphiWebScriptNeedUnit
    Left = 120
    Top = 16
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
    Left = 120
    Top = 72
  end
end
