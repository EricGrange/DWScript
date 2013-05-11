object DwsIdeDemoForm: TDwsIdeDemoForm
  Left = 0
  Top = 0
  Caption = 'DwsIdeDemoForm'
  ClientHeight = 192
  ClientWidth = 281
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
  object Button1: TButton
    Left = 40
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Open IDE'
    TabOrder = 0
    OnClick = Button1Click
  end
  object DelphiWebScript1: TDelphiWebScript
    Config.CompilerOptions = [coOptimize, coSymbolDictionary, coContextMap, coAssertions]
    Config.ScriptPaths.Strings = (
      'c:\scratch')
    Config.OnNeedUnit = DelphiWebScript1NeedUnit
    Left = 168
    Top = 32
  end
  object DemoUnit: TdwsUnit
    Script = DelphiWebScript1
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
    Left = 168
    Top = 96
  end
end
