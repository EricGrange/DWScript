object MainForm: TMainForm
  Left = 562
  Top = 140
  Caption = 'CustomFuncs'
  ClientHeight = 377
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 14
  object MECode: TMemo
    Left = 8
    Top = 8
    Width = 321
    Height = 161
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'SayHello('#39'World'#39');'
      ''
      'PrintLn( GetComputerName );'
      ''
      'var i : Integer;'
      ''
      'i:=2;'
      'MultiplyByTwo(i);'
      ''
      'PrintLn( IntToStr(i) );'
      '')
    ParentFont = False
    TabOrder = 0
  end
  object BURun: TButton
    Left = 128
    Top = 175
    Width = 75
    Height = 25
    Caption = 'Run'
    TabOrder = 1
    OnClick = BURunClick
  end
  object MEResult: TMemo
    Left = 8
    Top = 206
    Width = 321
    Height = 161
    TabOrder = 2
  end
  object DelphiWebScript: TDelphiWebScript
    Left = 160
    Top = 56
  end
  object dwsUnit: TdwsUnit
    Script = DelphiWebScript
    Functions = <
      item
        Name = 'SayHello'
        Parameters = <
          item
            Name = 'toWho'
            DataType = 'String'
          end>
        OnEval = dwsUnitFunctionsSayHelloEval
      end
      item
        Name = 'GetComputerName'
        ResultType = 'String'
        OnEval = dwsUnitFunctionsGetComputerNameEval
      end
      item
        Name = 'MultiplyByTwo'
        Parameters = <
          item
            Name = 'value'
            DataType = 'Integer'
            IsVarParam = True
          end>
        OnEval = dwsUnitFunctionsMultiplyByTwoEval
      end>
    UnitName = 'Test'
    StaticSymbols = False
    Left = 256
    Top = 56
  end
end
