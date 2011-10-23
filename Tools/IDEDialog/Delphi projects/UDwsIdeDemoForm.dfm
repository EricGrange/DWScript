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
    OnNeedUnit = DelphiWebScript1NeedUnit
    Left = 168
    Top = 32
  end
end
