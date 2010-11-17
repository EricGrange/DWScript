object asmExtensionTest: TasmExtensionTest
  Left = 392
  Top = 136
  Caption = 'asmExtensionTest'
  ClientHeight = 346
  ClientWidth = 549
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object MECode: TMemo
    Left = 8
    Top = 9
    Width = 452
    Height = 329
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'const cOne = 1.0;'
      'function RSqrt(x : Float) : Float;'
      'begin'
      '   asm'
      '      fld x;'
      '      fsqrt;'
      '      fld cOne;'
      '      fdivr;'
      '      fstp Result;'
      '   end;'
      'end;'
      ''
      'PrintLn(RSqrt(1/4));'
      '')
    ParentFont = False
    TabOrder = 0
  end
  object Button1: TButton
    Left = 466
    Top = 7
    Width = 75
    Height = 25
    Caption = 'Run'
    TabOrder = 1
    OnClick = Button1Click
  end
  object DelphiWebScript: TDelphiWebScript
    Left = 224
    Top = 56
  end
end
