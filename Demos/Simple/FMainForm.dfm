object MainForm: TMainForm
  Left = 341
  Top = 204
  Caption = 'Simple'
  ClientHeight = 512
  ClientWidth = 640
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 14
  object Splitter1: TSplitter
    Left = 0
    Top = 297
    Width = 640
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 215
  end
  object MESourceCode: TMemo
    Left = 0
    Top = 0
    Width = 640
    Height = 297
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'var i : Integer;'
      ''
      'for i:=1 to 10 do'
      '   PrintLn(IntToStr(i));')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    WantTabs = True
    WordWrap = False
  end
  object MEResult: TMemo
    Left = 0
    Top = 300
    Width = 640
    Height = 212
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object MainMenu: TMainMenu
    Left = 232
    Top = 25
    object MILoad: TMenuItem
      Caption = 'Load...'
      OnClick = MILoadClick
    end
    object MISave: TMenuItem
      Caption = 'Save...'
      OnClick = MISaveClick
    end
    object MIRun: TMenuItem
      Caption = 'Run'
      OnClick = MIRunClick
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'Pascal files (*.pas)|*.pas'
    Left = 312
    Top = 24
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'pas'
    Filter = 'Pascal files (*.pas)|*.pas'
    Left = 392
    Top = 24
  end
  object DelphiWebScript: TDelphiWebScript
    Left = 232
    Top = 104
  end
  object dwsGUIFunctions: TdwsGUIFunctions
    Left = 360
    Top = 104
  end
end
