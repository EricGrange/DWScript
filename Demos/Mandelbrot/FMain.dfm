object MainForm: TMainForm
  Left = 383
  Top = 136
  BorderStyle = bsDialog
  Caption = 'MainForm'
  ClientHeight = 289
  ClientWidth = 545
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object IMDelphi: TImage
    Left = 8
    Top = 25
    Width = 256
    Height = 256
  end
  object IMDWScript: TImage
    Left = 280
    Top = 25
    Width = 256
    Height = 256
  end
  object LADelphi: TLabel
    Left = 8
    Top = 8
    Width = 43
    Height = 14
    Caption = 'LADelphi'
  end
  object LADWScript: TLabel
    Left = 280
    Top = 8
    Width = 59
    Height = 14
    Caption = 'LADWScript'
  end
  object DelphiWebScript: TDelphiWebScript
    Config.MaxDataSize = 0
    Config.Timeout = 0
    Left = 144
    Top = 8
  end
end
