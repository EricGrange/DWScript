object MainForm: TMainForm
  Left = 370
  Top = 122
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 8
  Caption = 'MainForm'
  ClientHeight = 520
  ClientWidth = 1006
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClick = FormClick
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object IMDelphi: TImage
    Left = 0
    Top = 20
    Width = 500
    Height = 500
    Stretch = True
    OnClick = FormClick
  end
  object IMDWScript: TImage
    Left = 506
    Top = 20
    Width = 500
    Height = 500
    Stretch = True
    OnClick = FormClick
  end
  object LADelphi: TLabel
    Left = 0
    Top = 0
    Width = 48
    Height = 15
    Caption = 'LADelphi'
  end
  object LADWScript: TLabel
    Left = 506
    Top = 0
    Width = 63
    Height = 15
    Caption = 'LADWScript'
  end
  object DelphiWebScript: TDelphiWebScript
    Left = 144
    Top = 8
  end
end
