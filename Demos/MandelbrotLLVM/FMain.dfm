object MainForm: TMainForm
  Left = 370
  Top = 122
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 8
  Caption = 'MainForm'
  ClientHeight = 517
  ClientWidth = 1006
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClick = FormClick
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object IMDelphi: TImage
    Left = 0
    Top = 17
    Width = 500
    Height = 500
    OnClick = FormClick
  end
  object IMDWScript: TImage
    Left = 506
    Top = 17
    Width = 500
    Height = 500
    OnClick = FormClick
  end
  object LADelphi: TLabel
    Left = 0
    Top = 0
    Width = 43
    Height = 14
    Caption = 'LADelphi'
  end
  object LADWScript: TLabel
    Left = 506
    Top = 0
    Width = 59
    Height = 14
    Caption = 'LADWScript'
  end
  object DelphiWebScript: TDelphiWebScript
    Left = 144
    Top = 8
  end
end
