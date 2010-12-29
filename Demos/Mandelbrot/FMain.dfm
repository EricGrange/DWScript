object MainForm: TMainForm
  Left = 383
  Top = 136
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 8
  Caption = 'MainForm'
  ClientHeight = 417
  ClientWidth = 806
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
    Left = 0
    Top = 17
    Width = 400
    Height = 400
  end
  object IMDWScript: TImage
    Left = 406
    Top = 17
    Width = 400
    Height = 400
  end
  object LADelphi: TLabel
    Left = 0
    Top = 0
    Width = 43
    Height = 14
    Caption = 'LADelphi'
  end
  object LADWScript: TLabel
    Left = 406
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
