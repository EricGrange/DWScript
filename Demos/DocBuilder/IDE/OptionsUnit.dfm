object FormOptions: TFormOptions
  Left = 0
  Top = 0
  Caption = 'Options'
  ClientHeight = 74
  ClientWidth = 302
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    302
    74)
  PixelsPerInch = 96
  TextHeight = 13
  object LblOutputDir: TLabel
    Left = 8
    Top = 11
    Width = 81
    Height = 13
    Caption = 'Output Directory'
  end
  object EdtOutputDir: TEdit
    Left = 96
    Top = 8
    Width = 201
    Height = 21
    TabOrder = 0
    Text = '.\Docs\'
  end
  object BtnOK: TButton
    Left = 138
    Top = 41
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object BtnCancel: TButton
    Left = 219
    Top = 41
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
