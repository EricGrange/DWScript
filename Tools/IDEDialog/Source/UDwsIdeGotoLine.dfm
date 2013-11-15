object DwsIdeGotoLineNumber: TDwsIdeGotoLineNumber
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Go to Line Number'
  ClientHeight = 95
  ClientWidth = 270
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBoxLineNumber: TGroupBox
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 254
    Height = 43
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alTop
    TabOrder = 0
    object LabelLineNumber: TLabel
      Left = 16
      Top = 16
      Width = 111
      Height = 13
      Caption = 'Enter new line number:'
    end
    object ComboBoxLineNumber: TComboBox
      Left = 133
      Top = 13
      Width = 100
      Height = 21
      TabOrder = 0
      Text = '1'
      OnKeyPress = ComboBoxLineNumberKeyPress
    end
  end
  object ButtonCancel: TButton
    Left = 141
    Top = 62
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object ButtonOK: TButton
    Left = 60
    Top = 62
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
end
