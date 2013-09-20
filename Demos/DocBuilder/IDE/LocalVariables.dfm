object FrmLocalVariables: TFrmLocalVariables
  Left = 0
  Top = 0
  Caption = 'Local Variables'
  ClientHeight = 240
  ClientWidth = 204
  Color = clBtnFace
  DockSite = True
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnDockOver = FormDockOver
  OnStartDock = FormStartDock
  PixelsPerInch = 96
  TextHeight = 13
  object ListView: TListView
    Left = 0
    Top = 0
    Width = 204
    Height = 240
    Align = alClient
    Columns = <
      item
        Caption = 'Variable'
      end
      item
        Caption = 'Value'
        Width = 150
      end>
    TabOrder = 0
    ViewStyle = vsReport
  end
end
