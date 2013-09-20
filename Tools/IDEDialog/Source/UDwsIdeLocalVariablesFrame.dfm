object DwsIdeLocalVariablesFrame: TDwsIdeLocalVariablesFrame
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object ListView: TListView
    Left = 0
    Top = 17
    Width = 320
    Height = 223
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
  object PanelHeader: TPanel
    Left = 0
    Top = 0
    Width = 320
    Height = 17
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Local Variables'
    TabOrder = 1
  end
end
