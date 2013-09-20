object DwsIdeWatchesFrame: TDwsIdeWatchesFrame
  Left = 0
  Top = 0
  Width = 506
  Height = 411
  TabOrder = 0
  object lvWatches: TListView
    Left = 0
    Top = 17
    Width = 506
    Height = 394
    Align = alClient
    Columns = <
      item
        Caption = 'Expression'
        Width = 80
      end
      item
        Caption = 'Value'
        Width = 150
      end>
    PopupMenu = WatchWindowPopupMenu
    TabOrder = 0
    ViewStyle = vsReport
  end
  object PanelHeader: TPanel
    Left = 0
    Top = 0
    Width = 506
    Height = 17
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Watches'
    TabOrder = 1
  end
  object ActionList: TActionList
    Left = 56
    Top = 80
    object actDeleteWatch: TAction
      Caption = 'Delete Watch'
      ShortCut = 16452
      OnExecute = actDeleteWatchExecute
      OnUpdate = actDeleteWatchUpdate
    end
    object actAddWatch: TAction
      Caption = 'Add Watch'
      ShortCut = 16449
      OnExecute = actAddWatchExecute
    end
    object actEditWatch: TAction
      Caption = 'Edit Watch'
      ShortCut = 16453
      OnExecute = actEditWatchExecute
      OnUpdate = actEditWatchUpdate
    end
  end
  object WatchWindowPopupMenu: TPopupMenu
    Left = 168
    Top = 80
    object MenuItemAddWatch: TMenuItem
      Action = actAddWatch
    end
    object MenuItemDeleteWatch: TMenuItem
      Action = actDeleteWatch
    end
    object MenuItemEditWatch: TMenuItem
      Action = actEditWatch
    end
  end
end
