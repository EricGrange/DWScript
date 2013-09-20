object DwsIdeCallStackFrame: TDwsIdeCallStackFrame
  Left = 0
  Top = 0
  Width = 506
  Height = 411
  TabOrder = 0
  object memCallStack: TMemo
    Left = 0
    Top = 17
    Width = 506
    Height = 394
    Align = alClient
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object PanelHeader: TPanel
    Left = 0
    Top = 0
    Width = 506
    Height = 17
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Call Stack'
    TabOrder = 1
  end
end
