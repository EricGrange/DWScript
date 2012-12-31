object SynDWScript: TSynDWScript
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 745
  Top = 94
  Height = 222
  Width = 341
  object dwsHtmlFilter: TdwsHtmlFilter
    PatternClose = '%>'
    PatternEval = '='
    PatternOpen = '<%'
    Left = 208
    Top = 32
  end
  object dwsGlobalVarsFunctions: TdwsGlobalVarsFunctions
    Left = 208
    Top = 96
  end
  object DelphiWebScript: TDelphiWebScript
    Config.Filter = dwsHtmlFilter
    Config.MaxRecursionDepth = 512
    Config.TimeoutMilliseconds = 3000
    Config.StackChunkSize = 300
    Left = 72
    Top = 32
  end
end
