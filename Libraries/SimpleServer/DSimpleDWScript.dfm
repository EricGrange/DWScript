object SynDWScript: TSynDWScript
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 745
  Top = 94
  Height = 249
  Width = 341
  object DelphiWebScript: TDelphiWebScript
    Config.Filter = dwsHtmlFilter
    Config.CompilerOptions = [coOptimize, coAssertions, coHintsDisabled, coWarningsDisabled]
    Config.StackChunkSize = 256
    Left = 72
    Top = 32
  end
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
  object dwsRTTIConnector: TdwsRTTIConnector
    Script = DelphiWebScript
    StaticSymbols = False
    Left = 72
    Top = 96
  end
end
