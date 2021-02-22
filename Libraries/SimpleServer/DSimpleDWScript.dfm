object SimpleDWScript: TSimpleDWScript
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 282
  Width = 341
  object dwsHtmlFilter: TdwsHtmlFilter
    PatternOpen = '<?pas'
    PatternClose = '?>'
    PatternEval = '='
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
    Config.CompileFileSystem = dwsCompileSystem
    Config.RuntimeFileSystem = dwsRuntimeFileSystem
    Config.TimeoutMilliseconds = 3000
    Config.StackChunkSize = 300
    Config.OnInclude = DoInclude
    Config.OnNeedUnit = DoNeedUnit
    Left = 72
    Top = 32
  end
  object dwsCompileSystem: TdwsRestrictedFileSystem
    Left = 72
    Top = 96
  end
  object dwsRuntimeFileSystem: TdwsRestrictedFileSystem
    Left = 72
    Top = 160
  end
  object dwsComConnector: TdwsComConnector
    StaticSymbols = False
    Left = 208
    Top = 160
  end
  object dwsDatabaseFileSystem: TdwsRestrictedFileSystem
    Left = 72
    Top = 224
  end
end
