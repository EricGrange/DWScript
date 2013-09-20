object SimpleDWScript: TSimpleDWScript
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 745
  Top = 94
  Height = 307
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
    Config.CompileFileSystem = dwsRestrictedFileSystem
    Config.TimeoutMilliseconds = 3000
    Config.StackChunkSize = 300
    Config.OnInclude = DoInclude
    Config.OnNeedUnit = DoNeedUnit
    Left = 72
    Top = 32
  end
  object dwsRestrictedFileSystem: TdwsRestrictedFileSystem
    Left = 72
    Top = 96
  end
  object dwsFileIO: TdwsUnit
    Script = DelphiWebScript
    Functions = <
      item
        Name = 'DeleteFile'
        Parameters = <
          item
            Name = 'fileName'
            DataType = 'String'
          end>
        ResultType = 'Boolean'
        OnFastEval = dwsFileIOFunctionsDeleteFileFastEval
      end
      item
        Name = 'FileExists'
        Parameters = <
          item
            Name = 'fileName'
            DataType = 'String'
          end>
        ResultType = 'Boolean'
      end>
    UnitName = 'File.IO'
    StaticSymbols = True
    Left = 72
    Top = 168
  end
end
