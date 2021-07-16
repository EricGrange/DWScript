object dwsSQLiteLib: TdwsSQLiteLib
  OldCreateOrder = False
  Height = 150
  Width = 215
  object dwsSQLite: TdwsUnit
    Dependencies.Strings = (
      'System.Data')
    Functions = <
      item
        Name = 'SQLiteBackup'
        Parameters = <
          item
            Name = 'destination'
            DataType = 'DataBase'
          end
          item
            Name = 'destinationName'
            DataType = 'String'
          end
          item
            Name = 'source'
            DataType = 'DataBase'
          end
          item
            Name = 'sourceName'
            DataType = 'String'
          end>
        OnEval = dwsSQLiteFunctionsSQLiteBackupEval
      end>
    UnitName = 'System.Data.SQLite'
    StaticSymbols = True
    Left = 72
    Top = 16
  end
end
