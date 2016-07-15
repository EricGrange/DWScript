object dwsSQLiteVTArraysLib: TdwsSQLiteVTArraysLib
  OldCreateOrder = False
  Left = 282
  Top = 170
  Height = 150
  Width = 215
  object dwsVTArrays: TdwsUnit
    Dependencies.Strings = (
      'System.Data')
    Functions = <
      item
        Name = 'ExposeArray'
        Parameters = <
          item
            Name = 'db'
            DataType = 'DataBase'
          end
          item
            Name = 'a'
            DataType = 'array of Any Type'
          end
          item
            Name = 'tableName'
            DataType = 'String'
          end>
        OnEval = dwsVTArraysFunctionsEnableVTArraysEval
      end>
    UnitName = 'System.Data.VTArrays'
    StaticSymbols = True
    Left = 72
    Top = 32
  end
end
