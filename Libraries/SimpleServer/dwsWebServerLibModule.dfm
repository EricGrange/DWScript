object dwsWebServerLib: TdwsWebServerLib
  OldCreateOrder = False
  Left = 810
  Top = 86
  Height = 150
  Width = 215
  object dwsWebServer: TdwsUnit
    Classes = <
      item
        Name = 'WebServer'
        IsStatic = True
        Methods = <
          item
            Name = 'Name'
            ResultType = 'String'
            Attributes = [maClassMethod, maStatic]
            Kind = mkClassFunction
          end>
      end>
    UnitName = 'System.WebServer'
    StaticSymbols = False
    Left = 48
    Top = 24
  end
end
