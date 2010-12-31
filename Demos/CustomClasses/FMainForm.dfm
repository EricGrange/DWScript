object MainForm: TMainForm
  Left = 462
  Top = 93
  Caption = 'CustomFuncs'
  ClientHeight = 406
  ClientWidth = 826
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    826
    406)
  PixelsPerInch = 96
  TextHeight = 14
  object MECode: TMemo
    Left = 8
    Top = 8
    Width = 482
    Height = 389
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'procedure PlanetDetails(planet : TPlanet);'
      'var'
      '   i : Integer;'
      '   satellite : TPlanet;'
      'begin'
      '   PrintLn('#39'--- Planet ---'#39');'
      '   PrintLn('#39'Nom: '#39'+planet.Name);'
      ''
      '   if planet.SatelliteCount>0 then begin'
      ''
      '      Print('#39'Satellites: '#39');'
      '      satellite:=planet.Satellites[0];'
      '      Print(satellite.Name);'
      '      for i:=1 to planet.SatelliteCount-1 do begin'
      '         satellite:=planet.Satellites[i];'
      '         Print('#39', '#39'+satellite.Name);'
      '      end;'
      '      PrintLn('#39#39');'
      ''
      '   end else begin'
      ''
      '      PrintLn('#39'Satellite: none'#39')'
      ''
      '   end;'
      '   PrintLn('#39#39');'
      'end;'
      ''
      'var planet : TPlanet;'
      'var satellite : TPlanet;'
      ''
      'planet:=TEarth.Create;'
      'PlanetDetails(planet);'
      ''
      'satellite:=planet.Satellites[0];'
      'PlanetDetails(satellite);'
      ''
      'PrintLn('#39'Let'#39#39's check instance reuse:'#39');'
      ''
      'var moon : TMoon = TMoon.Create;'
      ''
      'if moon=satellite then'
      '   PrintLn('#39'New moon is same TMoon instance'#39')'
      'else PrintLn('#39'New moon is different TMoon instance'#39');'
      ''
      'if planet.Satellites[0]=satellite then'
      '   PrintLn('#39'Re-queried satellite is same TMoon instance'#39')'
      
        'else PrintLn('#39'Re-queried satellite is different TMoon instance'#39')' +
        ';'
      ''
      'if moon.Name=satellite.Name then'
      '   PrintLn('#39'New moon has same TPlanet.Name'#39')'
      'else PrintLn('#39'New moon has different TPlanet.Name'#39');'
      ''
      ''
      '')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object BURun: TButton
    Left = 496
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Run'
    TabOrder = 1
    OnClick = BURunClick
  end
  object MEResult: TMemo
    Left = 496
    Top = 39
    Width = 321
    Height = 358
    Anchors = [akTop, akRight, akBottom]
    TabOrder = 2
  end
  object DelphiWebScript: TDelphiWebScript
    Left = 560
    Top = 72
  end
  object dwsUnit: TdwsUnit
    Script = DelphiWebScript
    Classes = <
      item
        Name = 'TPlanet'
        Constructors = <
          item
            Name = 'Create'
            OnEval = dwsUnitClassesTPlanetConstructorsCreateEval
            Attributes = [maVirtual]
          end>
        Fields = <>
        Methods = <
          item
            Name = 'GetName'
            ResultType = 'String'
            OnEval = dwsUnitClassesTPlanetMethodsGetNameEval
            Kind = mkFunction
          end
          item
            Name = 'GetSatellite'
            Parameters = <
              item
                Name = 'position'
                DataType = 'Integer'
              end>
            ResultType = 'TPlanet'
            OnEval = dwsUnitClassesTPlanetMethodsGetSatelliteEval
            Kind = mkFunction
          end
          item
            Name = 'SatelliteCount'
            ResultType = 'Integer'
            OnEval = dwsUnitClassesTPlanetMethodsSatelliteCountEval
            Kind = mkFunction
          end>
        Operators = <>
        Properties = <
          item
            Name = 'Name'
            DataType = 'String'
            ReadAccess = 'GetName'
            IsDefault = False
          end
          item
            Name = 'Satellites'
            DataType = 'TPlanet'
            ReadAccess = 'GetSatellite'
            Parameters = <
              item
                Name = 'position'
                DataType = 'Integer'
              end>
            IsDefault = False
          end>
        OnCleanUp = dwsUnitClassesTPlanetCleanUp
      end
      item
        Name = 'TEarth'
        Ancestor = 'TPlanet'
        Constructors = <
          item
            Name = 'Create'
            OnEval = dwsUnitClassesTEarthConstructorsCreateEval
            Attributes = [maOverride]
          end>
        Fields = <>
        Methods = <>
        Operators = <>
        Properties = <>
        OnCleanUp = dwsUnitClassesTPlanetCleanUp
      end
      item
        Name = 'TMoon'
        Ancestor = 'TPlanet'
        Constructors = <
          item
            Name = 'Create'
            OnEval = dwsUnitClassesTMoonConstructorsCreateEval
            Attributes = [maOverride]
          end>
        Fields = <>
        Methods = <>
        Operators = <>
        Properties = <>
        OnCleanUp = dwsUnitClassesTPlanetCleanUp
      end>
    UnitName = 'Test'
    StaticSymbols = False
    Left = 656
    Top = 72
  end
end
