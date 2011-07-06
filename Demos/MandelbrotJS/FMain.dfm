object MainForm: TMainForm
  Left = 232
  Top = 130
  BorderWidth = 10
  Caption = 'Mandelbrot DWS -> JS / CEF'
  ClientHeight = 662
  ClientWidth = 816
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 816
    Height = 662
    ActivePage = TabSheet3
    Align = alClient
    TabOrder = 0
    object TabSheet3: TTabSheet
      Caption = 'Source'
      ImageIndex = 2
      ExplicitLeft = 8
      ExplicitTop = 30
      ExplicitWidth = 0
      ExplicitHeight = 0
      object MESource: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 36
        Width = 802
        Height = 593
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          '<html>'
          ''
          '<script type="text/javascript">'
          ''
          '<%pas2js'
          ''
          '// this Pascal code will get compiled into JavaScript'
          ''
          'procedure SetPixel(x, y, c : Integer); external;'
          ''
          'procedure Mandel;'
          'const'
          '   cSize = 500;'
          'var'
          '   i, j, newColor : Integer;'
          '   u, v, x, y, z : Float;'
          'begin'
          '   for i := 0 to cSize-2 do begin'
          '      for j := 0 to cSize-2 do begin'
          '         x := -0.8 + 3 * i / cSize;'
          '         y := -1.4 + 2.8 * j / cSize;'
          '         newColor := 0;'
          '         u := 0;'
          '         v := 0;'
          '         repeat'
          '            z := Sqr(u) - Sqr(v) - x;'
          '            v := 2 * u * v - y;'
          '            u := z;'
          '            newColor := newColor + 1;'
          '         until (Sqr(u) + Sqr(v) > 9) or (newColor = 14);'
          '         SetPixel(i + 1, j + 1, newColor);'
          '      end;'
          '   end;'
          'end;'
          ''
          '%>'
          ''
          'var pixData;'
          ''
          
            'var cColors = [[0,0,0x22],[0,0,0x33],[0,0,0x44],[0,0,0x55],[0,0,' +
            '0x66],'
          
            '               [0,0,0x77],[0,0,0x88],[0x10,0x10,0x99],[0x30,0x30' +
            ',0xAA],'
          
            '               [0x40,0x40,0xBB],[0x50,0x50,0xCC],[0x60,0x60,0xDD' +
            '],'
          
            '               [0x70,0x70,0xEE],[0x80,0x80,0xFF],[0x00,0x00,0x00' +
            ']];'
          'function SetPixel(x,y,c) {'
          '  var p=(x+500*y)*4;'
          '  pixData[p]=cColors[c][0];'
          '  pixData[p+1]=cColors[c][1];'
          '  pixData[p+2]=cColors[c][2];'
          '  pixData[p+3]=255;'
          '}'
          ''
          'window.onload=function () {'
          '   var canvas = document.getElementById("canvas");'
          '   var ctx = canvas.getContext("2d");'
          '   var imageData = ctx.createImageData(500, 500);'
          '   pixData = imageData.data;'
          ''
          '   var t = new Date().getTime();'
          ''
          '   Mandel();'
          ''
          '   t = new Date().getTime()-t;'
          ''
          '   document.getElementById("time").innerHTML=t+" ms";'
          '   ctx.putImageData(imageData, 0, 0);'
          '}'
          '</script>'
          ''
          '<body>'
          ''
          '<div style="text-align:center">'
          '   <div id="time"></div>'
          '   <canvas id="canvas" width="500" height="500"></canvas>'
          '</div>'
          ''
          '</body>'
          '</html>')
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        WantTabs = True
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 808
        Height = 33
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Button1: TButton
          Left = 4
          Top = 4
          Width = 120
          Height = 25
          Caption = 'Compile && Run'
          TabOrder = 0
          OnClick = Button1Click
        end
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Chromium'
      object Chromium: TChromium
        Left = 0
        Top = 0
        Width = 808
        Height = 632
        Align = alClient
        TabOrder = 0
        Options = [coAccelerated2dCanvasDisabled]
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Compiled Output'
      ImageIndex = 1
      object MEOutput: TMemo
        Left = 0
        Top = 0
        Width = 808
        Height = 632
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object DWSMain: TDelphiWebScript
    Config.Filter = HtmlFilter
    Left = 144
    Top = 80
  end
  object DWSJS: TDelphiWebScript
    Left = 144
    Top = 144
  end
  object HtmlFilter: TdwsHtmlFilter
    PatternClose = '?>'
    PatternEval = '='
    PatternOpen = '<?'
    Left = 232
    Top = 80
  end
end
