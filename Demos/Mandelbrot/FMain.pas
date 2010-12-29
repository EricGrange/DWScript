unit FMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, dwsComp, dwsExprs, dwsFunctions, dwsSymbols;

type
  TMainForm = class(TForm)
    IMDelphi: TImage;
    IMDWScript: TImage;
    LADelphi: TLabel;
    LADWScript: TLabel;
    DelphiWebScript: TDelphiWebScript;
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
    FBitmap : TBitmap;
    FScanLines : array of Pointer;
    procedure PrepareBitmap;
    procedure SetPixel(x, y, color : Integer);

    procedure PaintBitmapDelphi;
    procedure PaintBitmapDWSscript;
  public
    { Déclarations publiques }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

type

   // TSetPixelMagic
   //
   TSetPixelMagic = class (TInternalMagicProcedure)
      procedure DoEvalProc(args : TExprBaseList); override;
   end;

// DoEvalProc
//
procedure TSetPixelMagic.DoEvalProc(args : TExprBaseList);
begin
   MainForm.SetPixel(args.AsInteger[0], args.AsInteger[1], args.AsInteger[2]);
end;

// FormCreate
//
procedure TMainForm.FormCreate(Sender: TObject);
var
   tStart, tStop, tFreq : Int64;
begin
   RegisterInternalProcedure(TSetPixelMagic, 'SetPixel', ['x', 'Integer', 'y', 'Integer', 'color', 'Integer']);

   QueryPerformanceFrequency(tFreq);

   PrepareBitmap;
   QueryPerformanceCounter(tStart);

   PaintBitmapDelphi;

   QueryPerformanceCounter(tStop);
   IMDelphi.Picture.Assign(FBitmap);
   FBitmap.Free;
   LADelphi.Caption:=Format('Delphi: %0.1f ms', [1000*(tStop-tStart)/tFreq]);

   PrepareBitmap;
   QueryPerformanceCounter(tStart);

   PaintBitmapDWSscript;

   QueryPerformanceCounter(tStop);
   IMDWScript.Picture.Assign(FBitmap);
   FBitmap.Free;
   LADWScript.Caption:=Format('DWScript: %0.1f ms (incl. compilation)', [1000*(tStop-tStart)/tFreq]);
end;

// PrepareBitmap
//
procedure TMainForm.PrepareBitmap;
var
   i : Integer;
begin
   FBitmap:=TBitmap.Create;
   FBitmap.PixelFormat:=pf32bit;
   FBitmap.Width:=400;
   FBitmap.Height:=400;

   SetLength(FScanLines, FBitmap.Height);
   for i:=0 to FBitmap.Height-1 do
      FScanLines[i]:=FBitmap.ScanLine[i];
end;

// SetPixel
//
procedure TMainForm.SetPixel(x, y, color : Integer);
const
   cColors: array[0..14] of TColor = (
      $000022, $000033, $000044, $000055, $000066, $000077, $000088,
      $101099, $3030AA, $4040BB, $5050CC, $6060DD, $7070EE, $8080FF, $000000
      );
begin
   PIntegerArray(FScanLines[y])[x]:=cColors[color];
end;

// PaintBitmapDelphi
//
procedure TMainForm.PaintBitmapDelphi;
var
   i, j, newColor : Integer;
   u, v, x, y, z : Double;
begin
   for i := 0 to 400-2 do begin
      for j := 0 to 400-2 do begin
         x := -0.8 + 3 * i / 400;
         y := -1.4 + 2.8 * j / 400;
         newColor := 0;
         u := 0;
         v := 0;
         repeat
            z := Sqr(u) - Sqr(v) - x;
            v := 2 * u * v - y;
            u := z;
            newColor := newColor + 1;
         until (Sqr(u) + Sqr(v) > 9) or (newColor = 14);
         SetPixel(i + 1, j + 1, newColor);
      end;
   end;
end;

// PaintBitmapDWSscript
//
procedure TMainForm.PaintBitmapDWSscript;
const
   cSource = ''
      +'var i, j, newColor : Integer;'#13#10
      +'var u, v, x, y, z : Float;'#13#10
      +#13#10
      +'for i := 0 to 400-2 do begin'#13#10
      +'   for j := 0 to 400-2 do begin'#13#10
      +'      x := -0.8 + 3 * i / 400;'#13#10
      +'      y := -1.4 + 2.8 * j / 400;'#13#10
      +'      newColor := 0;'#13#10
      +'      u := 0;'#13#10
      +'      v := 0;'#13#10
      +'      repeat'#13#10
      +'         z := Sqr(u) - Sqr(v) - x;'#13#10
      +'         v := 2 * u * v - y;'#13#10
      +'         u := z;'#13#10
      +'         newColor := newColor + 1;'#13#10
      +'      until (Sqr(u) + Sqr(v) > 9) or (newColor = 14);'#13#10
      +'      SetPixel(i + 1, j + 1, newColor);'#13#10
      +'   end;'#13#10
      +'end;';

var
   prog : TdwsProgram;
begin
   prog:=DelphiWebScript.Compile(cSource);
   try
      if prog.Msgs.Count=0 then
         prog.Execute
      else ShowMessage(prog.Msgs.AsInfo);
   finally
      prog.Free;
   end;
end;

end.
