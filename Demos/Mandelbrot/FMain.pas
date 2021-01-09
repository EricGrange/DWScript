unit FMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls,
  {$ifdef WIN32}
  dwsJIT, dwsJITx86,
  {$endif}
  {$ifdef WIN64}
  dwsJIT, dwsJITx86_64,
  {$endif}
  dwsComp, dwsExprs, dwsFunctions, dwsSymbols, dwsMagicExprs, dwsExprList,
  dwsErrors, dwsUtils;

type
  TMainForm = class(TForm)
    IMDelphi: TImage;
    IMDWScript: TImage;
    LADelphi: TLabel;
    LADWScript: TLabel;
    DelphiWebScript: TDelphiWebScript;
    procedure FormCreate(Sender: TObject);
    procedure FormClick(Sender: TObject);
  private
    { Déclarations privées }
    FBitmap : TBitmap;
    FScanLines : array of Pointer;
    procedure PrepareBitmap;
    procedure SetPixel(x, y, color : Integer);

    procedure PaintBitmapDelphi;
    procedure PaintBitmapDWSscript;
//    procedure PaintBitmapLaPe;
  public
    procedure Benchmark;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

(*
uses lptypes, lpparser, lpcompiler, lputils, lpvartypes, lpeval, lpinterpreter,
   lpdisassembler;

procedure LaPeSetPixel(Params: PParamArray);
begin
   MainForm.SetPixel(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

// PaintBitmapLaPe
//
procedure TMainForm.PaintBitmapLaPe;
const
   cSource = ''
      +'program test;'
      +'const cSize = 500;'#13#10
//      +'var i, j, newColor : Integer;'#13#10
//      +'var u, v, x, y, z : Double;'#13#10
//      +'begin'#13#10
//      +'for i := 0 to cSize-2 do begin'#13#10
//      +'   for j := 0 to cSize-2 do begin'#13#10
//      +'      x := -0.8 + 3.0 * i / cSize;'#13#10
//      +'      y := -1.4 + 2.8 * j / cSize;'#13#10
//      +'      newColor := 0;'#13#10
//      +'      u := 0;'#13#10
//      +'      v := 0;'#13#10
//      +'      repeat'#13#10
//      +'         z := Sqr(u) - Sqr(v) - x;'#13#10
//      +'         v := 2 * u * v - y;'#13#10
//      +'         u := z;'#13#10
//      +'         newColor := newColor + 1;'#13#10
//      +'      until (Sqr(u) + Sqr(v) > 9) or (newColor = 16);'#13#10
////      +'      SetPixel(i + 1, j + 1, newColor);'#13#10
//      +'   end;'#13#10
      +'end;'
      +'end.';
var
  Parser: TLapeTokenizerBase;
  Compiler: TLapeCompiler;
begin
   Parser := TLapeTokenizerString.Create(UTF8Encode(cSource));
   Compiler := TLapeCompiler.Create(Parser);
   try
      InitializePascalScriptBasics(Compiler, [psiTypeAlias]);
//      ExposeGlobals(Compiler);

//      Compiler.addGlobalFunc('procedure SetPixel(x, y, color : Integer);', @LaPeSetPixel);

//      Compiler.Compile();

      RunCode(Compiler.Emitter.Code);
   finally
      Compiler.Free();
      Parser.Free();
   end;
end;
*)
type

   // TSetPixelMagic
   //
   TSetPixelMagic = class (TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

// DoEvalProc
//
procedure TSetPixelMagic.DoEvalProc(const args : TExprBaseListExec);
begin
   MainForm.SetPixel(args.AsInteger[0], args.AsInteger[1], args.AsInteger[2]);
end;

// FormCreate
//
procedure TMainForm.FormCreate(Sender: TObject);
begin
   RegisterInternalProcedure(TSetPixelMagic, 'SetPixel', ['x', 'Integer', 'y', 'Integer', 'color', 'Integer']);
   Benchmark;
end;

// FormClick
//
procedure TMainForm.FormClick(Sender: TObject);
begin
   Benchmark;
end;

// Benchmark
//
procedure TMainForm.Benchmark;
const
   cNB_LOOPS = 10;
var
   tStart, tStop, tFreq, tBest : Int64;
   i : Integer;
begin
   QueryPerformanceFrequency(tFreq);

   PrepareBitmap;
   tBest := MaxInt;

   for i:=1 to cNB_LOOPS do begin
      QueryPerformanceCounter(tStart);
      PaintBitmapDelphi;
      QueryPerformanceCounter(tStop);
      if tStop-tStart < tBest then
         tBest := tStop-tStart;
   end;

   IMDelphi.Picture.Assign(FBitmap);
   FBitmap.Free;
   LADelphi.Caption:=Format('Delphi: %0.1f ms', [1000*(tStop-tStart)/tFreq]);

   PrepareBitmap;
   tBest := MaxInt;

   for i:=1 to cNB_LOOPS do begin
      QueryPerformanceCounter(tStart);
      PaintBitmapDWSscript;
//   PaintBitmapLaPe;
      QueryPerformanceCounter(tStop);
      if tStop-tStart < tBest then
         tBest := tStop-tStart;
   end;

   QueryPerformanceCounter(tStop);
   IMDWScript.Picture.Assign(FBitmap);
   FBitmap.Free;
   LADWScript.Caption:=Format('DWScript: %0.1f ms (incl. compilation)', [1000*tBest/tFreq]);
end;

// PrepareBitmap
//
procedure TMainForm.PrepareBitmap;
var
   i : Integer;
begin
   FBitmap:=TBitmap.Create;
   FBitmap.PixelFormat:=pf32bit;
   FBitmap.Width:=1000;
   FBitmap.Height:=1000;

   SetLength(FScanLines, FBitmap.Height);

   for i:=0 to FBitmap.Height-1 do
      FScanLines[i]:=FBitmap.ScanLine[i];
end;

// SetPixel
//
procedure TMainForm.SetPixel(x, y, color : Integer);
const
   cColors: array[0..16] of TColor = (
      $000022, $000033, $000044, $000055, $000066, $000077, $000088,
      $101099, $3030AA, $4040BB, $5050CC, $6060DD, $7070EE, $8080FF,
      $9090FF, $A0A0FF,
      $000000
      );
begin
   PIntegerArray(FScanLines[y])[x]:=cColors[color];
//   FBitmap.Canvas.Pixels[x, y]:=cColors[color];
end;

// PaintBitmapDelphi
//
procedure TMainForm.PaintBitmapDelphi;
const
   cSize = 1000;
var
   i, j, newColor : Integer;
   u, v, x, y, z : Double;
begin
   for i := 0 to cSize-2 do begin
      for j := 0 to cSize-2 do begin
         x := -0.8 + 3.0 * i / cSize;
         y := -1.4 + 2.8 * j / cSize;
         newColor := 0;
         u := 0;
         v := 0;
         repeat
            z := Sqr(u) - Sqr(v) - x;
            v := 2 * u * v - y;
            u := z;
            newColor := newColor + 1;
         until (Sqr(u) + Sqr(v) > 9) or (newColor = 16);
         SetPixel(i + 1, j + 1, newColor);
      end;
   end;
end;

// PaintBitmapDWSscript
//
procedure TMainForm.PaintBitmapDWSscript;
const
   cSource = ''
      +'const cSize = 1000;'#13#10
      +'var i, j, newColor : Integer;'#13#10
      +'var u, v, x, y, z : Float;'#13#10
      +#13#10
      +'for i := 0 to cSize-2 do begin'#13#10
      +'   for j := 0 to cSize-2 do begin'#13#10
      +'      x := -0.8 + 3.0 * i / cSize;'#13#10
      +'      y := -1.4 + 2.8 * j / cSize;'#13#10
      +'      newColor := 0;'#13#10
      +'      u := 0;'#13#10
      +'      v := 0;'#13#10
      +'      repeat'#13#10
      +'         z := Sqr(u) - Sqr(v) - x;'#13#10
      +'         v := 2 * u * v - y;'#13#10
      +'         u := z;'#13#10
      +'         newColor := newColor + 1;'#13#10
      +'      until (Sqr(u) + Sqr(v) > 9) or (newColor = 16);'#13#10
      +'      SetPixel(i + 1, j + 1, newColor);'#13#10
      +'   end;'#13#10
      +'end;'
   ;
type
   TJitter = {$ifdef WIN32}TdwsJITx86{$endif}{$ifdef WIN64}TdwsJITx86_64{$endif};
var
   prog : IdwsProgram;
   jitter : TJitter;
begin
   prog:=DelphiWebScript.Compile(cSource);

   jitter := TJitter.Create;
   jitter.Options := jitter.Options-[jitoNoBranchAlignment];
   jitter.GreedyJIT(prog.ProgramObject);
   jitter.Free;

   if prog.Msgs.Count=0 then
      prog.Execute
   else ShowMessage(prog.Msgs.AsInfo);
end;

end.
