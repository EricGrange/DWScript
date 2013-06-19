unit FMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls,
  dwsComp, dwsExprs, dwsFunctions, dwsSymbols, dwsMagicExprs, dwsExprList,
  dwsLLVM, dwsLLVMClasses, dwsLLVMCodeGen;

type
  TMainForm = class(TForm)
    IMDelphi: TImage;
    IMDWScript: TImage;
    LADelphi: TLabel;
    LADWScript: TLabel;
    DelphiWebScript: TDelphiWebScript;
    procedure FormCreate(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FBitmap : TBitmap;
    FScanLines : array of Pointer;
    FLLVMCodeGen: TdwsLLVMCodeGen;
    FJITOptimizations: TLLVMCodeGenOptLevel;
    procedure PrepareBitmap;
    procedure SetPixel(x, y, color : Integer);

    procedure PaintBitmapDelphi;
    procedure PaintBitmapDWSscript;
  public
    procedure Benchmark;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  dwsXPlatform;

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

   // create LLVM codegent and set module name (not required)
   FLLVMCodeGen := TdwsLLVMCodeGen.Create;
   FLLVMCodeGen.ModuleName := 'Mandelbrot';

   // tweak optimisations
   FLLVMCodeGen.Optimizations := loDefault;
   FJITOptimizations := LLVMCodeGenLevelNone;

   // custom passes (only applied if FLLVMCodeGen.Optimizations := loCustom;)
   FLLVMCodeGen.CustomOptimizationPasses := [cpDeadStoreEliminationPass,
      cpInstructionCombiningPass, cpPromoteMemoryToRegisterPass];

   // initialize native target
   if LLVMInitializeNativeTarget then
      raise Exception.Create('Initialization of native target failed');

   // initialize JIT compiler (interpreter is outdated and may not work)
   LLVMLinkInJIT;

   Benchmark;
end;

// FormDestroy
//
procedure TMainForm.FormDestroy(Sender: TObject);
begin
   FreeAndNil(FLLVMCodeGen);
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
   tStart, tStop, tFreq : Int64;
   i : Integer;
begin
   QueryPerformanceFrequency(tFreq);

   PrepareBitmap;
   QueryPerformanceCounter(tStart);

   for i:=1 to cNB_LOOPS do
      PaintBitmapDelphi;

   QueryPerformanceCounter(tStop);
   IMDelphi.Picture.Assign(FBitmap);
   FBitmap.Free;
   LADelphi.Caption:=Format('Delphi: %0.1f ms', [1000*(tStop-tStart)/tFreq]);

   PrepareBitmap;
   QueryPerformanceCounter(tStart);

   for i:=1 to cNB_LOOPS do
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
   FBitmap.Width:=500;
   FBitmap.Height:=500;

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
   cSize = 500;
var
   i, j, newColor : Integer;
   u, v, x, y, z : Double;
begin
   for i := 0 to cSize-2 do begin
      for j := 0 to cSize-2 do begin
         x := -0.8 + 3 * i / cSize;
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

// SetPixelFlat (flattened version of SetPixel method)
//
procedure SetPixelFlat(x, y, color : Int64); cdecl;
const
   cColors: array[0..16] of TColor = (
      $000022, $000033, $000044, $000055, $000066, $000077, $000088,
      $101099, $3030AA, $4040BB, $5050CC, $6060DD, $7070EE, $8080FF,
      $9090FF, $A0A0FF,
      $000000
      );
begin
   PIntegerArray(MainForm.FScanLines[y])[x]:=cColors[color];
//   FBitmap.Canvas.Pixels[x, y]:=cColors[color];
end;

// PaintBitmapDWSscript
//
procedure TMainForm.PaintBitmapDWSscript;
const
   cSource = ''
      +'const cSize = 500;'#13#10
      +'var i, j, newColor : Integer;'#13#10
      +'var u, v, x, y, z : Float;'#13#10
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
      +'end;'#13#10
   ;

var
   prog : IdwsProgram;
   eeJIT: PLLVMExecutionEngine;
   err: PAnsiChar;
   fn: PLLVMValue;
   args, ret: PLLVMGenericValue;
   module: PLLVMModule;
begin
   prog := DelphiWebScript.Compile(cSource);

   // compile program to LLVM IR
   FLLVMCodeGen.CompileProgram(prog);

   // create LLVM JIT compiler and check for errors
   if not LLVMCreateJITCompilerForModule(eeJIT, FLLVMCodeGen.Module.Handle, FJITOptimizations, err) then
      try
         // map SetPixel function in LLVM code to SetPixelFlat
         if not LLVMFindFunction(eeJIT, 'SetPixel', fn) then
            LLVMAddGlobalMapping(eeJIT, fn, @SetPixelFlat);

         // locate main function and execute
         if not LLVMFindFunction(eeJIT, 'main', fn) then begin
            ret := LLVMRunFunction(eeJIT, fn, 0, args);
            LLVMDisposeGenericValue(ret);
         end;

         // savely remove module from execution engine
         LLVMRemoveModule(eeJIT, FLLVMCodeGen.Module.Handle, module, err);
      finally
         LLVMDisposeExecutionEngine(eeJIT);
      end
   else begin
      raise Exception.Create(string(AnsiString(err)));
      LLVMDisposeMessage(err);
   end;
end;

end.
