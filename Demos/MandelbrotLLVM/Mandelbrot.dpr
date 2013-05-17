program Mandelbrot;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  Forms,
  dwsLLVM in '..\..\Libraries\LlvmCodeGen\dwsLLVM.pas',
  dwsLLVMClasses in '..\..\Libraries\LlvmCodeGen\dwsLLVMClasses.pas',
  dwsLLVMCodeGen in '..\..\Libraries\LlvmCodeGen\dwsLLVMCodeGen.pas',
  FMain in 'FMain.pas' {MainForm};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
