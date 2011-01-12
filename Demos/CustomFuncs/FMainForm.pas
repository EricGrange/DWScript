unit FMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dwsComp, StdCtrls, dwsExprs, dwsCompiler;

type
  TMainForm = class(TForm)
    MECode: TMemo;
    BURun: TButton;
    MEResult: TMemo;
    DelphiWebScript: TDelphiWebScript;
    dwsUnit: TdwsUnit;
    procedure dwsUnitFunctionsSayHelloEval(Info: TProgramInfo);
    procedure dwsUnitFunctionsGetComputerNameEval(Info: TProgramInfo);
    procedure dwsUnitFunctionsMultiplyByTwoEval(Info: TProgramInfo);
    procedure BURunClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.dwsUnitFunctionsSayHelloEval(Info: TProgramInfo);
begin
   ShowMessage('Hello '+Info.ValueAsString['toWho']+' !');
end;

procedure TMainForm.dwsUnitFunctionsGetComputerNameEval(Info: TProgramInfo);
var
   buffer : array[0..255] of Char;
   size : DWord;
begin
   size:=256;
   GetComputerName(buffer, size);
   Info.ResultAsString:=buffer;
end;

procedure TMainForm.dwsUnitFunctionsMultiplyByTwoEval(Info: TProgramInfo);
begin
   Info.ValueAsInteger['value']:=2*Info.ValueAsInteger['value'];
end;

procedure TMainForm.BURunClick(Sender: TObject);
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=DelphiWebScript.Compile(MECode.Lines.Text);

   if prog.Msgs.Count=0 then begin
      exec:=prog.Execute;
      MEResult.Lines.Text:=exec.Result.ToString;
   end else MEResult.Lines.Text:=prog.Msgs.AsInfo;
end;

end.
