unit FMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, dwsComp, dwsExprs, dwsCompiler, dwsAsmLibModule;

type
  TasmExtensionTest = class(TForm)
    MECode: TMemo;
    DelphiWebScript: TDelphiWebScript;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    FAsmExtension : TdwsASMLibModule;
  end;

var
  asmExtensionTest: TasmExtensionTest;

implementation

{$R *.dfm}

procedure TasmExtensionTest.FormCreate(Sender: TObject);
begin
   FAsmExtension:=TdwsASMLibModule.Create(Self);
   FAsmExtension.Script:=DelphiWebScript;
end;

procedure TasmExtensionTest.Button1Click(Sender: TObject);
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=DelphiWebScript.Compile(MECode.Lines.Text);

   if prog.Msgs.Count>0 then
      ShowMessage(prog.Msgs.AsInfo)
   else begin
      exec:=prog.Execute;
      if exec.Msgs.Count>0 then
         ShowMessage(exec.Msgs.AsInfo)
      else ShowMessage(exec.Result.ToString);
   end;
end;

end.
