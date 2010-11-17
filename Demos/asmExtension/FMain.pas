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
   prog : TdwsProgram;
begin
   prog:=DelphiWebScript.Compile(MECode.Lines.Text);
   try
      if prog.Msgs.Count>0 then
         ShowMessage(prog.Msgs.AsInfo)
      else begin
         prog.Execute;
         if prog.Msgs.Count>0 then
            ShowMessage(prog.Msgs.AsInfo)
         else ShowMessage((prog.Result as TdwsDefaultResult).Text);
      end;
   finally
      prog.Free;
   end;
end;

end.
