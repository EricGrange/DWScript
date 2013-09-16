unit OptionsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormOptions = class(TForm)
    EdtOutputDir: TEdit;
    LblOutputDir: TLabel;
    BtnOK: TButton;
    BtnCancel: TButton;
  private
  public
  end;

implementation

{$R *.dfm}

end.

