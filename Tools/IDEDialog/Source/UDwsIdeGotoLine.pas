unit UDwsIdeGotoLine;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TDwsIdeGotoLineNumber = class(TForm)
    GroupBoxLineNumber: TGroupBox;
    ButtonCancel: TButton;
    ButtonOK: TButton;
    LabelLineNumber: TLabel;
    ComboBoxLineNumber: TComboBox;
    procedure ComboBoxLineNumberKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    function GetLineNumber: Integer;
  public
    property LineNumber: Integer read GetLineNumber;
  end;

implementation

{$R *.dfm}

procedure TDwsIdeGotoLineNumber.ComboBoxLineNumberKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not CharInSet(Key, ['0'..'9']) then
    Key := #0;
end;

procedure TDwsIdeGotoLineNumber.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if ModalResult = mrOk then
    ComboBoxLineNumber.Items.Add(ComboBoxLineNumber.Text);
end;

function TDwsIdeGotoLineNumber.GetLineNumber: Integer;
begin
  Result := StrToInt(ComboBoxLineNumber.Text);
end;

end.

