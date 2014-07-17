unit Form1;

interface

type
  TForm1 = class
    procedure W3Button1Click(Sender: TObject);
  end;

implementation

{ TForm1 }

function TForm1.W3Button1Click(Sender: TObject);


procedure TForm1.W3Button1Click(Sender: TObject);
var
  Test: string;
begin
  Test := IntToStr(RandomInt(4));
  ShowMessage(Test);
end;

end.