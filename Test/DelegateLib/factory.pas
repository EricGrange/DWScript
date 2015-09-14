unit Factory;

interface

type
  TMyStuff = class
  public
    procedure DoIt(const Msg: string);
  end;

function Stuff: TMyStuff;

implementation

var FStuff: TMyStuff = nil;

function Stuff: TMyStuff;
begin
  if (FStuff = nil) then
    FStuff := TMyStuff.Create;

  Result := FStuff;
end;

procedure TMyStuff.DoIt(const Msg: string);
begin
  PrintLn(Msg);
end;

initialization
finalization
  FStuff.Free;
end;
