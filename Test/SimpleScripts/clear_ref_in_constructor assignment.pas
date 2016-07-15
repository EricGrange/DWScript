type
  TMyStuff = class
  public
    destructor Destroy; override;
  end;

var
  vMyStuff: TMyStuff = nil;

destructor TMyStuff.Destroy;
begin
  PrintLn('hello');
  vMyStuff := nil;
  inherited;
end;

function MyStuff: TMyStuff;
begin
  if (vMyStuff = nil) then
    vMyStuff := TMyStuff.Create;
  Result := vMyStuff;
end;

MyStuff;
PrintLn(1);
vMyStuff := TMyStuff.Create;
PrintLn(2);
vMyStuff.Free;
