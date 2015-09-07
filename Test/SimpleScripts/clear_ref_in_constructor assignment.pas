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
vMyStuff := TMyStuff.Create;
vMyStuff.Free;
