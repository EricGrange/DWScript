unit InitFactoryUnit;

interface

type
  TMyStuff = class
  public
    destructor Destroy; override;
  end;

function MyStuff: TMyStuff;

implementation

var
  FMyStuff: TMyStuff = nil;

destructor TMyStuff.Destroy;
begin
  //FMyStuff := nil;
  inherited;
end;

function MyStuff: TMyStuff;
begin
  if (FMyStuff = nil) then
    FMyStuff := TMyStuff.Create;
  Result := FMyStuff;
end;

initialization
  FMyStuff := TMyStuff.Create;
finalization
  //FMyStuff.Free;
end;