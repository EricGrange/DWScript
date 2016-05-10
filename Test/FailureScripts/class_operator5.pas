type
  TFoo = class
  end;

type
  TFooClass = class of TFoo;

var
  Foo: TFoo

procedure SetFoo(Value: TFooClass);
begin
  if Value <> Foo then
  begin
  end;
end;