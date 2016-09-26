type
  TFoo = class
  private
    FBar: Float;
  public
    property Bar: Float write FBar;
  end;

var Foo := TFoo.Create;
Foo.Bar.