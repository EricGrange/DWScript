type
  TFoo = class external 'Foo'
    a: Float;
  end;

var Foo := new TFoo;
Foo.a := Null;