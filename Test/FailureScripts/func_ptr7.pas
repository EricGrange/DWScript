type
  TFoo = class
  public
    procedure Bar(callback: procedure);
  end;  

var Foo: TFoo;
Foo.Bar(@);