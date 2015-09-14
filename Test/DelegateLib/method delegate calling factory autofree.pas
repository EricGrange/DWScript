uses
  Factory;

procedure DoOnTest(Sender: TObject);
begin
  Stuff.DoIt(Sender.ClassName);
//  PrintLn(Sender.ClassName);
end;

var Foo := TFoo.Create;
Foo.OnTest := DoOnTest;
Foo.Test;
