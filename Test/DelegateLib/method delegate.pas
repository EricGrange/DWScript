uses
  Factory;

procedure DoOnTest(Sender: TObject);
begin
  PrintLn(Sender.ClassName);
end;

begin
  var Foo := TFoo.Create;
  try
    Foo.OnTest := DoOnTest;
    Foo.Test;
  finally
    Foo.Free;
  end;
end;