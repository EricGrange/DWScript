uses
  Test.Delegate,
  Factory;

procedure DoOnTest(Sender: TObject);
begin
  Stuff.DoIt(Sender.ClassName);
end;

begin
  var OnTest: TNotifyEvent := DoOnTest;
  var Foo := TFoo.Create;
  try
    OnTest(Foo);
  finally
    Foo.Free;
  end;
end;
