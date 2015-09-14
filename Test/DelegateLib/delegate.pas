uses
  Test.Delegate;

procedure DoOnTest(Sender: TObject);
begin
  PrintLn(Sender.ClassName);
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