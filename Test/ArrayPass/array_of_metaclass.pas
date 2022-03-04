type TFoo = class end;
type TBar = class end;

procedure Test(t : array of TClass);
begin
   PrintLn(t.Length);
   for var c in t do
      PrintLn(c.ClassName);
end;

Test([]);
Test([TObject]);
Test([TFoo]);
Test([TFoo, TBar]);
Test([TFoo, TObject]);
Test([TObject, TFoo]);
Test([TBar, TObject, TFoo]);