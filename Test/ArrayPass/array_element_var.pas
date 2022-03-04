var a : array of Float;

procedure TestResize(var v : Float);
begin
   v := 1234;
   a.Clear;
   a.SetLength(1);
   PrintLn(v);
end;

a.SetLength(1);
TestResize(a[0]);

procedure TestOutOfBound(var v : Float);
begin
   a.Clear;
   try PrintLn(v) except on E : Exception do PrintLn('Read: ' + E.Message) end;
   try v := 1234 except on E : Exception do PrintLn('Write: ' + E.Message) end;
end;

TestOutOfBound(a[0]);
