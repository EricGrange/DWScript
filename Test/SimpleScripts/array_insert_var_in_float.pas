var v : Variant;
var t : array of Float;

v := 1.5;
t.Insert(0,v);

v := 10;
t.Insert(1,v);

PrintLn(t[0]);
PrintLn(t[1]);

v := 'bug';
try
   t.Insert(0,v);
except
   on E: Exception do
      PrintLn(E.Message);
end;
