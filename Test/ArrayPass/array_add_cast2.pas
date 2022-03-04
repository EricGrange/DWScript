var a : array of String;

a.Add(Variant('hello'));

var v : Variant := 'world';

a.Add(v);

PrintLn(a.Join(' '));

var i : array of Integer;

v := 123;

i.Add(v);

PrintLn(i[0]);

v := 'bug';

try
   i.Add(v); 
except
   on E: Exception do PrintLn('Expected Failure'); 
end;
