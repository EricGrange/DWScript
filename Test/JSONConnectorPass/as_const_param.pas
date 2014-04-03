procedure TestVal(const value: JSONVariant);
begin
	PrintLn(JSON.Stringify(value));
end;

procedure Test;
var data: JSONVariant;
begin
   data := JSON.Parse('[ [], [], "hello" ]');
   for var i := 0 to data.length() - 1 do
      testVal(data[i]);

   println('Success!');
end;

Test;