var j : JSONVariant;

PrintLn(j.TypeName());
j := 1;
PrintLn(j.TypeName());
j := "a";
PrintLn(j.TypeName());
j := 1.5;
PrintLn(j.TypeName());
j := False;
PrintLn(j.TypeName());
j := Null;
PrintLn(j.TypeName());

procedure Test(v : JSONVariant);
begin
   PrintLn(v.TypeName());
end;

var k : Variant;
Test(Null);
Test(k);
Test(0);
Test('b');
Test(0.5);
Test(True);