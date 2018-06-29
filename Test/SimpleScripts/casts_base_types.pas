var i := Integer(1);
i := i + Integer(i);

PrintLn(i);

var f := Float(1.25);
f := 0.25 + Float(f);

PrintLn(f);

PrintLn(i + Integer(f));
PrintLn(Float(i) + f);
PrintLn(i + f);

var b := Boolean(True);

PrintLn(b);

PrintLn(Boolean(i));
PrintLn(Boolean(f));

i:=0;
f:=0;

PrintLn(Boolean(i));
PrintLn(Boolean(f));

