var a : array of Float;

procedure PrintA; begin for var i := 0 to a.High-1 do Print(a[i].ToString + ' '); PrintLn(a.Peek); end;

a.Offset(0).Multiply(0).Reciprocal;

a.Add(1, 2, 4);

a.Offset(1);

PrintA;

a.Multiply(2);

PrintA;

a.Offset(-2).Reciprocal;

PrintA;
