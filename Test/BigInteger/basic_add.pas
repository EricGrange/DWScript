var a : BigInteger;

PrintLn(a.ToHex);

for var i := 1 to 1000 step 11 do begin

	a := (i + a) + (a + i);
	PrintLn(a.ToHex);

end;

