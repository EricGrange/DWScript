var a := '613069000200208324900377821045138855'.ToBigInteger;

for var i := 1 to 51 step 10 do begin

	var b := a * i + (i div 2);
	PrintLn(b.ToString);
	PrintLn(b mod i);
	PrintLn(b div i);

end;
