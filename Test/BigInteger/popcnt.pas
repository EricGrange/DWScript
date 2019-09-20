var b : BigInteger;

PrintLn(b.PopCount);
for var i := 1 to 70 step 5 do begin
	b := StringOfChar('1', i).ToBigInteger;
   PrintLn(b.PopCount);
end;
