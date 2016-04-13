
var b := BigInteger(1);

PrintLn(BigInteger(0).BitLength);

for var i := 1 to 70 do begin
	PrintLn(b.BitLength);
	b := b + b;
end;
