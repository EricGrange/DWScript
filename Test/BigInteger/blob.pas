
var b : BigInteger;
PrintLn(BlobFieldToBigInteger(BigIntegerToBlobParameter(b)).ToString);

for var i := 1 to 100 do begin
	var nb := StringOfChar('9', i);
	b := BigInteger(nb);
	var blob := BigIntegerToBlobParameter(b);
	var fb := BlobFieldToBigInteger(blob);
	if b <> fb then
		PrintLn('Failed numeric match for '+b.ToString+': '+fb.ToString);
	if fb.ToString <> nb then
		PrintLn('Failed string match for '+nb+': '+fb.ToString);
end;

for var i := 1 to 100 do begin
	var nb := '-' + StringOfChar('9', i);
	b := BigInteger(nb);
	var blob := BigIntegerToBlobParameter(b);
	var fb := BlobFieldToBigInteger(blob);
	if b <> fb then
		PrintLn('Failed negative numeric match for '+b.ToString+': '+fb.ToString);
	if fb.ToString <> nb then
		PrintLn('Failed negative string match for '+nb+': '+fb.ToString);
end;
