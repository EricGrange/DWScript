const cHex = ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'];

procedure PrintBlob(r : Variant);
begin
	var s := String(r);
	var i := 1;
	if s.StartsWith(#$ff) then begin
		Print('-');
		i += 1;
	end;
	for i := i to s.Length do begin
		var b := Ord(s[i]);
		Print(cHex[b shr 4]);
		Print(cHex[b and 15]);
	end;
	PrintLn('');
end;

var bm := BigInteger($ff);
var bf := bm.ToBlobParameter;
PrintBlob(bf);
PrintLn(BlobFieldToBigInteger(bf));

bm := BigInteger(-1);
bf := bm.ToBlobParameter;
PrintBlob(bf);
PrintLn(BlobFieldToBigInteger(bf));

for var i := 0 to 16 do begin
	var s := '';
	for var j := 1 to i do
		s += 'f';
	var b := s.ToBigInteger(16);
	var p := b.ToBlobParameter;
	PrintBlob(p);
	if BlobFieldToBigInteger(p) <> b then PrintLn('bug');
end;

for var i := 1 to 16 do begin
	var s := '-';
	for var j := 1 to i do
		s += 'f';
	var b := s.ToBigInteger(16);
	var p := b.ToBlobParameter;
	PrintBlob(p);
	if BlobFieldToBigInteger(p) <> b then PrintLn('bug');
end;
