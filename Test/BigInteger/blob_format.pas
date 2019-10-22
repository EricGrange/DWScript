const cHex = ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'];

procedure PrintBlob(r : Variant);
begin
	var s := String(r);
   {$ifdef JS_CODEGEN}
	if s.StartsWith('ff') then begin
      Print('-');
      s := s.DeleteLeft(2);
   end;
   PrintLn((if Odd(s.Length) then '0') + s);
   {$else}
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
   {$endif}
end;

for var i := 0 to 20 do begin
	var s := '';
	for var j := 1 to i do
		s += cHex[j and 15];
	var b := s.ToBigInteger(16);
	PrintBlob(b.ToBlobParameter);
end;

for var i := 1 to 20 do begin
	var s := '-';
	for var j := 1 to i do
		s += cHex[j and 15];
	var b := s.ToBigInteger(16);
	PrintBlob(b.ToBlobParameter);
end;
