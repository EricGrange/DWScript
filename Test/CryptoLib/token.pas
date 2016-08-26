for var i := 0 to 15 step 5 do
	PrintLn(CryptographicToken(i).Length);

var a : array of String;
for var i := 1 to 100 do begin
	var token := CryptographicToken;
	if token in a then Println(token);
	a.Add(token);
end;
