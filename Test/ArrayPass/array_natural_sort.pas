var aInt : array of Integer;
var aFloat : array of Float;
var aStr : array of String;

for var i:=1 to 19 do begin
	var k:=i xor 13;
	aInt.Add(k);
	aFloat.Add(k-1.5);
	aStr.Add('z'+k.ToString);
end;

aInt.Sort;
for var e in aInt do
	Print(e.ToString+',');
PrintLn('');

aFloat.Sort;
for var e in aFloat do
	Print(e.ToString+',');
PrintLn('');

aStr.Sort;
PrintLn(aStr.Join(','));