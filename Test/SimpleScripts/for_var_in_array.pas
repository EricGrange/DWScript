var a := [1, 2, 3];

for var i in a do
	PrintLn(i);

var ad : array of String;

ad.Add('one', 'two');
for var i in ad do
	PrintLn(i);

procedure PrintThem(const oa : array of const);
begin
	PrintLn(oa.Length);
	for var i in oa do
		PrintLn(i);
end;

PrintThem([]);
PrintThem(['alpha', 2, False, 'four']);