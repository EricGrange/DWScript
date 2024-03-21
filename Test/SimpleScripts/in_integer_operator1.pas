for var i := 0 to 7 do begin
	var k := 0;
	if i in [ 1..3, 5, 500 ] then
		k := 1;
	if k in [ 1 ] then
		PrintLn(i.ToString + ' in [1..3, 5, 500]');
	if k not in [ 1 ] then
		PrintLn('not in ' + i.ToString);
end;

var i := 500;
if i in [ 1..3, 5, 500 ] then
	PrintLn('500 ok');
