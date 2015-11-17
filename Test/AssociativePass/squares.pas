const NB = 100;
var a : array [Integer] of Integer;

PrintLn('Add');

for var i := 1 to NB do 
	a[i] := i*i;

for var i := 1 to NB do 
	if a[i] <> i*i then 
		PrintLn(i);

PrintLn('Replace');

for var i := 1 to NB do 
	a[i] := i*i+1;

for var i := 1 to NB do 
	if a[i] <> i*i+1 then 
		PrintLn(i);
