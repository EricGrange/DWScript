var ab : array of array of Integer;

ab.SetLength(2);
for var i:=0 to ab.High do
	for var j:=1 to 3 do
		ab[i].Add(i*10+j);

PrintLn(JSON.Stringify(ab));

var r : array of record a : array of Integer end;

r.SetLength(2);
for var i:=0 to r.High do
	for var j:=1 to 3 do
		r[i].a.Add(i*10+j);

PrintLn(JSON.Stringify(r));