const a : array [0..1] of record
		a : array[0..1] of record 
			x, y : Integer; 
		end;
	end = [
		(a: [ (x: 1; y: 2), (x: 2; y: 3) ]),
		(a: [ (x: 4; y: 5), (x: 6; y: 7) ])
	];

for var i := a.Low to a.High do begin
	var b := a[i];
	for var j := b.a.Low to b.a.High do begin
		var c := b.a[j];
		PrintLn(c.x.ToString + ',' + c.y.ToString);
	end;
	for var j := b.a.Low to b.a.High do
		PrintLn(b.a[j].x * b.a[j].y);
	for var j := a[i].a.Low to a[i].a.High do
		PrintLn(a[i].a[j].x + a[i].a[j].y);
end;
