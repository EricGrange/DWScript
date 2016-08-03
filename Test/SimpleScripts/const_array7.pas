const a : array [0..1] of record
		a : record x, y : Integer; end;
		b : array[0..1] of String;
	end = [
		(a: (x: 1; y: 2); b: ['a', 'b']),
		(a: (x: 3; y: 4); b: ['c', 'd'])
	];

for var i := a.Low to a.High do begin

	var aa := a[i].a;
	var bb := a[i].b;
	
	PrintLn(aa.x.ToString + ',' + aa.y.ToString);
	PrintLn(bb[0] + ',' + bb[1]);

end;

var c := a;

for var i := c.Low to c.High do begin

	var aa := c[i].a;
	var bb := c[i].b;
	
	PrintLn(aa.x.ToString + ',' + aa.y.ToString);
	PrintLn(bb[0] + ',' + bb[1]);

end;
