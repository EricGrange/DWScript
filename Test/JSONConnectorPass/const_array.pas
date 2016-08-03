const a : array [0..1] of record
		a : record x, y : Integer; end;
		b : array[0..1] of String;
	end = [
		(a: (x: 1; y: 2); b: ['a', 'b']),
		(a: (x: 3; y: 4); b: ['c', 'd'])
	];

PrintLn(JSON.Stringify(a));

