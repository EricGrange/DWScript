type TKey = record
		x, y : Integer;
	end;
type TElem = record
		a, b, c : String;
		procedure Print; begin PrintLn(a+','+b+','+c) end;
	end;
	
var a : array [TKey] of TElem;

var k : TKey;
var e : TElem = (a: 'a'; b: 'b'; c: 'c');

a[k].Print;

a[k] := e;
a[k].Print;

e.a := 'aa';

a[k].Print;

k.x := 1;
a[k] := e;

a[k].Print;

k.x := 0;
a[k].Print;
