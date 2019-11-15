var a : array [String] of String;

a['foo'] := 'bar';

var v : Variant = 'foo';

PrintLn(a[v]);

v := 123;
a[v] := 'bar2';

PrintLn(a[v]);
PrintLn(a['123']);