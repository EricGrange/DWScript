
var a : array [Integer] of String;

a[1] := 'one';
a[10] := 'ten';

PrintLn(a[1]);

PrintLn(a[2]);

var s : String = a[10];
PrintLn(s);

a := nil;

PrintLn(Length(a[1]));

