procedure Blank; external;
procedure Ints3(a, b, c: integer); external;

Blank();

var a := 1;
var b := 5;
Ints3(a, b, a + b);
