procedure Blank; external;
procedure Ints3(a, b, c: integer); external;
procedure TestString(a: integer; b: string); external;
procedure TestStringExc(a: integer; b: string); external;
procedure TestBool(a: integer; b: boolean); external;

Blank();

var a := 1;
var b := 5;
Ints3(a, b, a + b);
TestString(b, 'Testing');

try
   testStringExc(b, 'Testing');
except
end;

TestBool(b, true);
