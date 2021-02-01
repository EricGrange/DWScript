procedure Blank; external;
procedure Ints3(a, b, c: integer); external;
procedure TestString(a: integer; b: string); external;
procedure TestStringExc(a: integer; b: string); external;
procedure TestBool(a: integer; b: boolean); external;
procedure TestStack (a, b, c, d: integer); external;
procedure TestFloat(a: integer; b: float); external;
procedure TestObject(a: integer; b: TBoxedString); external;
procedure TestObjectExc(a: integer; b: TBoxedString); external;
function  TestReturnInt(a, b: integer): integer; external;
function  TestReturnObject: TBoxedString; external;

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

TestStack(a, b, 12, -57);

TestFloat(1, 0.5);

var box := TestReturnObject;
TestObject(1, box);
try
  TestObjectExc(1, box);
except
end;

var intReturn := TestReturnInt(a, b);
if intReturn <> 6 then
   raise Exception.Create(format('Expected TestReturnInt to return 6 but got %d', [intReturn]));
