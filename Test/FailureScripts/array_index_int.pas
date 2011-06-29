type TEnum = (eZero, eOne, eTwo);

var ai : array [1..10] of Integer;
var v : Variant;

ai[1]:=1;
ai['1']:=1;
ai[True]:=1;
ai[Integer(True)]:=1;
ai[eZero]:=1;
ai[Integer(eZero)]:=1;
ai[v]:=1;