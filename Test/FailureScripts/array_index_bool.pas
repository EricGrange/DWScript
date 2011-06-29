type TEnum = (eZero, eOne, eTwo);

var ab1 : array [False..True] of Integer;
var ab2 : array [True..False] of Integer;

var v : Variant;

ab1[1]:=1;
ab1['1']:=1;
ab1[True]:=1;
ab1[Integer(True)]:=1;
ab1[eZero]:=1;
ab1[Integer(eZero)]:=1;
ab1[v]:=1;