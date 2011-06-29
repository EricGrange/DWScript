type TEnum = (eZero, eOne, eTwo);

var ae : array [eZero..eTwo] of Integer;

var v : Variant;

ae[1]:=1;
ae['1']:=1;
ae[True]:=1;
ae[Integer(True)]:=1;
ae[eZero]:=1;
ae[Integer(eZero)]:=1;
ae[v]:=1;