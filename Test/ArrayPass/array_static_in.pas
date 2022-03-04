var aInt : array [2..4] of Integer;
aInt := [3, 6, 9];
for var i := 1 to 10 do
    Print(Ord(i in aInt));
PrintLn('');
aInt[3] := 4;  
for var i := 1 to 10 do
    Print(Ord(i in aInt));
PrintLn('');    

var aStr := [ 'Minus', 'Zero', 'Plus' ];
Print('' in aStr);
Print('Minus' in aStr);
Print('Zero' in aStr);
Print('Plus' in aStr);
PrintLn('bug' in aStr);

type TRec = record a, b : Integer end;
var aRec : array [1..2] of TRec = (
    (a:1; b:2), (a:2; b:1)
    );
var r : TRec;
Print(r in aRec);
Print(aRec[1] in aRec);
PrintLn(aRec[2] in aRec);
r := aRec[2];
Print(aRec[2] in aRec);
aRec[2].a := 9;
Print(aRec[2] in aRec);
PrintLn(r in aRec);


