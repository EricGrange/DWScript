type TXY = Integer;

var astr : array [TXY] of String;
var istr : array [TXY] of Integer;
var fstr : array [TXY] of Float;
var bstr : array [TXY] of Boolean;

var xy1 := 12;
var xy2 := 23;

astr[xy2] := 'hello';
PrintLn(astr[xy1]);
PrintLn(astr[xy2]);

istr[xy2] := 123;
PrintLn(istr[xy1]);
PrintLn(istr[xy2]);

fstr[xy2] := 1.5;
PrintLn(fstr[xy1]);
PrintLn(fstr[xy2]);

bstr[xy2] := True;
PrintLn(bstr[xy1]);
PrintLn(bstr[xy2]);

