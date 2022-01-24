type TStaticArray = array [0..2] of Integer;
type TRecord = record S : String; F : Float; end;

var sa : array [Integer] of TStaticArray;
var ra : array [Integer] of TRecord;

sa[1][1] := 123;
sa[0][2] := 456;
sa[2][0] := 789;

for var i := 0 to 2 do begin
   for var j := 0 to 2 do
      Print(sa[i][j]);
   PrintLn('');
end;

ra[2].S := 'hello';
ra[2].F := 1.5;
ra[3].S := 'world';
ra[3].F := 0.5;

for var i := 1 to 3 do begin
   PrintLn(ra[i].S);
   PrintLn(ra[i].F);
end;
