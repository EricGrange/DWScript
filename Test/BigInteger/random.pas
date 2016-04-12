//
// New Script
//

const N = 10;
var nb : array [Integer] of Integer;

for var i := 1 to 1000 do begin
   var r := Integer(RandomBigInteger(BigInteger(N)));
   nb[r] := nb[r]+1;
end;

for var i := 0 to N do 
   PrintLn(nb[i] > 50);