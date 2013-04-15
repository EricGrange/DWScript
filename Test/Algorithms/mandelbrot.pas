var i, j, newColor : Integer;
var rU, rV, rX, rY, rZ : Float;

const cSize = 80;
const cAscii = '0123456789.';

for i := 0 to cSize-2 do begin
   for j := 0 to cSize-2 do begin
      rX := -0.8 + 3 * i / cSize;
      rY := -1.4 + 2.8 * j / cSize;
      newColor := 1;
      rU := 0;
      rV := 0;
      repeat
         rZ := Sqr(rU) - Sqr(rV) - rX;
         rV := 2 * rU * rV - rY;
         rU := rZ;
         newColor := newColor + 1;
      until (Sqr(rU) + Sqr(rV) > 9) or (newColor = 11);
      Print(cAscii[newColor]);
   end;
   PrintLn('');
end;

