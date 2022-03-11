type TXY = array [0..1] of Integer;
var a : array of TXY;

const cXY01 = [0,1];
const cXY10 = [1,0];
const cXY11 = [1,1];

var b : array [TXY] of String;

a.Add(cXY01, cXY10, cXY11);
b[cXY01] := 'one';
b[cXY10] := 'two';

procedure PrintNum(var n : TXY);
begin
   PrintLn(b[n]);
end;

for var i := 2 downto 0 do begin
   var j := a[i];
   PrintNum(j);
   PrintNum(a[i]);
end;