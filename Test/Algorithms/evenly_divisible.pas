//
// This test answers Project Euler's Problem 5
//
// DO NOT READ IT if you want to participate
//

type TDivs = array [2..19] of Integer;

function DivsOf(n : Integer) : TDivs;
var
   i : Integer;
begin
   i:=2;
   while i<n do begin
      if (n mod i)=0 then begin
         Inc(Result[i]);
         n:=n div i;
      end else Inc(i);
   end;
   if n>1 then
      Inc(Result[n]);
end;

var i, j : Integer;
var m, c : TDivs;

for i:=2 to 20 do begin
   c:=DivsOf(i);
   for j:=2 to 19 do
      m[j]:=MaxInt(m[j], c[j]);
end;
j:=1;
for i:=2 to 19 do begin
   if m[i]=0 then continue;
   PrintLn(Format('%d: %d', [i, m[i]]));
   j*=Round(Power(i, m[i]));
end;
PrintLn(j);

// proof
for i:=2 to 20 do
   if (j mod i)<>0 then
      PrintLn('failed '+IntToStr(i));

