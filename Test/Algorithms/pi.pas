const NumDigits = 50;

var
  A: array [0..10*NumDigits div 3] of Integer;
var
  I, J, K, P, Q, X, Nines, Predigit: Integer;

var result : String;

for I := Low(A) to High(A) do
   A[I] := 2;
Nines := 0;
Predigit := 0;
for J := 0 to NumDigits-1 do begin
   Q := 0;
   P := 2 * High(A) + 1;
   for I := High(A) downto Low(A) do begin
      X := 10*A[I] + Q*(I+1);
      A[I] := X mod P;
      Q := X div P;
      P := P - 2;
   end;
   A[Low(A)] := Q mod 10;
   Q := Q div 10;
   if Q = 9 then
      Inc(Nines)
   else if Q = 10 then begin
      Print(Chr(Predigit + 1 + Ord('0')));
      for K := 1 to Nines do
         Print('0');
      Predigit := 0;
      Nines := 0;
   end else begin
      Print(Chr(Predigit + Ord('0')));
      Predigit := Q;
      for K := 1 to Nines do
         Print('9');
      Nines := 0;
   end;
end;
PrintLn(Chr(Predigit + Ord('0')));
