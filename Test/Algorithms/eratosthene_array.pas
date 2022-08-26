var sieve : array of Boolean;

const cLast = 1000;
sieve.SetLength(cLast + 1);

for var i := 2 to cLast do begin
   if sieve[i] then continue;
   var j := i + i;
   while j <= cLast do begin
      sieve[j] := True;
      j += i;
   end;
end;

var n := 3;
for var i := cLast downto 0 do begin
   if not sieve[i] then begin
      PrintLn(i);
      Dec(n);
      if n <= 0 then break;
   end;
end;
