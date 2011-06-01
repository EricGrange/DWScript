var i, lo, hi : Integer;

lo:=1;
hi:=3;
for i:=lo to hi do begin
   PrintLn(i);
   hi:=1;
   lo:=3;
end;

lo:=1;
hi:=3;
for i:=hi downto lo do begin
   PrintLn(i);
   hi:=1;
   lo:=3;
end;