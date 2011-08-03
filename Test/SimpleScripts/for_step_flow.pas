var i, j : Integer;

for i:=1 to 11 step 2 do begin
   PrintLn(i);
   if i=3 then continue;
   for j:=i*30 downto i*10 step 10 do begin
      if (j mod 50)=0 then continue;
      PrintLn(j);
      if j<=i*20 then Break;
   end;
   if i=9 then Break;
end;
