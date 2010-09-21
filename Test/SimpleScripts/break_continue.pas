var i : Integer;

PrintLn('Upward for');
for i:=1 to 10 do begin
   if (i and 1)=0 then continue;
   if i>7 then break;
   PrintLn(i);
end;

PrintLn('Downward for');
for i:=10 downto 1 do begin
   if (i and 1)=0 then continue;
   if i<4 then break;
   PrintLn(i);
end;

PrintLn('While');
i:=1;
while i<=10 do begin
   if (i and 1)=0 then begin
      i:=i+1;
      continue;
   end;
   if i>7 then break;
   PrintLn(i);
   i:=i+1;
end;

PrintLn('Repeat');
i:=1;
repeat
   if (i and 1)=0 then begin
      i:=i+1;
      continue;
   end;
   if i>7 then break;
   PrintLn(i);
   i:=i+1;
until i>10;
