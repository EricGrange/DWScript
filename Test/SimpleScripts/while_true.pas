var i : Integer;

repeat
   while True do begin
      Inc(i);
      if i=2 then continue;
      PrintLn(i);
      if i=3 then Break;
   end;
until True;

i:=0;

while True do begin
   repeat
      Inc(i);
      if i=2 then continue;
      PrintLn(i);
      if i=3 then Break;
   until False;
   Break;
end;

