procedure Trap;
begin
   while True do begin
      repeat
         PrintLn('');
      until False;
   end;
end;

procedure NoTrap;
begin
   while True do begin
      repeat
         Exit;
      until False;
   end;
end;

while True do ;

while True do begin
   PrintLn('');
end;
while False do begin
   PrintLn('');
end;

while 1=1 do begin
   PrintLn('');
   Break;
end;

repeat
   PrintLn('');
until False;
repeat
   PrintLn('');
until True;

repeat
   PrintLn('');
   Break;
until 1=1;
