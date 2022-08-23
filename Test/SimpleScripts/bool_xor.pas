var a, b, c : Boolean;
b := True;

for var i := 0 to 1 do begin
   c := a xor (i = 0);
	Print(Ord(c));
   c := b xor (i = 0);
	Print(Ord(c));
   c := (i = 0) xor a;
	Print(Ord(c));
   c := (i = 0) xor b;
	PrintLn(Ord(c));
	
   if a xor (i = 0) then Print(2) else Print(0);
   if b xor (i = 0) then Print(2) else Print(0);
   if (i = 0) xor a then Print(2) else Print(0);
	if (i = 0) xor b then PrintLn(2) else PrintLn(0);
end;
