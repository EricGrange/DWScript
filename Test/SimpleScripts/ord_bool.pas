for var i := 123 to 132 do begin
   for var k := 1 to 7 do
      Print(Ord((i and k)=1).ToString);
   PrintLn('');
   for var k := 1 to 7 do
      Print(Ord(not ((i and k)=1)).ToString);
   PrintLn('');
end;

var a := True;
PrintLn(IntToStr(Ord(a xor (not a))));
PrintLn(IntToStr(Ord(a or (not a))));
PrintLn(IntToStr(Ord(a and (not a))));
PrintLn(IntToStr(Ord(a)));
PrintLn(IntToStr(Ord(not a)));

var i := 23;
PrintLn(Ord((i and 16)<>0)*16 + Ord((i and 8)<>0)*8 + Ord((i and 4)<>0)*4 + Ord((i and 2)<>0)*2 + Ord((i and 1)<>0)*1);
PrintLn(Ord((i and 16)=0)*16 + Ord((i and 8)=0)*8 + Ord((i and 4)=0)*4 + Ord((i and 2)=0)*2 + Ord((i and 1)=0)*1);

