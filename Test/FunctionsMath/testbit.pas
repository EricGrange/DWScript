var v : Integer;

v := 0b10111010;

for var i := 0 to 7 do
   Print(if v.TestBit(i) then '1' else '0');
PrintLn('');
   
v := -1;
PrintLn(v.TestBit(-1));   
PrintLn(v.TestBit(0));
{$ifdef JS_CODEGEN}
PrintLn(True); // only 32 bits in JS
{$else}
PrintLn(v.TestBit(63));
{$endif}
PrintLn(v.TestBit(64));

v := 1;
PrintLn(v.TestBit(-1));   
PrintLn(v.TestBit(0));
PrintLn(v.TestBit(63));
PrintLn(v.TestBit(64));
