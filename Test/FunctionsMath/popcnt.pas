var i : Integer;

for i := 0 to 5 do
   PrintLn(i.PopCount);
   
{$ifdef JS_CODEGEN}

// only 54 bits in JS, with only 32bits in unsigned

i := 1234567890123456;
PrintLn(i.PopCount + 3);  

PrintLn(PopCount(Unsigned32(-1)) + 32);

{$else}   

i := 1234567890123456789;
PrintLn(i.PopCount);
PrintLn(PopCount(-1));

{$endif}

