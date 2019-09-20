var i : Integer;

for i := 0 to 5 do
   PrintLn(i.PopCount);
   
i := 1234567890123456789;
PrintLn(i.PopCount);

PrintLn((-1).PopCount);