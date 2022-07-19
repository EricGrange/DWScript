var f : array of String;

for var k := 1 to 3 do begin
   for var i := 1 to 10 do f.Add(i.ToString);

   f.Delete(7, 3);
   f.Delete(0, 2);

   PrintLn(f.Join(','));
end;
