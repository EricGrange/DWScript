procedure Test;
begin
   var a: array of Integer;
   
   if a.Length <> 0 then PrintLn('bug');
   
   for var i := 1 to 10 do a.Add(i);
end;

for var i := 1 to 5 do begin
   PrintLn(i.ToString + '-----------');
   Test;
end;