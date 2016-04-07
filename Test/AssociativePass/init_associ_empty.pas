procedure Test;
begin
   var a: array [String] of Integer;
   
   for var i := 1 to 100 do begin
      var str := i.ToString;
      if a[str] > 0 then
         PrintLn('bug')
      else a[str] := i;
   end;
   
   for var i := 1 to 100 do begin
      var str := i.ToString;
      if a[str] <> i then
         PrintLn('bug')
      else a[str] := i;
   end;
end;

for var i := 1 to 5 do begin
   PrintLn(i.ToString + '-----------');
   Test;
end;