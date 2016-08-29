var a : array of String;

for var i := 0 to 199 do begin
   var n := Nonces.Generate(30000);
   if n in a then PrintLn('bug '+n);
	a.Add(n);
end;

for var i := 0 to a.High do begin
   if Odd(i) then
      Nonces.Remove(a[i])
   else if not Nonces.CheckAndRemove(a[i]) then
      PrintLn('failed check and keep '+i.ToString);
   if (i and 15) = 0 then
      Nonces.Collect;
   for var j := 0 to a.High do begin
      if j <= i then begin
         if Nonces.CheckAndKeep(a[j]) then
            PrintLn('failed check removal ' + j.ToString);
      end else begin
         if not Nonces.CheckAndKeep(a[j]) then
            PrintLn('failed existence ' + j.ToString);
      end;
   end;
end;

Nonces.Collect;

PrintLn('done');