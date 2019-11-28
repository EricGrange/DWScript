var a : array[String] of array[0..1] of String;
for var s in ['titi', 'toto'] do begin
   PrintLn(s);
   a[s] := [s + '0', s + '1'];
end;
PrintLn(a['titi'][0]);
PrintLn(a['toto'][1]); 
