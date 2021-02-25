procedure Test;
begin
   var a : ComVariantArray;
   a.High := 2;
   for var i := 0 to a.High do a[i] := 'toto' + i.ToString;
   PrintLn(a[0]);
   PrintLn(a[1]);
   PrintLn(a[2]);
   PrintLn(a[3]);
end;

try
   Test;
except
   on e: exception do PrintLn(e.Message);
end;