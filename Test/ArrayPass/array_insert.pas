procedure PrintArray(a : array of Integer);
var
   i : Integer;
begin
   Print(IntToStr(Length(a))+':{');
   for i:=0 to High(a) do begin
      if i>0 then
         Print(',');
      Print(IntToStr(a[i]));
   end;
   PrintLn('}');
end;
   
var a : array of Integer;

a.Insert(0, 10);
a.Insert(1, 20);
PrintArray(a);

a.Insert(0, 0);
PrintArray(a);

a.Insert(2, 15);
PrintArray(a);

try
   a.Insert(-1, 0);
except
   on E : Exception do PrintLn(E.Message);
end;

try
   a.Insert(5, 0);
except
   on E : Exception do PrintLn(E.Message);
end;

PrintArray(a);
