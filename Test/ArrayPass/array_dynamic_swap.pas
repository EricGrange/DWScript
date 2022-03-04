var a : array of Integer;

function MakeArray(n : Integer) : array of Integer;
begin
   Result:=new Integer[n];
   while n>0 do begin
      Dec(n);
      Result[n]:=n;
   end;
end;

procedure PrintArray(a : array of Integer);
begin
   var i : Integer;
   PrintLn(IntToStr(a.Length)+' elements');
   for i:=a.low to a.high do begin
      Print(i);
      Print(': ');
      PrintLn(a[i]);
   end;
end;

a:=MakeArray(0);
a.Reverse;
PrintArray(a);

a:=MakeArray(1);
a.Reverse;
PrintArray(a);
a.Swap(0,0);
PrintArray(a);

a:=MakeArray(2);
a.Reverse;
PrintArray(a);
a.Swap(0,1);
PrintArray(a);

a:=MakeArray(3);
a.Reverse;
PrintArray(a);
a.Swap(0,2);
PrintArray(a);
