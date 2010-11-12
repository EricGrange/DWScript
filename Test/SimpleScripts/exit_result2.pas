function MyFunc(i : Integer) : Integer;
begin
   if i<=0 then
      exit -1
   else begin
      Result:=i;
      if i<10 then
         exit
      else if i=10 then
         exit i+1
      else if i>10 then
         exit Result+2
   end;
end;

PrintLn(MyFunc(0));
PrintLn(MyFunc(5));
PrintLn(MyFunc(10));
PrintLn(MyFunc(11));
