type TMyClass = class
   function MyFunc(i : Integer) : Integer;
end;

function TMyClass.MyFunc(i : Integer) : Integer;
begin
   if i<=0 then
      Exit(-1)
   else begin
      Result:=i;
      if i=10 then
         Exit(i+1)
      else if i>10 then
         Exit(Result+2)
   end;
end;

var o : TMyClass = TMyClass.Create;
PrintLn(o.MyFunc(0));
PrintLn(o.MyFunc(5));
PrintLn(o.MyFunc(10));
PrintLn(o.MyFunc(11));
