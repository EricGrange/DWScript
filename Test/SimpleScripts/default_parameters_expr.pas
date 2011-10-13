procedure Test1(a : Float = 1);
begin
   PrintLn(a);
end;

procedure Test2(a : Integer = 1+1);
begin
   PrintLn(a);
end;

procedure Test3(a : Float = StrToInt("123"));
begin
   PrintLn(a);
end;

Test1;
Test2;
Test3;

Test1(10.5);
Test1(10);
Test1(11.5);