procedure Test1;
begin
   repeat 
      PrintLn('Test1')
   until True
end;

function Test2 : Integer;
begin
   Result := 2
end;

Test1;
PrintLn(Test2);