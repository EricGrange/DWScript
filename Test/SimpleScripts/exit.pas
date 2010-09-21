function Test1 : Integer;
begin
   Result:=1;
   Exit;
   Result:=2;
end;

function Test2 : Integer;
var i : Integer;
begin
   for i:=1 to 10 do begin
      Result:=i;
      if i=2 then Exit;
   end;
end;

PrintLn(Test1);
PrintLn(Test2);
Exit;
PrintLn('Not printed');