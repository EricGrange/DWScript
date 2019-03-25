
function Test(i : Integer) : String;
begin
   Result := JSON.Stringify([ i, 1 ]);
end;

PrintLn(Test(1));
PrintLn(Test(2));
