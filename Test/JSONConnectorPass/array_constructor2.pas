
function Test(i : Integer) : String;
begin
   Result := JSON.Stringify(record
      a := [ i ];
      b := [ i, 1 ];
      c := [ 0, i ];
   end);
end;

PrintLn(Test(1));
PrintLn(Test(2));
