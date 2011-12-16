type
   TMyClass = class
      function Test(a : Integer; var b : Integer) : Integer;
   end;
   
function TMyClass.Test(a : Integer; var b : Integer) : Integer;
begin
   Result:=a;
end;

var c := TMyClass.Create;

PrintLn(c.Test(1));

