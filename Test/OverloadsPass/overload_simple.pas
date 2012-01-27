procedure Test; overload;
begin
   PrintLn('Hello');
end;

procedure Test(s : String); overload;
begin
   PrintLn('Hello '+s);
end;

function Test(i : Integer) : String; overload;
begin
   Result:='Hello '+IntToStr(i);
end;

Test;
Test('world');
PrintLn(Test(123));

var sv := 'World';
var iv := 456;

Test(sv);
PrintLn(Test(iv));
