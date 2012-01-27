procedure Test; overload; forward;
procedure Test(s : String); overload; forward;
function Test(i : Integer) : String; overload; forward;

procedure Test;
begin
   PrintLn('Hello');
end;

procedure Test(s, s2 : String); overload;
begin
   PrintLn('Hello '+s);
end;

function Test(i : Integer = 0) : String;
begin
   Result:='Hello '+IntToStr(i);
end;
