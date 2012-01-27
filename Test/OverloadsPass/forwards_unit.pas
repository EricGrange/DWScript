unit  test;

interface

procedure Test; overload;
procedure Test(s : String); overload;
function Test(i : Integer) : String; overload;

implementation

procedure Test;
begin
   PrintLn('Hello');
end;

procedure Test(s : String); overload;
begin
   PrintLn('Hello '+s);
end;

function Test(i : Integer) : String;
begin
   Result:='Hello '+IntToStr(i);
end;

end.