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

procedure Test(s : String; i : Integer); overload;
begin
   PrintLn('Hello '+s);
end;

function Test(i : String) : String;
begin
   Result:='Hello '+IntToHex(i, 1);
end;

end.