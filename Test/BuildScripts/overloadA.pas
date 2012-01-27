unit OverloadA;

interface

procedure Test; overload;

implementation

procedure Test;
begin
   PrintLn('Hello From A');
end;

end.
