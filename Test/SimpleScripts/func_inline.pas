function Next(i : Integer) : Integer; inline;
begin
	Result:=i+1;
end;

PrintLn(Next(1));

var i:=2;

PrintLn(Next(i));