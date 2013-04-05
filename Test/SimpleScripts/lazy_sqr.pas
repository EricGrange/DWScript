var k : Integer = 1;
var f : Float := 2;

function GlobInc : Integer;
begin
   Result:=Inc(k, 1);
end;

function GlobFloat : Float;
begin
   Result:=f;
	f+=2;
end;

procedure TestInt(lazy i : Integer);
begin
	PrintLn(Sqr(i));
	PrintLn(Sqr(i));
	PrintLn(i*i);
	PrintLn(i*i);
end;

procedure TestFloat(lazy v : Float);
begin
	PrintLn(Sqr(v));
	PrintLn(Sqr(v));
	PrintLn(v*v);
	PrintLn(v*v);
end;

TestInt(GlobInc);
TestFloat(GlobFloat);
