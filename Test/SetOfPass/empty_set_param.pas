type TEnum = (A);
type TSet = set of TEnum;

procedure Test(p : TSet = []);
begin
	PrintLn(A in p);
end;

procedure Test2(p : TSet = [A]);
begin
	PrintLn(A in p);
end;

Test;
Test([A]);
Test([]);

Test2;
Test2([A]);
Test2([]);
