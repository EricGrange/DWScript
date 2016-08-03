function Func1(const a : Integer) : Integer;
begin
	function Func2(const b : Integer) : Integer;
	begin
		Result := a + b;
	end;
	Result := a + Func2( 10*a ) + Func2(100);
end;

PrintLn(Func1(1));