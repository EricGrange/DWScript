procedure test(const a : array of const);
begin
	PrintLn(a.low);
	PrintLn(Low(a));
	PrintLn(a.Length);
	PrintLn(Length(a));
	PrintLn(a.High);
	PrintLn(High(a));
end;

test([]);
test([1]);
test(['one', 'two']);