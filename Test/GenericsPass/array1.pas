type
	TTest<T> = array of T;

var i : TTest<Integer>;
i.Add(123);
PrintLn(IntToStr(i[0]));

var s : TTest<String>;
s.Add('abc');
s.Add('def');
PrintLn(s.Join(','));
