var s := '<script>alert("xss")</script>';

PrintLn(s.ToXML);

s := 'hello'#9'world';

PrintLn(s.ToXML);

s := 'a'#11'b';
PrintLn(s.ToXML);
PrintLn(s.ToXML(1));
try
	PrintLn(s.ToXML(2));
except
	on E: Exception do
		PrintLn(E.Message);
end;