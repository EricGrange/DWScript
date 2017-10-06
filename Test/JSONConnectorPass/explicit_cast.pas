var j := JSON.NewObject;

PrintLn(Boolean(j));
PrintLn(String(j));
try PrintLn(Float(j)); except on E : Exception do PrintLn(E.Message) end;
try PrintLn(Integer(j)); except on E : Exception do PrintLn(E.Message) end;

j := 1.25;

PrintLn(Boolean(j));
PrintLn(String(j));
PrintLn(Float(j));
PrintLn(Integer(j));

j := '';

PrintLn(Boolean(j));
PrintLn(String(j));
PrintLn(Float(j));
PrintLn(Integer(j));

j := '1.25';

PrintLn(Boolean(j));
PrintLn(String(j));
PrintLn(Float(j));
PrintLn(Integer(j));
