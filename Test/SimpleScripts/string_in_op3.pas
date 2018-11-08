function Test(s : String) : Boolean;
begin 
    Result := s in ['a'..'z', '0'..'9', '-']; 
end;

PrintLn(Test(''));
PrintLn(Test('a'));
PrintLn(Test('alpha'));
PrintLn(Test('B'));
PrintLn(Test('Beta'));
PrintLn(Test('5'));
PrintLn(Test('12345'));
PrintLn(Test('$'));
PrintLn(Test('_'));
PrintLn(Test('---'));
PrintLn(Test('-'));