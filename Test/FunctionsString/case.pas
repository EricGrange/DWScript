var s := 'Hello éric!';

PrintLn(ASCIIUpperCase(s));
PrintLn(AnsiUpperCase(s));

PrintLn(ASCIILowerCase(s));
PrintLn(AnsiLowerCase(s));

PrintLn(s.UpperCase);
PrintLn(s.ToUpper);

PrintLn(s.LowerCase);
PrintLn(s.ToLower);
