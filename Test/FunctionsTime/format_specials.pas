var dt := 42000.42;

PrintLn(FormatDateTime('a yyy', dt));
PrintLn(FormatDateTime('"hh" ''nn''', dt));
PrintLn(FormatDateTime('h""h n''''n!""', dt));
PrintLn(FormatDateTime('DDD MMM HH:NN', dt));