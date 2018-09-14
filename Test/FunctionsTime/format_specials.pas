var dt := EncodeDate(2014, 12, 13) + EncodeTime(10, 4, 0, 0);

PrintLn(FormatDateTime('a yyy', dt));
PrintLn(FormatDateTime('"hh" ''nn''', dt));
PrintLn(FormatDateTime('h""h n''''n!""', dt));
PrintLn(FormatDateTime('DDD MMM HH:NN', dt));