var s := 'a';

PrintLn(s in ['a', 'b', 'c']);
PrintLn(s in ['b', 'c', 'a']);
PrintLn(s in ['c', 'a']);

PrintLn(s in ['d', 'b', 'c']);
PrintLn(s in ['b', 'c', 'd']);
PrintLn(s in ['c', 'd']);
PrintLn(s in ['c']);

PrintLn(s in ['a'..'b']);
PrintLn(s in ['0'..'z']);
PrintLn(s in ['c'..'d']);
PrintLn(s in ['c'..'s', 'a']);
PrintLn(s in ['c'..'s', 'z']);