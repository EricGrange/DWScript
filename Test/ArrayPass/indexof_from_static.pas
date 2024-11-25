var a : array [0..3] of Integer = [ 0, 1, 0, 1 ];

PrintLn(a.IndexOf(1, 0));
PrintLn(a.IndexOf(1, 2));
PrintLn(a.IndexOf(1, 5));
PrintLn(a.IndexOf(1, -5));