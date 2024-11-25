var a : array of Integer := [ 0, 1, 0, 1 ];

PrintLn(a.IndexOf(1));
PrintLn(a.IndexOf(1, 2));

var f : array of Float := [ 0, 1, 0, 1 ];

PrintLn(f.IndexOf(0));
PrintLn(f.IndexOf(0, 2));

var s : array of String := [ 'a', 'b', 'a', 'b' ];

PrintLn(s.IndexOf('b'));
PrintLn(s.IndexOf('b', 2));

var b : array of Boolean := [ False, True, False, True ];

PrintLn(b.IndexOf(False));
PrintLn(b.IndexOf(False, 2));
