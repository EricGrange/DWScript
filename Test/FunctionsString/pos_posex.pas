PrintLn(Pos('na', 'banana'));
PrintLn(Pos('na', 'banana', 1));
PrintLn(Pos('na', 'banana', 4));
PrintLn(PosEx('na', 'banana', 1));
PrintLn(PosEx('na', 'banana', 4));
PrintLn(RevPos('na', 'banana'));
PrintLn(RevPos('', 'banana'));
PrintLn(RevPos('z', 'banana'));

PrintLn('---');

var s := 'banana';

PrintLn(Pos('na', s));
PrintLn(Pos('na', s, 1));
PrintLn(Pos('na', s, 4));
PrintLn(PosEx('na', s, 1));
PrintLn(PosEx('na', s, 4));
PrintLn(RevPos('na', s));
PrintLn(RevPos('', s));
PrintLn(RevPos('z', s));
