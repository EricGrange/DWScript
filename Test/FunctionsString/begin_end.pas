PrintLn(StrBeginsWith('banane', 'b'));
PrintLn(StrBeginsWith('banane', 'a'));
PrintLn(StrBeginsWith('banane', 'ba'));
PrintLn(StrBeginsWith('banane', 'aba'));
PrintLn(StrBeginsWith('aba', 'banane'));

var v := 'babar';

PrintLn(StrBeginsWith('baba', v));
PrintLn(StrBeginsWith('babaria', v));

PrintLn(StrBeginsWith(v, 'b'));
PrintLn(StrBeginsWith(v, ''));

PrintLn(StrEndsWith('banane', 'ana'));
PrintLn(StrEndsWith('banane', 'ane'));
PrintLn(StrEndsWith('aba', 'banane'));

PrintLn(v.StartsWith('b'));
PrintLn(v.StartsWith('z'));
PrintLn(v.EndsWith('b'));
PrintLn(v.EndsWith('r'));