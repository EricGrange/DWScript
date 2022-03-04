var ai : array [0..1] of Integer = [789, 123];
var ai2 : array [1..3] of Integer = [12, 34, 56];

PrintLn(Low(ai));
PrintLn(High(ai));
PrintLn(ai[0]);
PrintLn(ai[1]);

PrintLn(Format('%d - %d - %d', [Low(ai2), High(ai2), Length(ai2)]));
PrintLn(ai2[1]);
PrintLn(ai2[2]);
PrintLn(ai2[3]);

