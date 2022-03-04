var astr : array [0..1] of String = ['a', 'b'];
var af1 : array [1..3] of Float = [12, 3.4, 56];
var af2 : array [2..3] of Float = [123, 456];

PrintLn(Format('%d - %d - %d', [Low(astr), High(astr), Length(astr)]));
PrintLn(astr[0]);
PrintLn(astr[1]);

PrintLn(Format('%d - %d - %d', [Low(af1), High(af1), Length(af1)]));
PrintLn(af1[1]);
PrintLn(af1[2]);
PrintLn(af1[3]);

PrintLn(Format('%d - %d - %d', [Low(af2), High(af2), Length(af2)]));
PrintLn(af2[2]);
PrintLn(af2[3]);

af2 := [7.8, 90];

PrintLn(Format('%d - %d - %d', [Low(af2), High(af2), Length(af2)]));
PrintLn(af2[2]);
PrintLn(af2[3]);


