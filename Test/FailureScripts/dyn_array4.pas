Type TStrings = Array Of String;

TStrings.Add('1', '2', '3', '4', '5');
PrintLn(TStrings.Low);
PrintLn(TStrings.High);

Type TStrings2 = Array [1..2] Of String;

PrintLn(TStrings2.Low);
PrintLn(TStrings2.High);
TStrings2.Add('1', '2', '3', '4', '5');

