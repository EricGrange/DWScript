var s := '';
var f := 0.5;

PrintLn(TryStrToFloat(s, f));
PrintLn(f);

s := '1.5';
PrintLn(TryStrToFloat(s, f));
PrintLn(f);

s := 'booo';
PrintLn(TryStrToFloat(s, f));
PrintLn(f);

s := '.5';
PrintLn(TryStrToFloat(s, f));
PrintLn(f);