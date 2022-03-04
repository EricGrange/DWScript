var a : array of Integer;

var b := a;

a.Add(1);

PrintLn(a.Length);
PrintLn(b.Length);

a := nil;

PrintLn(a.Length);
PrintLn(b.Length);
