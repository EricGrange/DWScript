var a : array of TObject = [nil];

PrintLn(a.Length);
PrintLn(a[0]);

var s := [nil, TObject.Create];

PrintLn(s.Length);
PrintLn(s[0]);
PrintLn(s[1].ClassName);
