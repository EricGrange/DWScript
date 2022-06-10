var a : array of TObject = [nil];

PrintLn(a.Length);
PrintLn(if a[0] = nil then 'nil');

var s := [nil, TObject.Create];

PrintLn(s.Length);
PrintLn(if s[0] = nil then 'nil');
PrintLn(s[1].ClassName);
