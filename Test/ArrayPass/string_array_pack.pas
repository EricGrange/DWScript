var a : array of String;

a.Pack;
PrintLn(a.Join(','));

a.Add('', 'abc');
a.Pack;
PrintLn(a.Join(','));

a.Add('', '', 'def', '');
a.Pack;
PrintLn(a.Join(','));
