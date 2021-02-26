var a : array of String;


a.Add('hello', 'world');

a.Move(0, 1);
PrintLn(a.Join(','));

a.Move(1, 0);
PrintLn(a.Join(','));

a.Insert(1, 'foo');

a.Move(0, 2);
PrintLn(a.Join(','));

a.Move(2, 0);
PrintLn(a.Join(','));

PrintLn(a.IndexOf('bar'));
PrintLn(a.IndexOf('foo'));

a.Reverse;
PrintLn(a.Join(','));

a.Add(a);
PrintLn(a.Join(','));