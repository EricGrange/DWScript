var a : array of String;

a.Add('a', 'b', 'c');
a.Reverse;
PrintLn(a.Join(','));

PrintLn(a.Reverse.Join(','));

PrintLn(a.Swap(0,1).Join(','));