var a : array of String;

try
    a.Move(0, 0);
except
    on E : Exception do PrintLn(E.Message);
end;
a.Add('a');

try
    a.Move(0, 1);
except
    on E : Exception do PrintLn(E.Message);
end;
try
    a.Move(1, 0);
except
    on E : Exception do PrintLn(E.Message);
end;
a.Move(0, 0);
PrintLn(a.Join(','));

a.Add('b');
PrintLn(a.Join(','));
a.Move(0, 1);
PrintLn(a.Join(','));
a.Move(1, 0);
PrintLn(a.Join(','));

a.Add('c');
PrintLn(a.Join(','));  // a,b,c
a.Move(0, 2);
PrintLn(a.Join(','));  // b,c,a
a.Move(2, 0);
PrintLn(a.Join(','));  // a,b,c
a.Move(1, 2);
PrintLn(a.Join(','));  // a,c,b
a.Move(1, 0);
PrintLn(a.Join(','));  // c,a,b
a.Move(2, 1);
PrintLn(a.Join(','));  // c,b,a


