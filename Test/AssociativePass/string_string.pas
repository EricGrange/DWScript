var a : array [String] of String;

PrintLn(a.Length);

a['one'] := 'un';
a['two'] := 'deux';
a['three'] := 'trois';

PrintLn(a.Length);

PrintLn(a['one']);
PrintLn(a['two']);
PrintLn(a['three']);
PrintLn(a['four']);

a['one'] := 'uno';
a['four'] := '44';

PrintLn(a['one']);
PrintLn(a['four']);

PrintLn(a.Length);

a.Clear;

PrintLn(a.Length);