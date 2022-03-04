var a : array of String;
var b : array of String;
b:=['c', 'd'];

PrintLn(StrJoin(a, ','));
 a.Add('a', 'b');
 
PrintLn(StrJoin(a, ','));
PrintLn(StrJoin(b, ','));
 
a.Add(b, 'e', 'f');
PrintLn(StrJoin(a, ','));

b.Clear;

a.Add(b, a);
PrintLn(StrJoin(a, ','));