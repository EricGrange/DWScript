var a : array of String;
var b : array of String;
b:=['c', 'd'];

PrintLn(StrJoin(a, ','));
a.Add(['a', 'b']);
 
PrintLn(StrJoin(a, ','));
PrintLn(StrJoin(b, ','));
 
var c : array [1..2] of String = ['e', 'f'];
 
a.Add(b, c);
PrintLn(StrJoin(a, ','));

b.Clear;

a.Add(b, []);
PrintLn(StrJoin(a, ','));