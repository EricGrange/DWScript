var a := JSON.NewArray;
var b := JSON.NewObject;
a.Add(b);

var a2 := JSON.NewArray;

PrintLn(JSON.Stringify(a));
PrintLn(JSON.Stringify(a2));

a2.Add(a[0]);

PrintLn(JSON.Stringify(a));
PrintLn(JSON.Stringify(a2));

b.test := '1';

PrintLn(JSON.Stringify(a));
PrintLn(JSON.Stringify(a2));

