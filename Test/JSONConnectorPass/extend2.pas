var a := JSON.Parse('[1]');
var b := JSON.Parse('[ "a", { "b" : 2 }, [ 3 ] ]');

PrintLn(a);
PrintLn(b);

a.Extend(b);

PrintLn(a);
PrintLn(b);

a.AddFrom(b);

PrintLn(a);
PrintLn(b);
