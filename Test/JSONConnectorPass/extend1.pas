var a := JSON.Parse('{ "a" : 1 }');
var b := JSON.Parse('{ "b" : 2 }');
var c := JSON.Parse('{ "a" : 2 }');

a.Extend(b);
PrintLn(JSON.Stringify(a));
a.Extend(c);
PrintLn(JSON.Stringify(a));