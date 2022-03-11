var a := JSON.ParseStringArray('["a"]');

PrintLn(a.Length);
PrintLn(a[0]);

a := JSON.ParseStringArray('[ ]');

PrintLn(a.Length);

var b := JSON.ParseFloatArray('[1,2.5]');

PrintLn(b.Length);
PrintLn(b[0]);
PrintLn(b[1]);

var c := JSON.ParseIntegerArray('[1,2,3]');

PrintLn(c.Length);
PrintLn(c[0]);
PrintLn(c[1]);
PrintLn(c[2]);

c := JSON.ParseIntegerArray('[1,null,2]');
PrintLn(JSON.Stringify(c));
c := JSON.ParseIntegerArray('[1,null,2]', -1);
PrintLn(JSON.Stringify(c));