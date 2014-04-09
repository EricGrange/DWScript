var v := JSON.Parse('');

PrintLn(v.Dummy);
PrintLn(v.TypeName());

PrintLn(v.Clone().TypeName());

PrintLn(v[1]);
PrintLn(v['t'].typeName());

var a := JSON.NewArray;
a[0] := JSON.Parse('');
PrintLn(JSON.Stringify(a));
a[0] := v;
PrintLn(JSON.Stringify(a));

var o := JSON.NewObject;
o.Field := JSON.Parse('');
PrintLn(JSON.Stringify(o));
o.Field := v;
PrintLn(JSON.Stringify(o));

v := JSON.Parse('');

PrintLn(v.Dummy);
PrintLn(v.TypeName());

