var v : JSONVariant;

PrintLn(v.Dummy);
PrintLn(v.TypeName());

PrintLn(v.Clone().TypeName());

PrintLn(v[1]);
PrintLn(v['t'].typeName());

var a := JSON.NewArray;
a[0] := Null;
PrintLn(JSON.Stringify(a));
a[0] := v;
PrintLn(JSON.Stringify(a));

var o := JSON.NewObject;
o.Field := Null;
PrintLn(JSON.Stringify(o));
o.Field := v;
PrintLn(JSON.Stringify(o));

v := Null;

PrintLn(v.Dummy);
PrintLn(v.TypeName());

