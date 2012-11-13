var v := JSON.Parse('{"hello":"world"}');

PrintLn(v.TypeName());
PrintLn(v.hello.TypeName());
PrintLn(v.hello);

PrintLn('missing');
var v2 := v.missing;
PrintLn(v2.TypeName());
PrintLn(v2.sub.TypeName());
PrintLn(v2.sub);

PrintLn("['missing']");
v2 := v['missing'];
PrintLn(v2.TypeName());
PrintLn(v2.sub.TypeName());
PrintLn(v2.sub);

PrintLn('[5]');
v2 := v[5];
PrintLn(v2.TypeName());
PrintLn(v2.sub.TypeName());
PrintLn(v2.sub);

PrintLn('ElementName');
PrintLn(v.ElementName(5));
PrintLn(v2.ElementName(5));

PrintLn('Length/Low/High');
PrintLn(v2.Length());
PrintLn(v2.Low());
PrintLn(v2.High());
