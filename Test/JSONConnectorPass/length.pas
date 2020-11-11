var v := JSON.Parse('["hello","world"]');

PrintLn(v.Length());
PrintLn(v[0]);
PrintLn(v[99].TypeName());

PrintLn(v.High());
