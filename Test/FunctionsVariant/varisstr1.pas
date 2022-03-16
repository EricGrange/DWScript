PrintLn(VarIsStr('hello'));
PrintLn(VarIsStr(123));

var v : Variant;

PrintLn(VarIsStr(v));
v := 'world';
PrintLn(VarIsStr(v));
v := 123;
PrintLn(VarIsStr(v));
