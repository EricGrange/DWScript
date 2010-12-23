var v : Variant;

PrintLn(VarIsEmpty(v));
PrintLn(VarIsNull(v));

v:=12;

PrintLn(VarIsEmpty(v));
PrintLn(VarIsNull(v));

VarClear(v);

PrintLn(VarIsEmpty(v));
PrintLn(VarIsNull(v));
