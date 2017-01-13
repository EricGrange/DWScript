var a : ComVariant;

a := 1;
PrintLn(VarIsClear(a));
PrintLn(VarIsArray(a));
PrintLn(VarType(a));

VarClear(a);
PrintLn(VarIsClear(a));
PrintLn(VarIsArray(a));
PrintLn(VarType(a));

var v : ComVariantArray;
a := v;
PrintLn(VarIsClear(a));
PrintLn(VarIsArray(a));
PrintLn(VarType(a));

