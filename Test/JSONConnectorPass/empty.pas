var v : JSONVariant;

PrintLn('Initial:');
PrintLn(VarIsEmpty(v));
PrintLn(VarIsEmpty(v.foo));

v := 'abcd';

PrintLn('String:');
PrintLn(VarIsEmpty(v));
PrintLn(VarIsEmpty(v.foo));

v := Unassigned;

PrintLn('Unassigned:');
PrintLn(VarIsEmpty(v));
PrintLn(VarIsEmpty(v.foo));


