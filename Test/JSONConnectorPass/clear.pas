var v : JSONVariant;

PrintLn('Initial:');
PrintLn(VarIsClear(v));
PrintLn(VarIsClear(v.foo));

v := 'abcd';

PrintLn('String:');
PrintLn(VarIsClear(v));
PrintLn(VarIsClear(v.foo));

v := Unassigned;

PrintLn('Unassigned:');
PrintLn(VarIsClear(v));
PrintLn(VarIsClear(v.foo));


