var v : JSONVariant;

PrintLn('Initial:');
PrintLn(v.Defined());
PrintLn(v.foo.Defined());

v := 'abcd';

PrintLn('String:');
PrintLn(v.Defined());
PrintLn(v.foo.Defined());

v := Unassigned;

PrintLn('Unassigned:');
PrintLn(v.Defined());
PrintLn(v.foo.Defined());


