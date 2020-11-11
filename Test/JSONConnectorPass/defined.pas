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

v := JSON.NewObject;
v.a := JSON.NewArray;
v.a.Push(1);

PrintLn('Object with array:');
PrintLn(v.Defined());
PrintLn(v.a.Defined());
PrintLn(v.b.Defined());
PrintLn(v.a[0].Defined());
PrintLn(v.a[1].Defined());
PrintLn(v.a.b.Defined());


