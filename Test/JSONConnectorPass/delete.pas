var v := JSON.NewObject;

v.a := 1;
v.b := 2;
v.c := 3;

PrintLn(v.ToString());

v.Delete('a');

PrintLn(v.ToString());

v.Delete('c');

PrintLn(v.ToString());

v.Delete('foobar');

PrintLn(v.ToString());

v.Delete('b');

PrintLn(v.ToString());

v.Delete('b');

PrintLn(v.ToString());