var v : JSONVariant;

v := "hello";

PrintLn(v.length);

v := JSON.Parse('[1, 2]');

PrintLn(v.length);

v := JSON.NewObject;

v.length := 'world';

PrintLn(v.length);

