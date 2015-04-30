var v := JSON.NewObject;

PrintLn(v.one??v.two??3);
v.two:=2;
PrintLn(v.one??v.two??3);
v.one:=1;
PrintLn(v.one??v.two??3);

var a := JSON.NewArray;

PrintLn(a[0]??'a');
a.Add('b');
PrintLn(a[0]??'a');

v := 0;
PrintLn(v??'c');
v := 1;
PrintLn(v??'d');

v := '';
PrintLn(v??'e');
v := 'a';
PrintLn(v??'f');