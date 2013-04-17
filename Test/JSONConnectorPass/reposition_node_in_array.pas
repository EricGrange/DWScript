var a:=JSON.NewArray;
var b:=JSON.NewObject;
b.ID:='r';
a[0]:=b;
a[1]:=b;
PrintLn(JSON.Stringify(a));