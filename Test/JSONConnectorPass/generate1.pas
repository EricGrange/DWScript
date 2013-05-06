var a:=JSON.Parse('[]');
a[0]:=JSON.Parse('{}');
a[0].TEST:=3;

PrintLn(JSON.Stringify(a));