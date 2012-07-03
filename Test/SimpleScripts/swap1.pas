var i1 := 1;
var i2 := 2;

Print(i1);
PrintLn(i2);
Swap(i1, i2);
Print(i1);
PrintLn(i2);

type TTest = class end;
var o1 := TObject.Create;
var o2 : TObject = TTest.Create;

PrintLn(o1.ClassName);
PrintLn(o2.ClassName);
Swap(o1, o2);
PrintLn(o1.ClassName);
PrintLn(o2.ClassName);

