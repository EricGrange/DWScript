type TClassA = class(TObject);
type TClassB = class end;

var a1 : array of function : TClass;

a1.Add(TObject.ClassType);
a1.Add(TClassA.ClassType);
a1.Add(TClassB.ClassType);

var a2 : array of function : String;

a2.Add(TObject.ClassName);
a2.Add(TClassA.ClassName);
a2.Add(TClassB.ClassName);

var i : Integer;

for i:=0 to High(a1) do
  PrintLn(a1[i]().ClassName);

for i:=0 to High(a2) do
  PrintLn(a2[i]);  