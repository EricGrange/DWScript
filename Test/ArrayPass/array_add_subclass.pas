type TTest = class end;
type TTestClass = class of TTest;

var c := TTestClass;

procedure Test(a : array of TTest);
begin
   a.Add(TTest.Create);
   a.Add(c.Create);
end;

var a : array of TObject;
a.Add(TTest.Create);
a.Add(c.Create);
PrintLn(a[0].ClassName);
PrintLn(a[1].ClassName);

var t : array of TTest;
Test(t);
PrintLn(t[0].ClassName);
PrintLn(t[1].ClassName);
