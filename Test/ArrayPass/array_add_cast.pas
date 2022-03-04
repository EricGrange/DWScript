type TTest = class end;

var a: array of TTest;

var o : TObject;

a.Add(TTest(nil));
a.Add(TTest(o));
a.Add((o as TTest));

PrintLn(a.Length);