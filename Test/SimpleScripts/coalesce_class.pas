type TTest = class end;
type TSub = class(TTest) end;

var t : TTest;
PrintLn((t ?? TSub.Create).ClassName);
t := new TTest;
PrintLn((t ?? TSub.Create).ClassName);

var o : TObject;
PrintLn((o ?? TTest.Create).ClassName);
o := new TTest;
PrintLn((o ?? TObject.Create).ClassName);
o := nil;
PrintLn((o ?? TObject.Create).ClassName);

PrintLn((o ?? o ?? t).ClassName);
PrintLn((o ?? t ?? o).ClassName);
