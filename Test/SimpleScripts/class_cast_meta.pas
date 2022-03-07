type TTest1 = class end;
type TTest2 = class end;

type TTest1Class = class of TTest1;
type TTest2Class = class of TTest2;

var obj : TObject := new TTest1;

PrintLn((obj.ClassType as TTest1Class).ClassName);
try
   PrintLn((obj.ClassType as TTest2Class).ClassName);
except
   on E: Exception do
      PrintLn(E.Message);
end;