type IIntf = interface procedure Hello; end;

type
   TTest = class (IInterface, IIntf)
      procedure Hello;
      begin
         PrintLn('Hello ' + ClassName);
      end;
   end;
type TTest2 = class (IInterface) end;


var obj1 : TObject := new TTest;
var obj2 : TObject := new TTest2;

(obj1 as IIntf).Hello;
try
   (obj2 as IIntf).Hello;
except
   on E: Exception do
      PrintLn(E.Message);
end;

var i1 : IInterface = obj1 as IInterface;
var i2 : IInterface = obj2 as IInterface;

(i1 as IIntf).Hello;
try
   (i2 as IIntf).Hello;
except
   on E: Exception do
      PrintLn(E.Message);
end;
