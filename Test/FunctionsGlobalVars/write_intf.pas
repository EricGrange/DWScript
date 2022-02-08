CleanupGlobalVars;

type TTest = class (IInterface) end;

var i := TTest(nil) as IInterface;
WriteGlobalVar('intf', Variant(i));
PrintLn(ReadGlobalVar('intf'));

i := new TTest as IInterface;
try
   WriteGlobalVar('intf', Variant(i));
except
   on E : Exception do
      PrintLn(E.Message);
end;

