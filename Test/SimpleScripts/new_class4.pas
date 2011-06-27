type
   TMyClass = class
   end;

var c : TClass;

try
   new c;
except
   on E: Exception do
      PrintLn(E.Message);
end;

c:=TObject;
PrintLn(new c.ClassName);

c:=TMyClass;
PrintLn(new c.ClassName);