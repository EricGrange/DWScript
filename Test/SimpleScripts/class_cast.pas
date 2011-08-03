type TBase = class end;
  
type TChild = class (TBase) end;

var o := TObject.Create;
var c := TChild.Create;

PrintLn(TObject(o).ClassName);
PrintLn(TObject(c).ClassName);

PrintLn((o as TObject).ClassName);
PrintLn((c as TObject).ClassName);

try
   PrintLn(TChild(c).ClassName);
   PrintLn(TChild(o).ClassName);
except
   on E: Exception do PrintLn(E.Message);
end;

try
   PrintLn((c as TChild).ClassName);
   PrintLn((o as TChild).ClassName);
except
   on E: Exception do PrintLn(E.Message);
end;
