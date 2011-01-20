type
   TMyClass = class (TExposedBoomClass)
      constructor Create;
   end;

constructor TMyClass.Create;
begin
   inherited Create;
   raise Exception.Create('boom');
end;

PrintLn('Direct');

try
   TExposedBoomClass.Create;
except
   on E: Exception do PrintLn(E.Message);
end;

try
   var v := TExposedBoomClass.Create;
except
   on E: Exception do PrintLn(E.Message);
end;

var vob : TExposedBoomClass;
try
   vob := TExposedBoomClass.Create;
except
   on E: Exception do PrintLn(E.Message);
end;

PrintLn('SubClass');

try
   TMyClass.Create;
except
   on E: Exception do PrintLn(E.Message);
end;

try
   var v := TMyClass.Create;
except
   on E: Exception do PrintLn(E.Message);
end;

var vo : TMyClass;
try
   vo := TMyClass.Create;
except
   on E: Exception do PrintLn(E.Message);
end;