type
   TMyClass = class
      constructor Create;
   end;

constructor TMyClass.Create;
begin
   raise Exception.Create('boom');
end;

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