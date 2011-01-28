type
   TMyObj = class
      Field : Integer;
   end;

var o : TMyObj;

PrintLn('Free');
o:=TMyObj.Create;
o.Free;
try
   o.Free;
except
   on E: Exception do PrintLn(E.Message);
end;
try
   PrintLn(o.Field);
except
   on E: Exception do PrintLn(E.Message);
end;
try
   PrintLn(o.ClassName);
except
   on E: Exception do PrintLn(E.Message);
end;

PrintLn('Destroy');
o:=TMyObj.Create;
o.Destroy;
try
   o.Destroy;
except
   on E: Exception do PrintLn(E.Message);
end;
try
   PrintLn(o.Field);
except
   on E: Exception do PrintLn(E.Message);
end;
try
   PrintLn(o.ClassName);
except
   on E: Exception do PrintLn(E.Message);
end;

PrintLn('nil');
o:=TMyObj.Create;
o:=nil;
o.Free;
try
   PrintLn(o.Field);
except
   on E: Exception do PrintLn(E.Message);
end;
try
   PrintLn(o.ClassName);
except
   on E: Exception do PrintLn(E.Message);
end;
