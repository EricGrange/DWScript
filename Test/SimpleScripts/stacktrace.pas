procedure ThisOneBombs;
begin
   raise Exception.Create('boom!');
end;

procedure CallsABomb;
begin
   ThisOneBombs;
end;


try
   raise Exception.Create('booh');
except
   on E: Exception do begin
      PrintLn(E.Message);
      PrintLn(E.StackTrace);
   end;
end;

try
   ThisOneBombs;
except
   on E: Exception do begin
      PrintLn(E.Message);
      PrintLn(E.StackTrace);
   end;
end;

try
   CallsABomb;
except
   on E: Exception do begin
      PrintLn(E.Message);
      PrintLn(E.StackTrace);
   end;
end;

type
   TMyClass = class
      procedure Boom;
   end;

procedure TMyClass.Boom;
begin
   raise Exception.Create('class boom');
end;

try
   var c := TMyClass.Create;
   c.Boom;
except
   on E: Exception do begin
      PrintLn(E.Message);
      PrintLn(E.StackTrace);
   end;
end;
