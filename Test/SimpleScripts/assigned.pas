type
   TMyObj = class
      Field : Integer;
      procedure Proc;
      procedure ProcVirtual; virtual;
   end;

procedure TMyObj.Proc;
begin
   PrintLn('Proc');
   PrintLn(Field);
end;

procedure TMyObj.ProcVirtual;
begin
   PrintLn('ProcVirtual');
   PrintLn(Field);
end;

var o : TMyObj;

if Assigned(o) then PrintLn('bug');

o:=TMyObj.Create;

if Assigned(o) then PrintLn('Assigned');

o.Field:=1234;
o.Proc;
o.ProcVirtual;

o:=nil;

if not Assigned(o) then PrintLn('not Assigned');

try
   o.Field:=456;
except
   on E : Exception do PrintLn(E.Message);
end;

try
   o.Proc;
except
   on E : Exception do PrintLn(E.Message);
end;

try
   o.ProcVirtual;
except
   on E : Exception do PrintLn(E.Message);
end;

