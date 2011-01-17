type
   TBase = class
      procedure Check(i : Integer); virtual;
   end;
type
   TChild = class (TBase)
   end;
type
   TSubChild = class (TBase)
      procedure Check(i : Integer); override;
   end;

procedure TBase.Check(i: Integer);
require
   i > 0;
begin
   PrintLn('base '+IntToStr(i));
ensure
   i < 10;
end;

procedure TSubChild.Check(i: Integer);
begin
   PrintLn('subchild '+IntToStr(i));
ensure
   i < 5 : 'was '+IntToStr(i);
end;

var b = TBase.Create;
PrintLn('TBase');
b.Check(1);
try
   b.Check(-1);
except
   on E: Exception do PrintLn(E.Message);
end;
b.Check(7);
try
   b.Check(10);
except
   on E: Exception do PrintLn(E.Message);
end;

var c = TChild.Create;
PrintLn('TChild');
c.Check(1);
try
   c.Check(-1);
except
   on E: Exception do PrintLn(E.Message);
end;
c.Check(7);
try
   c.Check(10);
except
   on E: Exception do PrintLn(E.Message);
end;

var s = TSubChild.Create;
PrintLn('TSubChild');
s.Check(1);
try
   s.Check(-1);
except
   on E: Exception do PrintLn(E.Message);
end;
s.Check(4);
try
   s.Check(7);
except
   on E: Exception do PrintLn(E.Message);
end;
try
   s.Check(10);
except
   on E: Exception do PrintLn(E.Message);
end;
