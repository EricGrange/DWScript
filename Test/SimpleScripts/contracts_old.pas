function IncFunc(val, delta : Integer) : Integer;
begin
   Result := val + delta;
ensure
   Result = old val + 1;
end;

procedure IncProc(var val : Integer; delta : Integer);
begin
   val += delta;
ensure
   val = old val + 1;
end;

var v : Integer = 1;
v:=IncFunc(v, 1);
PrintLn(v);
IncProc(v, 1);
PrintLn(v);

try
   v:=IncFunc(v, 2)
except
   on E: Exception do PrintLn(E.Message);
end;
try
   IncProc(v, 2)
except
   on E: Exception do PrintLn(E.Message);
end;
PrintLn(v);
