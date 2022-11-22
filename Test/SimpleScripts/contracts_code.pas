function TestRequire(x, y : integer): float ;
require
   x <> 0;
   y <> 0;
begin
   Result := 1 / x + 1/ y
end;

try
   TestRequire(0,1);
except
   on e : Exception do PrintLn(e.Message);
end;
try
   TestRequire(1,0);
except
   on e : Exception do PrintLn(e.Message);
end;
