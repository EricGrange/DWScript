procedure RequirePositive(a : Integer);
require a > 0 : 'here';
begin
end;

function EnsureNegative(a : Integer) : Integer;
begin
   Result := -a;
ensure
   Result < 0 : 'there';
end;

procedure TestProc;
begin
   RequirePositive(-1);
end;

type TTest = class
   procedure TestMeth;
   begin
      EnsureNegative(-1);
   end;
end;

try
   TestProc;
except
   on E : Exception do
      PrintLn(E.Message + #13#10 + E.StackTrace);
end;


try
   (new TTest).TestMeth;
except
   on E : Exception do
      PrintLn(E.Message + #13#10 + E.StackTrace);
end;

