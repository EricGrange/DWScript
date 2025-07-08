procedure Test2;
begin
   PrintLn(CurrentStackTrace);
end;

procedure Test;
begin
   Test2;
end;

Test;
