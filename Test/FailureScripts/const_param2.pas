procedure Change(var v : Integer);
begin
   v:=1;
end;

procedure Test(const v : Integer);
begin
   Change(v);
end;