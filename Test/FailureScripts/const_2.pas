procedure P(const c : Integer);
begin
   c:=1;
end;

procedure P2;
const
   c = 2;
begin
   c:=1;
end;

procedure P3(const c : Integer);
const
   lc = c;
begin
   
end;