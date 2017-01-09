procedure Bug3(lazy a : Integer);
begin
   a:=1;
end;

procedure Bug4(lazy a : Integer);
begin
   Inc(Inc(a));
end;

procedure Bug1(lazy var a : Integer);
begin
end;
