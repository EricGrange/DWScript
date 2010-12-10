procedure Bug1(lazy var a : Integer);
begin
end;

procedure Bug2(lazy const a : Integer);
begin
end;

procedure Bug3(lazy a : Integer);
begin
   a:=1;
end;

procedure Bug4(lazy a : Integer);
begin
   Inc(Inc(a));
end;
