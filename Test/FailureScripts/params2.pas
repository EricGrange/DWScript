type
   TMyObj = class
      procedure Bug1(a, b, a : Integer);
      begin
      end;

      procedure Bug2(a, b : Integer; a : String);
      begin
      end;
   end;

procedure Bug1(a, b, a : Integer);
begin
end;

procedure Bug2(a, b : Integer; a : String);
begin
end;
