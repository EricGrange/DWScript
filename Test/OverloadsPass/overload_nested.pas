procedure Test; overload;
begin
   PrintLn('Hello');
end;

procedure Test(s : String); overload;
begin
   PrintLn('Hello '+s);
end;

Test;
Test('World');

procedure Nested;
begin

   procedure Test; overload;
   begin
      PrintLn('ByeBye');
   end;

   procedure Test(s : String); overload;
   begin
      PrintLn('ByeBye '+s);
   end;

   Test;
   Test('World');
end;

Nested;

Test;
Test('World');
