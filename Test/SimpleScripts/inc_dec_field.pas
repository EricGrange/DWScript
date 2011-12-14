type
   TTest = class
      Field : Integer;
      procedure IncDec;
      procedure IncDecN(n : Integer);
   end;
   
procedure TTest.IncDec;
begin
   Inc(Field);
   PrintLn(Field);
   Dec(Field);
   PrintLn(Field);
end;

procedure TTest.IncDecN(n : Integer);
begin
   Inc(Field, n);
   PrintLn(Field);
   Dec(Field, n);
   PrintLn(Field);
end;
   
TTest.Create.IncDec;
TTest.Create.IncDecN(2);

var t := new TTest;

Inc(t.Field);
PrintLn(t.Field);
t.IncDec;
Dec(t.Field);
PrintLn(t.Field);

Inc(t.Field, 2);
PrintLn(t.Field);
t.IncDecN(3);
Dec(t.Field, 1);
PrintLn(t.Field);
