type TTest = class
   Field : String;
   procedure AppendString(str : String);
   procedure AppendInteger(i : Integer);
   class operator += String uses AppendString;
   class operator += Integer uses AppendInteger;
end;

procedure TTest.AppendString(str : String);
begin
   Field:=Field+'"'+str+'",';
end;

procedure TTest.AppendInteger(i : Integer);
begin
   Field:=Field+IntToStr(i)+',';
end;

var t = TTest.Create;

t += 1;
PrintLn(t.Field);

t += 'second';
PrintLn(t.Field);

t += 3;
PrintLn(t.Field);

