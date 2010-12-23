type TTest = class
   Field : String;
   procedure AppendString(str : String);
   class operator += uses AppendString;
end;

procedure TTest.AppendString(str : String);
begin
   Field:=Field+str+',';
end;

var t = TTest.Create;

t.AppendString('first');
PrintLn(t.Field);

t += 'second';
PrintLn(t.Field);
