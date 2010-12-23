type TTest = class
   Field : String;
   procedure AppendString(str : String);
   procedure AppendString2(str : String);
   class operator += uses AppendString;
   class operator += uses AppendString2;
end;

procedure TTest.AppendString(str : String);
begin
   Field:=Field+str+',';
end;

procedure TTest.AppendString2(str : String);
begin
   Field:=Field+str+',';
end;
