type TTest = class
   Field : String;
   procedure AppendString(str : String; i : Integer);
   class operator += uses AppendString;
end;

procedure TTest.AppendString(str : String; i : Integer);
begin
   Field:=Field+str+',';
end;

