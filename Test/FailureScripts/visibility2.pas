type
   TTest = class
      protected
         Field : Integer;
      published
         procedure Proc;
   end;

procedure TTest.Proc;
begin
   Field:=1;
end;

var o := TTest.Create;
o.Proc;
o.Field:=2;
