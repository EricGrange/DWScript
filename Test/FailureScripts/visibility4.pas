type
   TTest = class
      private
         Field : Integer;
   end;
type
   TSubTest = class (TTest)
      protected
         Field2 : Integer;
         procedure Stuff;
   end;

procedure TSubTest.Stuff;
begin
   Field2:=1;
   Field:=2;
end;

var o := TSubTest.Create;
o.Field:=1;