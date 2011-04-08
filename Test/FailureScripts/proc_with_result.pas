procedure Test : Integer;
begin
   Result:=1;
end;

type
   TMyClass = class
      procedure Meth : String;
   end;
   
procedure TMyClass.Meth : String;
begin
   Result:='';
end;