type
   TMyObj = class
      Field : String;
      
      procedure Test(a : String);
      function GetIt : String;
   end;
   
procedure TMyObj.Test(a : String);
begin

   procedure Loc(b : String);
   begin
      Field:=a+b;
   end;
   
   Loc(a);

end;

function TMyObj.GetIt : String;
begin

   function Loc : String;
   begin
      Result:=Field;
   end;
   
   Result:=Loc;

end;

var o := TMyObj.Create;

o.Test('hello');
PrintLn(o.Field);
PrintLn(o.GetIt);
