Unit SubProc;

Interface

function Hello(name : String) : String;

Implementation

function Hello(name : String) : String;

   function HelloName : String;
   begin
      Result:='Hello '+name;
   end;

begin
   Result:=HelloName;
end;

End.