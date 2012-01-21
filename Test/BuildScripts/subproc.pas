Unit SubProc;

Interface

const
   cHello = 'Hello ';
   cWorld = 'World';

function Hello(name : String) : String;

Implementation

function Hello(name : String) : String;

   function HelloName : String;
   begin
      Result:=cHello+name;
   end;

begin
   Result:=HelloName;
end;

End.