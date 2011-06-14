type
   TAClass = class external
      procedure Hello;
   end;

function Dummy(param : Integer) : String; external;

try
   Dummy(12);
except
   on E : Exception do
      PrintLn(E.Message);
end;