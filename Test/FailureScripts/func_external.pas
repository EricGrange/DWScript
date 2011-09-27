unit Test;

interface

procedure Test; external;
function Dummy : String; external; deprecated "doh";

procedure Deprec;

implementation

procedure Deprec;
begin
   Test;
   Dummy;
end;

procedure Deprec;
begin
   Test;
   Dummy;
end;

end.