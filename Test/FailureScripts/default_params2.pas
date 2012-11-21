type TOTO = class
   function test(i : Integer=0) : Integer;
end;
function TOTO.test(i : Integer=1) : Integer;  begin Result:=i; end;

function test(i : Integer=0) : Integer; forward;

function test(i : Integer=1) : Integer;  begin Result:=i; end;
