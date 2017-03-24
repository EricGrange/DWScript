var i : Integer;

var f1 := lambda => Inc(i);

procedure Test;
begin
   var j := 3;
   var f2 := lambda => Inc(j);
end;

type
    TTest = class
        Field : Integer;
        function Func : Integer; empty;
        procedure Proc;
        begin
           f1 := lambda => Field; 
           f1 := lambda => Func;
        end;
    end;

