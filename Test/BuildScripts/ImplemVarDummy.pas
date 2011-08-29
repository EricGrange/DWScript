unit ImplemVarDummy;

interface

function GetDummy : Integer;

implementation

var dummy := 10;

function GetDummy : Integer;
begin
   Result:=dummy;
   Inc(dummy);
end;



