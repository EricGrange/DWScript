var i : Integer;
var r : Integer = 1;
for i:=200 downto 0 do r:=i;
if r<>0 then Print('r was '+IntToStr(r));