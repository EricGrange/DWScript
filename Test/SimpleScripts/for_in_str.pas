var c : Integer;
var s : String;

const cHello = 'Hello';

for c in cHello do
   PrintLn(Chr(c)+': '+IntToStr(c));

s:=#$1D538#$3B1#$1D516;

for c in s do
   PrintLn('unicode $'+UpperCase(IntToHex(c, 5)));
  
var i : Integer;
for i:=Low(s) to High(s) do begin
   c:=Ord(s[i]);
   PrintLn('ord $'+UpperCase(IntToHex(c, 5)));
end;
   
var trap := 'trap';
function TrapIt : String;
begin
   Result:=trap;
   trap:='bug';
end;

for c in TrapIt do
   PrintLn(Chr(c)+': $'+UpperCase(IntToHex(c, 5)));
