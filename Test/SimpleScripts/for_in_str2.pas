var c, s : String;

const cHello = 'Hello';

for c in cHello do
   PrintLn(c+': '+IntToStr(Ord(c)));

s:=#$1D538'A'#$3B1#$1D516;

for c in s do
   PrintLn('unicode $'+UpperCase(IntToHex(Ord(c), 5))+' len '+IntToStr(Length(c)));
  
   
var trap := 'trap';
function TrapIt : String;
begin
   Result:=trap;
   trap:='bug';
end;

for c in TrapIt do
   PrintLn(c+': $'+UpperCase(IntToHex(Ord(c), 5)));
