var t = True;
var f = not t;

function Oopsie : Boolean;
begin
   PrintLn('Oops');
   Result:=False;
end;

function Ookie : Boolean;
begin
   PrintLn('Ookie');
   exit True;
end;

PrintLn(t implies t);
PrintLn(t implies f);
PrintLn(f implies t);
PrintLn(f implies f);

if (f implies Oopsie) then
   PrintLn('Ok')
else PrintLn('Bug');

if (t implies Ookie) then
   PrintLn('Ok')
else PrintLn('Bug');
