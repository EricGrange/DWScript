var a := new Integer[3, 5];

procedure PrintA;
var
   x, y : Integer;
begin
   for y:=0 to 4 do begin
      for x:=0 to 2 do begin
         Print(a[x, y]);
         Print(',');
      end;
      PrintLn('');
   end;
end;

PrintA;

for var x := 0 to a.High do
   for var y := 0 to a[x].High do
      a[x, y]:=x*10+y+1;
      
PrintA;

for var x := 0 to a.High do
   for var y := 0 to a[x].High do
      a[x, y]:=a[x, y]*2;
      
PrintA;
