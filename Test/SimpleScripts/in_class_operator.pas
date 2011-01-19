type
   TMyRange = class
      FMin, FMax : Integer;

      constructor Create(min, max : Integer);     

      function Contains(i : Integer) : Boolean;

      class operator IN Integer uses Contains;
   end;

constructor TMyRange.Create(min, max : Integer);
begin
   FMin:=min;
   FMax:=max;
end;

function TMyRange.Contains(i : Integer) : Boolean;
begin
   Result:=(i>=FMin) and (i<=FMax);
end;

var range1to5 := TMyRange.Create(1, 5);
var range3to7 := TMyRange.Create(3, 7);

var i : Integer;

PrintLn('Range 1-5:');
for i:=1 to 9 do
  if i in range1to5 then
     Print(i);
PrintLn('');

PrintLn('Range 3-7:');
for i:=1 to 9 do
  if i in range3to7 then
     Print(i);
PrintLn('');
