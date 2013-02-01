type
   TMyRange = class
      FMin, FMax : Integer;

      constructor Create(min, max : Integer);     

      function Contains(i : Integer) : Boolean;
      function ContainsArray(i : array of Integer) : Boolean;

      class operator IN Integer uses Contains;
      class operator IN array of Integer uses ContainsArray;
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

function TMyRange.ContainsArray(i : array of Integer) : Boolean;
var
   k : Integer;
begin
   for k in i do
      if k not in Self then
         Exit(False);
   Result:=True;
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

var vals : array of Integer;
vals:=[1, 3];
PrintLn(vals in range1to5);
PrintLn(vals in range3to7);
