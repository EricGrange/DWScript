{
Demo: General Testprogram

This is an implementation of the classic
Bubble Sort algorithm

Warning: This is a very very very slow algorithm!
}

const size = 100;

var swaps: Integer;
type TData = array [0 ..size - 1] of integer;

procedure ShowData(d: TData);
var
  x: Integer;
begin
  PrintLn('Data:');
  for x := 0 to size - 1 do
    PrintLn(IntToStr(d[x]));
end;

procedure ShuffleData(var d: TData);
var
  x, tmp, idxa, idxb: Integer;
begin
  SetRandSeed(0);
  
  for x := 0 to size - 1 do
    d[x] := x;
  
  for x := 1 to size do
  begin
    idxa := Trunc(Random() * size);
    idxb := Trunc(Random() * size);
    tmp := d[idxa];
    d[idxa] := d[idxb]; 
    d[idxb] := tmp;
  end;
end;

procedure BubbleSort(var d: TData);
var
  x, tmp: Integer;
  changed: Boolean;
begin
  changed := True;
  while changed do
  begin
    changed := False;
    for x := 0 to size - 2 do
      if d[x] > d[x + 1] then
      begin
        tmp := d[x + 1];
        d[x + 1] := d[x];
        d[x] := tmp;
        changed := True;
        swaps := swaps + 1;
      end;
  end;
end;

swaps:= 0;
var data : TData;

ShuffleData(data);

BubbleSort(data);
Print('Swaps: ');
if swaps>=150 then
   PrintLn('>=1500')
else PrintLn('<1500');
ShowData(data);
