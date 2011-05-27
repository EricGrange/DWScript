{
Demo: procedures/functions and recursion

This is an implementation of the Selection Sort
algorithm
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

procedure SelectionSort(var dat: TData);
var
  i, j, tmp, minindex, minkey : integer;
begin
  for i := 0 to size - 2 do 
  begin
    minindex := i;
    minkey := dat[i];
    for j := i + 1 to size - 1 do
      if dat[j] < minkey then 
      begin
        minkey := dat[j];
        minindex := j;
      end;
    tmp := dat[minindex];
    dat[minindex] := dat[i];
    dat[i] := tmp;
    swaps := swaps + 1;
  end;
end;

var data : TData;

ShuffleData(data);

SelectionSort(data);
Print('Swaps: ');
if swaps>=75 then
   PrintLn('>=75')
else PrintLn('<75');
ShowData(data);
