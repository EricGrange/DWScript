{
Demo: procedures/functions and recursion

This is a simple implementation of the well
known QuickSort algorithm (recursive version)
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
    idxa := 2;
    idxa := Trunc(Random() * size);
    idxb := Trunc(Random() * size);
    tmp := d[idxa];
    d[idxa] := d[idxb]; 
    d[idxb] := tmp;
  end;

end;

function partition (var d: TData; i, j: integer) : integer;
var 
  m, tmp: integer;
begin
//  m := (d[i] + d[j]) div 2;
  m := d[(i + j) div 2];
//  PrintLn('Partition[' + IntToStr(m) + ']: ' + IntToStr(i) + ', ' + IntToStr(j));
  while i < j do
  begin
    while d[i] < m do i := i + 1;
    while d[j] > m do j := j - 1;
    if d[i] > d[j] then
    begin
      tmp := d[j];
      d[j] := d[i];
      d[i] := tmp;
      Swaps := Swaps + 1;
    end
    else 
      i := j;
  end;
  Result := i;
end;

procedure QuickSort(var d: TData; i, j : Integer);
begin
  var m : Integer;
  if i < j then
  begin 
//    PrintLn('QuickSort: ' + IntToStr(i) + ', ' + IntToStr(j));
//    ShowData(d);
    m := partition (d, i, j);
    quicksort (d, i, m);
    quicksort (d, m + 1, j);
  end;
//  PrintLn('STOP-QS: ' + IntToStr(i) + ', ' + IntToStr(j));
//  PrintLn('');
end;

var data : TData;

ShuffleData(data);

QuickSort(data, 0, size - 1);
Print('Swaps: ');
if swaps>=100 then
   PrintLn('>=100')
else PrintLn('<100');
ShowData(data);
