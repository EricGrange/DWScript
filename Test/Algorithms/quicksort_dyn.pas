{
Demo: procedures/functions and recursion

This is a simple implementation of the well
known QuickSort algorithm (recursive version)
}

const size = 100;
var swaps := 0;

type TData = array of Integer;

procedure ShowData(d : TData);
begin
   PrintLn('Data:');
   for var i in d do
      PrintLn(i);
end;

procedure ShuffleData(d : TData);
begin
   SetRandSeed(0);

   for var x := 0 to size - 1 do
      d.Add(x);
  
   for var x := 1 to size do 
      d.Swap(RandomInt(size), RandomInt(size));
end;

function Partition(d : TData; i, j : Integer) : Integer;
begin
   var m := d[(i + j) div 2];
   while i < j do begin
      while d[i] < m do i := i + 1;
      while d[j] > m do j := j - 1;
      if d[i] > d[j] then begin
         d.Swap(i, j);
         swaps += 1;
      end else i := j;
   end;
   Result := i;
end;

procedure QuickSort(d : TData; i, j : Integer);
begin
   if i < j then begin 
      var m := Partition (d, i, j);
      QuickSort(d, i, m);
      QuickSort(d, m + 1, j);
   end;
end;

var data : TData;

ShuffleData(data);

QuickSort(data, 0, size - 1);
Print('Swaps: ');
if swaps >= 100 then
   PrintLn('>=100')
else PrintLn('<100');
ShowData(data);
