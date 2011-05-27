{
Demo: procedures/functions and recursion

This is a simple implementation of the well
known QuickSort algorithm (iterative version)
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

procedure QuickSortIterative(var dat: TData);
begin

  type TStack = record L, R: integer; end;
  var stack: array [0..32] of TStack;
  var i, j, s: integer;	
  var L, R, tmp: integer;

  procedure part;
  var m,n: integer;
  begin
    n := (L + R) div 2;
    m := dat[n];
    i := L;
    j := R;
    repeat 
      while dat[i] < m do i := i + 1;
      while dat[j] > m do j := j - 1;
      if i <= j then 
      begin
        tmp := dat[i];
        dat[i] := dat[j];
        dat[j] := tmp;
	  i := i + 1;
        j := j - 1;
        swaps := swaps + 1;
	end;
    until (i > j);
  end;

  s := 1;
  stack[1].L := 0;
  stack[1].R := size - 1;

  repeat
    L := stack[s].L;
    R := stack[s].R;
    s := s - 1;

    repeat 
      part;
      if (j - L) < (R - i) then 
      begin
        if i < r then 
        begin
          s := s + 1;
          stack[s].L := i;
          stack[s].R := R;
        end;
        R := j;
      end
      else 
      begin
        if l < j then   
        begin
          s := s + 1;
          stack[s].L := L;
          stack[s].R := j;
        end;
        L := i;
      end;
    until L >= R;
  until (s = 0);
end;

var data : TData;

ShuffleData(data);
//ShowData(data);

QuickSortIterative(data);
Print('Swaps: ');
if swaps>=100 then
   PrintLn('>=100')
else PrintLn('<100');
ShowData(data);