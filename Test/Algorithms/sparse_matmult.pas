// based on SciMark

procedure SparseCompRow_matmult(M: Integer; y, val : array of Float;
  row, col: array of Integer; x: array of Float; NUM_ITERATIONS: Integer);
var
  reps, r, i: Integer;
  sum: Float;
begin
  for reps := 0 to NUM_ITERATIONS - 1 do
  begin
    for r := 0 to M - 1 do
    begin
      sum := 0.0;
      for i := row[r] to row[r + 1] - 1 do
        sum := sum + x[col[i]] * val[i];

      y[r] := sum;
    end;
  end;
end;

const N = 100;
const NZ = 500;

var x, y, val : array of Float;
var col, row : array of Integer;

for var i := 1 to N do
   x.Add(((i + N) xor $777)/Max(N, $777));

y.SetLength(N);

var nr := NZ div N;  { average number of nonzeros per row  }
var anz := nr *N;    { _actual_ number of nonzeros         }

for var i := 1 to anz do
   val.Add(((anz - i) xor $567)/Max(anz, $567));

col.SetLength(nz);
row.SetLength(N+1);

row[0] := 0;
for var _r:=0 to N-1 do
begin
   { initialize elements for row r }

   var rowr := row[_r];
   var step := _r div nr;

   row[_r+1] := rowr + nr;
   if (step < 1) then step := 1;   { take at least unit steps }


   for var i:=0 to nr-1 do
       col[rowr+i] := i*step;
end;

SparseCompRow_matmult(N, y, val, row, col, x, 10);

for var yv in Y do PrintLn(yv.ToString(2));
