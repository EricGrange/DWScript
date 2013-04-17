type Matrix = array of array of Float;

// based on SciMark 2a

function LU_factor(M, N: integer; A: Matrix;
  pivot: array of Integer): integer;
Var
  minMN, j, i, jp, k, ii, jj: integer;
  ab, t, recp, AiiJ: Float;
  ta, Aii, Aj: array of Float;

begin
  minMN := Min(M, N);
  j := 0;

  for j := 0 to minMN - 1 do
  begin
    { find pivot in column j and  test for singularity. }
    jp := j;

    t := Abs(A[j][j]);
    for i := j + 1 to M - 1 do
    begin
      ab := Abs(A[i][j]);
      if (ab > t) then
      begin
        jp := i;
        t := ab;
      end;
    end;

    pivot[j] := jp;

    { jp now has the index of maximum element }
    { of column j, below the diagonal }

    if (A[jp][j] = 0) then
      exit(1); { factorization failed because of zero pivot }

    if (jp <> j) then
    begin
      { swap rows j and jp }
      ta := A[j];
      A[j] := A[jp];
      A[jp] := ta;
    end;

    if (j < M - 1) then { compute elements j+1:M of jth column }
    begin
      { note A(j,j), was A(jp,p) previously which was }
      { guarranteed not to be zero (Label #1) }

      recp := 1.0 / A[j][j];

      for k := j + 1 to M - 1 do
        A[k][j] := A[k][j] * recp;
    end;

    if j < minMN - 1 then
    begin
      { rank-1 update to trailing submatrix:   E = E - x*y; }
      { E is the region A(j+1:M, j+1:N) }
      { x is the column vector A(j+1:M,j) }
      { y is row vector A(j,j+1:N) }

      for ii := j + 1 to M - 1 do
      begin
        Aii := A[ii];
        Aj := A[j];
        AiiJ := Aii[j];

        for jj := j + 1 to N - 1 do
          Aii[jj] := Aii[jj] - AiiJ * Aj[jj];

      end;
    end;
  end;

  Result := 0;
end;

const M = 4;
const N = 4;

var a := new Float[M, N];

for var i:=0 to M-1 do
	for var j:=0 to N-1 do
		a[i,j]:=(i xor 1)*M+j+1;
		
var p := new Integer[N];

PrintLn(LU_factor(M, N, a, p));

procedure PrintResult;
begin
	for var j:=0 to p.High do begin
		Print(p[j]);
		Print(', ');
	end;
	PrintLn('');

	for var i:=0 to M-1 do begin
		for var j:=0 to N-1 do begin
			Print(Format('%.2f', [a[i,j]]));
			Print(', ');
		end;
		PrintLn('');
	end;
end;

PrintResult;

for var i:=0 to M-1 do
	for var j:=0 to N-1 do
		a[i,j]:=(i xor 2)*11-((j*5) xor 3)*5;
		
PrintLn(LU_factor(M, N, a, p));

PrintResult;
