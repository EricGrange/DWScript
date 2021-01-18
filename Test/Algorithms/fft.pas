// based on SciMark

function IntLog2(n: Integer): Integer;
var
  k: Integer;
begin
  k := 1;
  Result := 0;

  while(k < n) do
  begin
    k := k*2;
    Inc(Result);
  end;

  if (n <> (1 shl Result)) then
  begin
    PrintLn('FFT: Data length is not a power of 2!: '+ n.ToString);
    {$ifdef VER150}exit;{$else}exit(1);{$endif}
  end;
end;

procedure FFT_bitreverse(_N: Integer; data: array of Float);
{ This is the Goldrader bit-reversal algorithm }
var
    n, nm1,i, j, ii, jj, k: Integer;
    tmp_real, tmp_imag: Float;
begin
    n:=_N div 2;
    nm1 := n-1;
    j:=0;
    for i :=0  to nm1-1 do
    begin
      {int ii = 2*i; }
      ii := i shl 1;

      {int jj = 2*j; }
      jj := j shl 1;

      { int k = n / 2 ; }
      k := n shr 1;

      if (i < j) then
      begin
        tmp_real    := data[ii];
        tmp_imag    := data[ii+1];
        data[ii]   := data[jj];
        data[ii+1] := data[jj+1];
        data[jj]   := tmp_real;
        data[jj+1] := tmp_imag;
      end;

      while (k <= j) do
      begin
        {j = j - k ; }
        Dec(j, k);

        {k = k / 2 ;  }
        k := k shr 1;
      end;

      Inc(j, k);
    end;
end;

procedure FFT_transform_internal(_N: Integer; data: array of Float; direction: Integer);
var
  n, bit, logn, dual, a, b, i, j: Integer;
  w_real, w_imag, theta, s, t, s2, wd_real, wd_imag, tmp_real, tmp_imag:Float;

begin
  n := _N div 2;
  bit := 0;
  dual := 1;

  if (n = 1) then exit;         { Identity operation! }
  logn := IntLog2(n);


  if (n = 0) then exit;

  { bit reverse the input data for decimation in time algorithm }
  FFT_bitreverse(_N, data) ;

  { apply fft recursion }
  { this loop executed int_log2(N) times }
  while(bit < logn) do
  begin
    w_real := 1.0;
    w_imag := 0.0;

    theta := 2.0 * direction * Pi / (2.0 * dual);
    s := Sin(theta);
    t := Sin(theta / 2.0);
    s2 := 2.0 * t * t;

    b:=0;
    while b < n do
    begin
      i := 2*b ;
      j := 2*(b + dual);

      wd_real := data[j] ;
      wd_imag := data[j+1] ;

      data[j]   := data[i]   - wd_real;
      data[j+1] := data[i+1] - wd_imag;
      data[i]   := data[i] + wd_real;
      data[i+1] := data[i+1] + wd_imag;

      Inc(b, 2 * dual);
    end;

    { a = 1 .. (dual-1) }
    for a := 1 to dual-1 do
    begin
      { trignometric recurrence for w-> exp(i theta) w }
      tmp_real := w_real - s * w_imag - s2 * w_real;
      tmp_imag := w_imag + s * w_real - s2 * w_imag;
      w_real := tmp_real;
      w_imag := tmp_imag;

      b:=0;
      while b < n do
      begin
        i := 2*(b + a);
        j := 2*(b + a + dual);

        wd_real := w_real * data[j] - w_imag * data[j+1];
        wd_imag := w_real * data[j+1] + w_imag * data[j];

        data[j]   := data[i]   - wd_real;
        data[j+1] := data[i+1] - wd_imag;
        data[i]   := data[i] + wd_real;
        data[i+1] := data[i+1] + wd_imag;

        Inc(b, 2 * dual);
      end;
    end;

    Inc(bit);
    dual := dual*2;
  end;
end;

function FFT_num_flops(_N: Integer): Float;
var
  Nd, logN: Float;
begin
  Nd := _N;
  logN := IntLog2(_N);

  Result := (5.0 * Nd - 2) * logN + 2 * (Nd + 1);
end;

procedure FFT_transform(_N: Integer; data: array of Float);
begin
  FFT_transform_internal(_N, data, -1);
end;

procedure FFT_inverse(_N: Integer; data: array of Float);
var
  n, i: Integer;
  norm: Float;
begin
  n := _N div 2;

  FFT_transform_internal(_N, data, 1);

  norm := 1.0 / n;
  for i := 0 to _N - 1 do
    data[i] := data[i] * norm;
end;

procedure PrintArray(x : array of Float);
begin
   Print(x[0].ToString(1));
   for var i := 1 to x.High do begin
      Print(';');
      Print(x[i].ToString(1));
   end;
   PrintLn('');
end;

const N = 8;

var twoN := 2*N;
var x : array of Float;
x.SetLength(twoN);
for var i := 0 to x.High do
   x[i] := i xor 5;

PrintLn(FFT_num_flops(N));

FFT_transform(twoN, x);     { forward transform }

PrintArray(x);

FFT_inverse(twoN, x);       { backward transform }

PrintArray(x);



