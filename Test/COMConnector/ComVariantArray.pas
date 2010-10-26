var A : ComVariantArray;
A.High := 3; // set dimension
A[0] := 10;
A[1] := 20;
A[2] := 'DWS';
A[3] := 'Array';

var i : Integer;
for i := A.Low to A.High do
   println(A[i]);

println(A.Low);
println(A.High);
println(A.Length);