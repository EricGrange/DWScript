type
   TBase = class 
      F : array [0..2] of Integer;
      property Arr[i : Integer] : Integer read (F[i]) write (F[i]); default;
      property ArrPlus[i, b : Integer] : Integer read (F[i]+b) write (F[i] := Value - b);
      property Count : Integer read (F.Length);
   end;
   
var a := TBase.Create;
var k : Integer;

for k:=0 to a.Count-1 do
   a[k]:=10*k;
   
for k:=0 to a.Count-1 do begin
   PrintLn(a[k]);
   PrintLn(a.ArrPlus[k, k+1]);
   a.ArrPlus[k, k+1] := 10+k+1;
   PrintLn(a[k]);
end;