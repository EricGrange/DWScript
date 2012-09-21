type
   TRecord = record
      F : Integer;
      A : array of TRecord;
      procedure PrintMe;
   end;

procedure TRecord.PrintMe;
var
   i : Integer;
begin
   PrintLn(F);
   for i:=A.Low to A.High do
      A[i].PrintMe;
end;   
   
var r : TRecord;
var r2 : TRecord;

r.F:=1;

r2.F:=2;
r.A.Add(r2);

r2.F:=3;
r.A.Add(r2);

r.A.Add(r2);

r.PrintMe;

