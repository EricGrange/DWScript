type
   TRec = record
      F1, F2 : Integer;
      procedure PrintIt;
      begin 
         Print(F1); Print(','); PrintLn(F2); 
      end;
   end;
   
var r, r1, r2 : TRec;

r1.F1:=1;
r1.F2:=2;
r2.F1:=10;
r2.F2:=20;

procedure CopyRec(const src : TRec; var dest : TRec);
begin
   dest:=src;
end;

CopyRec(r1, r);
r.PrintIt;
CopyRec(r2, r);
r.PrintIt;

var a : array [0..1] of TRec;
CopyRec(r1, a[0]);
CopyRec(r2, a[1]);

a[0].PrintIt;
a[1].PrintIt;
