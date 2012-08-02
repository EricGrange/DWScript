type 
   TSubRec = record
      A, B : Integer;
   end;
type 
   TRec = record
      S : TSubRec;
      C : Integer;
      procedure PrintIt;
      begin
         Print(S.A);
         Print(S.B);
         PrintLn(C);
      end;
   end;

var r1, r2 : TRec;

r1.s.A:=1;
r1.s.b:=2;
r1.c:=3;

r2.PrintIt;
r2:=r1;
r2.PrintIt;

procedure Test(r : TRec);
begin
   r.PrintIt;
end;

r1.S.A:=10;
r1.C:=30;

Test(r1);
Test(r2);
