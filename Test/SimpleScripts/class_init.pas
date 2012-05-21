type
   TBase = class
      F := 123;
      A := [1, 2, 3];
      procedure P; virtual;
      begin
         PrintLn(F);
         Print(A.Length);
         Print(A[0]);
         Print(A[1]);
         PrintLn(A[2]);
      end;
   end;
type
   TChild = class (TBase)
      B = 4.5;
      procedure P; override;
      begin
         inherited P;
         PrintLn(B);
      end;
   end;

var b := TBase.Create;
var c := TChild.Create;

b.P;
c.P;