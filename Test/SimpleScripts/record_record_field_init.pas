type
   TRec = record
      A : Integer;
   end;

const
   cRec : TRec = (A: 1);

type
   TTest1 = record
      B : TRec = cRec;
   end;

type
   TTest2 = record
      B : TRec = (A: 2);
   end;

var r1 : TTest1;
PrintLn(r1.B.A);

var r2 : TTest2;
PrintLn(r2.B.A);
