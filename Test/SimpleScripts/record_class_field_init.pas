type
   TRec = record
      A : Integer;
   end;

const
   cRec : TRec = (A: 1);

type
   TTest1 = class
      B : TRec = cRec;
   end;

type
   TTest2 = class
      B : TRec = (A: 2);
   end;

var r1 := new TTest1;
PrintLn(r1.B.A);

var r2 := new TTest2;
PrintLn(r2.B.A);
