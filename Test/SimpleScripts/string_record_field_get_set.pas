type
   TTest = record
      F : String := 'hello';
   end;

var o : TTest;

PrintLn(o.F);
PrintLn(o.F[3]);
o.F[3]:='z';
PrintLn(o.F);
PrintLn(o.F[3]);
