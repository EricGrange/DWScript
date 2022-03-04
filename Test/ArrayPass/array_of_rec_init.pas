type
   TRec = record
      Field : Integer
   end;

const a1 : array [0..0] of TRec  = [(Field: 1)];

PrintLn(a1[0].Field);

var a2 : array [1..2] of TRec = [(Field:2), (Field:3)];

PrintLn(a2[1].Field);
PrintLn(a2[2].Field);
