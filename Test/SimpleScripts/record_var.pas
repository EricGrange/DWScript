type
   TBase = record
      Field : Integer;
      class var Test : Integer;
   end;

PrintLn(TBase.Test);
TBase.Test:=123;
PrintLn(TBase.Test);

var b : TBase;

PrintLn(b.Test);
