type
   TBase = class
      class var Test : Integer;
   end;
type
   TChild = class (TBase)
   end;

type
   TChildHide = class (TBase)
      class var Test : Integer := 456;
   end;

PrintLn(TBase.Test);
TBase.Test:=123;
PrintLn(TBase.Test);
PrintLn(TChild.Test);
PrintLn(TChildHide.Test);

var b : TBase;
var c : TChild;
var ch : TChildHide;

PrintLn(b.Test);
PrintLn(c.Test);
PrintLn(ch.Test);
PrintLn(TBase(ch).Test);
