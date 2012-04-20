type
   TBase = class
      class const c1 = 1;
      const c2 = 2;
      property p1 : Integer read c1;
      property p2 : Integer read c2;
   end;

var o := TBase.Create;

PrintLn(TBase.P1);
PrintLn(TBase.P2);

PrintLn(o.P1);
PrintLn(o.P2);
