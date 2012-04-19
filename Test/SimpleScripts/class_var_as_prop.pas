type
   TBase = class
      class var v : Integer;
      property rv : Integer read v;
      property wv : Integer write v;
      property rwv : Integer read v write v;
   end;

TBase.v:=1;
PrintLn(TBase.rv);
TBase.wv:=2;
PrintLn(TBase.rwv);
TBase.rwv:=3;
PrintLn(TBase.v);