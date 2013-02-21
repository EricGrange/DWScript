type
   TTest = class
      class var V : Integer;
      class const C = 1;
      
      class property P1 : Integer read V write V;
      class property P2 : Integer read C write C;
      property F : Integer write Field;
   end;
type
   TBase = class
      const cT = TTest;
      class var vt := TTest;
      Field : class of TTest;
      property T : class of TTest read cT;
      property V : class of TTest read vt;
      property F : class of TTest read Field;
   end;
      
TBase.T.P1 := TBase.T.P1+1;
TBase.T.P1 := TBase.T.P2+1;

TBase.V.P1 := TBase.V.P1+1;

TBase.F.P1 := TBase.F.P1+1;

