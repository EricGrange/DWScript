type
   TBase = class 
      Field : Integer = 1;
      const MyConst = 2;
      property ReadOnly : Integer read Field;
      property WriteProp : Integer write (ReadOnly);
      property WriteConst : Integer write (MyConst);
      property WriteProc : Integer write (Print(Field));
      property WriteFunc : Integer write (Round(Field));
      property WriteExpr : Integer write (Field+Field);
   end;
