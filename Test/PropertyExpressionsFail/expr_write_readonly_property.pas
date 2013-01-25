type
   TBase = class 
      Field : Integer = 1;
      const MyConst = 2;
      property ReadOnly : Integer read Field;
      property WriteProp : Integer write (ReadOnly);
      property WriteConst : Integer write (MyConst);
      property WriteFunc : Integer write (Round(Field));
   end;
