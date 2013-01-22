type
   TBase = class 
      Field : Integer;
      property Test : Integer read (2*Field ;
      property Direct : Integer read Field;
      property Test2 : Integer read (2*Field write InvalidName;
   end;
