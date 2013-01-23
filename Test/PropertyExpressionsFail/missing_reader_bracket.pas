type
   TBase = class 
      Field : Integer;
      property Test : Integer read (2*Field ;
      property Direct : Integer read Field;
      property Test2 : Integer read (2*Field write Field;
      property Test3 : Integer read ((2*Field) write Field;
      property Test4 : Integer read (2*Field) write (2*Field;
    end;
