type
   TBase = class 
      Field : Integer = 1;
      property ReadOnly : Integer read Field;
      property WriteOnly : Integer write Field;
      property MappedRead : Integer read WriteOnly;
      property MappedWrite : Integer write ReadOnly;
   end;
