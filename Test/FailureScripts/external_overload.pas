type
   TExternal = class external end;
type
   TSub = class (TExternal)
      procedure Test; overload;
   end;