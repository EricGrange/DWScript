type
   TInternal = class
      procedure Bug1; name "hello";
   end;
   
procedure Dummy; forward; name "bug";
   
type
   TExternal = class external
      procedure Ok1; name "test";
      procedure Ok2;
      procedure Bug2; name 3;
   end;