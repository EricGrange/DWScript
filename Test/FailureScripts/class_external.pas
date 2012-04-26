type
   TInternal = class
      procedure Bug1; external "hello";
   end;
   
procedure Dummy; external "bug"; forward; 
   
type
   TExternal = class external
      procedure Ok1; external "test";
      procedure Ok2;
      procedure Bug2; external 3;
   end;