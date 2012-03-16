type
   TBase = class
      procedure Hello; begin PrintLn('Hello'); end;
      class var HelloPtr := @Hello;
   end;
