type
   TBase = class
      Field : Integer;
      class var Test := 1;
      class const C = 10;
      class var Test2 := test + c;
      class procedure Hello; begin PrintLn('Hello'); PrintLn(Test2); end;
      class var HelloPtr := @Hello;
   end;

TBase.HelloPtr;
