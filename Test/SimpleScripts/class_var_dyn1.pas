type
   TBase = class
      Field : Integer;
      class var Test : Integer := 11;
      class procedure PrintIt; begin PrintLn(Test); end;
      constructor Create; begin Field:=Test; end;
   end;

TBase.PrintIt;
PrintLn(TBase.Create.Field);

TBase.Test:=22;

TBase.PrintIt;
PrintLn(TBase.Create.Field);
