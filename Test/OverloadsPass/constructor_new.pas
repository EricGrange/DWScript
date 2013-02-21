type
   TTest = class
      constructor Create(i : Integer); overload; default;
      begin
         PrintLn(i);
      end;
      constructor Create(s : String); overload;
      begin
         PrintLn(s+s);
      end;
      class procedure Create(b : Boolean); overload;
      begin
         PrintLn(b);
      end;
      class procedure Create(c : TClass); overload;
      begin
         PrintLn(c.ClassName);
      end;
   end;
   
TTest.Create(123);
TTest.Create('abc');
TTest.Create(True);
TTest.Create(TTest);
new TTest(456);
new TTest('def');