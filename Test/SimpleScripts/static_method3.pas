type
   TStatic = class
      class function Test(a : Integer) : String; static;
      begin
         Result:=IntToStr(a);
      end;
   end;

type   
   TSubStatic = class (TStatic)
      class function Test(a : Integer) : String; static;
      begin
         Result:=TStatic.Test(a+1);
      end;
   end;

PrintLn(TStatic.Test(1));
PrintLn(TSubStatic.Test(2));