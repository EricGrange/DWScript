type
   TTest = class
      Field : Integer;
      constructor Create(a : Integer);
      begin
         if a<=0 then Exit;
         Field:=a;
      end;
   end;
   
var t : TTest;

t := new TTest(1);
PrintLn(t.Field);

t := new TTest(-1);
PrintLn(t.Field);