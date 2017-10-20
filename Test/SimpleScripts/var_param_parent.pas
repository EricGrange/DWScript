type 
    TTest = class
       Field : Integer;
       procedure Inc; begin Field += 1; end;
    end;

procedure Test(var a : TTest);
begin
   procedure SubTest;
   begin
      a.Inc;
   end;

   a.Inc;
   SubTest;
end;


var a := TTest.Create;
Test(a);
PrintLn(a.Field);