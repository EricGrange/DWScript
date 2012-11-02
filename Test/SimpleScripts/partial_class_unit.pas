unit partial_class_unit;

interface

type
   TTest = partial class
      Field : Integer;
      procedure PrintMe;
   end;
   
   TTest = partial class
      procedure Inc;
   end;
   
implementation

type   
   TTest = class partial
      procedure Dec; begin Field-=1; end;
   end;

   TTest = partial class
      procedure PrintDoubled; begin PrintLn(Field*2); end;
   end;
   
procedure TTest.PrintMe; 
begin 
   PrintLn(Field); 
end;

procedure TTest.Inc;
begin
   Field+=1; 
end;
   
initialization   
   
var o := new TTest;

o.PrintMe;
o.Inc;
o.PrintMe;
o.PrintDoubled;
o.Dec;
o.PrintMe;