type
   TTest = partial class
      Field : Integer;
      procedure PrintMe; begin PrintLn(Field); end;
   end;
   
type   
   TTest = partial class
      procedure Inc; begin Field+=1; end;
   end;

type   
   TTest = class partial
      procedure Dec; begin Field-=1; end;
   end;

var o := new TTest;

o.PrintMe;
o.Inc;
o.PrintMe;
o.Dec;
o.PrintMe;