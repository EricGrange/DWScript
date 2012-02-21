type
   TBase = class
      procedure Test; overload; virtual;
      begin
         PrintLn('Base no param');
      end;
      procedure Test(a : Integer); overload; virtual;
      begin
         PrintLn('Base: '+IntToStr(a));
      end;
   end;
type
   TSub = class(TBase)
      procedure Test; overload; override;
      begin
         PrintLn('Sub no param');
      end;
      procedure Test(a : Integer); override;
      begin
         PrintLn('Sub: '+IntToStr(a));
      end;
   end;

var b : TBase;
b := new TBase;
b.Test;
b.Test(1);

b := new TSub;
b.Test;
b.Test(2);
   
var s := new TSub;
s.Test;
s.Test(3);

