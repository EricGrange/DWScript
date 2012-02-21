type
   TBase = class
      procedure Test;
      begin
         PrintLn('Base');
      end;
   end;
   
type
   TSub = class(TBase)
      procedure Test(a : Integer); overload;
      begin
         PrintLn(a);
      end;
   end;
   
var b : TBase := TBase.Create;
b.Test;

var s : TSub := TSub.Create;
s.Test(123);
s.Test;
TBase(s).Test;

   
   