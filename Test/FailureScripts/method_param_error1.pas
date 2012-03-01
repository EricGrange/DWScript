type
   TMyClass = class
      procedure PrintMe(a : Integer);
   end;
   
var o : TMyClass;

o.PrintMe(IntToStr(bug));

