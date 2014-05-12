type
   TBase = class
      class property Base : TClass read (Self.ClassType);
   end;
   
var b : TClass;

b := TBase.Base;

if b=TBase then
	PrintLn(b.ClassName);
