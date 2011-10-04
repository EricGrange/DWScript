type
   TMyClass = class
      Field : Integer;
      property Prop : Integer write Field;
   end;
   
var o := new TMyClass;
PrintLn(o.Prop);