type
   TMyClass = class
      Field : Integer;
      property Prop : Integer read Field;
   end;
   
var o := new TMyClass;
o.Prop:=1;