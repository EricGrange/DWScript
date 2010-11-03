type
   TMyObj1 = class

   end;
type
   TMyObj2 = class
      Field : TMyObj1;
   end;

var o1 = TMyObj2.Create;

o1.Field:=TMyObj1.Create;
TMyObj2.Create.Field:=TMyObj1.Create;

