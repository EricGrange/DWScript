type
   TForwardClass = class;
type
   TOtherClass = class
      Field : TForwardClass;
   end;
type
   TSubClass = class (TForwardClass)
   end;
