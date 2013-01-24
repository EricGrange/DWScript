type
   TBase = class 
      class var Field : Integer = 1;
   end;
   
type
   TBaseHelper = class helper for TBase
      class property MultBy2 : Integer read (2*Field) write (Field:=Value div 2);
   end;

type   
   TSub = class (TBase)
      class property MultBy3 : Integer read (3*Field) write (Field := Value div 3);
      class property MultBy42 : Integer read (42*Field) write (Field := Value div 42);
   end;

type   
   TContainer = class
      class var FSub : TSub;
      class var FBase: TBase;
   end;
   
type
   TContainerHelper = class helper for TContainer
      class property MultBy2 : Integer read (FBase.MultBy2) write (FBase.MultBy2);
      class property MultBy42 : Integer read (FSub.MultBy42) write (FSub.MultBy42);
      class property MultByX : Integer read (TBase(FSub).MultBy2) write (TBase(FSub).MultBy2);
   end;


TBase.MultBy2 := 6;
PrintLn(TBase.Field);
   
var c := new TContainer;
c.FSub := new TSub;
c.FBase := new TBase;

c.MultBy2 := 30;
PrintLn(c.FBase.Field);
c.MultBy42 := 42;
PrintLn(c.FBase.Field);
c.MultByX := 30;
PrintLn(c.FBase.Field);

