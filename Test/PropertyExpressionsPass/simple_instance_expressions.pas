type
   TBase = class 
      Field : Integer = 1;
      property MultBy2 : Integer read (2*Field);
   end;

type   
   TSub = class (TBase)
      property MultBy3 : Integer read (3*Field);
   end;

type   
   TContainer = class
      FSub : TSub;
      property Field : Integer read (FSub.Field) write (FSub.Field);
      property MultBy2 : Integer read (FSub.MultBy2);
   end;

var o := TSub.Create;

PrintLn(o.Field);
PrintLn(o.MultBy2);
PrintLn(o.MultBy3);

var c := new TContainer;
c.FSub := o;

PrintLn(o.Field);
PrintLn(o.MultBy2);
o.Field := 3;
PrintLn(o.MultBy2);