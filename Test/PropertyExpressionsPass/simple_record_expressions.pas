type
   TBase = record
      Field : Integer = 1;
      property MultBy2 : Integer read (2*Field);
   end;

type   
   TContainer = record
      FSub : TBase;
      property Field : Integer read (FSub.Field) write (FSub.Field);
      property MultBy2 : Integer read (FSub.MultBy2);
   end;

var o : TBase;

PrintLn(o.Field);
PrintLn(o.MultBy2);

var c : TContainer;

PrintLn(c.Field);
PrintLn(c.MultBy2);
o.Field := 3;
PrintLn(c.MultBy2);
PrintLn(o.MultBy2);
c.FSub.Field := 5;
PrintLn(c.MultBy2);
PrintLn(o.MultBy2);