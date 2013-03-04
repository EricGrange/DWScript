type
   TBase = class 
      Field : Integer = 1;
      property Prop : Integer read Field write Field;
      property MultBy1 : Integer read (Field) write (Field);
      property MultBy2 : Integer read (2*Field) write (Field:=Value div 2);
      property MultBy3 : Integer read (MultBy2+Field) write (MultBy1:=Value div 3);
   end;

var o := new TBase;

PrintLn(o.Prop);
PrintLn(o.MultBy1);
PrintLn(o.MultBy2);
PrintLn(o.MultBy3);
o.MultBy2 := 4;
PrintLn(o.Prop);
PrintLn(o.MultBy1);
PrintLn(o.MultBy2);
PrintLn(o.MultBy3);
