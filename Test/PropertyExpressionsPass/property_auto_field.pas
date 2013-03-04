type
   TBase = record
      property Field : Integer;
      property Field2 : Integer read (Field*2) write (Field:=Value div 2);
	  class property VField : Integer;
   end;

var b : TBase;
b.Field:=1;
PrintLn(b.Field);
PrintLn(b.Field2);
b.Field2:=4;
PrintLn(b.Field2);
PrintLn(b.Field);

PrintLn(b.VField);
b.VField:=10;
PrintLn(b.VField);

type
   TTest = class
      property Field : Integer;
      property Field2 : Integer read (Field*2) write (Field:=Value div 2);
	  class property VField : Integer;
   end;

var t := new TTest;
t.Field:=1;
PrintLn(t.Field);
PrintLn(t.Field2);
t.Field2:=4;
PrintLn(t.Field2);
PrintLn(t.Field);

PrintLn(t.VField);
t.VField:=20;
PrintLn(t.VField);
