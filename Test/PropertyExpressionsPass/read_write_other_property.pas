type
   TBase = class 
      Field : Integer = 1;
      property Prop : Integer read Field write Field;
      property Mapped : Integer read Prop write Prop;
   end;
   
var b := new Tbase;

PrintLn(b.Mapped);
b.Mapped:=2;
PrintLn(b.Mapped);

