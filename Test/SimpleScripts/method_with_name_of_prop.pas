type
   TBase = class
      Field : Integer;
      property Prop : Integer read Field;
   end;

type
   TSub = class(TBase)
      function Prop : Integer;
      begin
         Result:=inherited Prop+1;
      end;
   end;
   
var o1 : TBase := new TBase;
var o2 : TBase := new TSub;
var s := TSub.Create;

o1.Field:=10;
o2.Field:=20;
s.Field:=30;

PrintLn(o1.Prop);
PrintLn(o2.Prop);
PrintLn(s.Prop);
PrintLn(TBase(s).Prop);