type
   TTest = record
      Field : Integer;
      property Prop : Integer read Field write Field;
   end;

procedure Stuff(const t : TTest);
begin
   t.Prop:=t.Prop+333;      // property bypass 'const' as in Delphi
end;

var t : TTest := (Field: 123);

PrintLn(t.Field);
Stuff(t);
PrintLn(t.Field);