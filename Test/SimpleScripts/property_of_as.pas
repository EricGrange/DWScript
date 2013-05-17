type
	TTest = class
		Field : Integer;
		property Prop : Integer read Field write Field;
	end;
	
var o : TObject;
o:=TTest.Create;

PrintLn((o as TTest).Field);
PrintLn((o as TTest).Prop);

(o as TTest).Field:=1;

PrintLn((o as TTest).Field);
PrintLn((o as TTest).Prop);

(o as TTest).Prop:=2;

PrintLn((o as TTest).Field);
PrintLn((o as TTest).Prop);
