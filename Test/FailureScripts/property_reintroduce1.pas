type
	TTest = class 
		Field : Integer;
		property Prop1 : Integer read Field;
		property Prop2 : Integer read Field reintroduce;
	end;
	
var t := new TTest;
t.Field := 123;
PrintLn(t.Prop1());
var a := t.Prop2(;