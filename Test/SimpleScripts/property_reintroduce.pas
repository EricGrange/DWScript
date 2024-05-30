type
	TTest = class 
		Field : Integer;
		property Prop : Integer read Field reintroduce;
	end;
	
var t := new TTest;
t.Field := 123;
PrintLn(t.Prop());