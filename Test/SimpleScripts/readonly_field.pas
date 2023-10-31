type
	TTest = class
		FField : Integer := 1; readonly;
		FTrap : Integer;
		//ReadOnly : Boolean;
		
		constructor CreateBis;
	end;

constructor TTest.CreateBis;
begin
	FField := 2;
end;

var t := new TTest;
PrintLn(t.FField);

t := TTest.CreateBis;
PrintLn(t.FField);