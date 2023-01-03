type 
	TTest = class
		FField : Integer;
		function GetField : Integer;
		property Field : Integer read GetField write FField;
	end;

function TTest.GetFiled : Integer;
begin
	Result := FField;
end;