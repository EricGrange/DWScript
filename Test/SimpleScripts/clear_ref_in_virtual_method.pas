
type
	TTest = class
		Field : String = 'hello';
		procedure Test; virtual;
	end;

var ref := TTest.Create;

procedure TTest.Test;
begin
	ref := nil;
	PrintLn(Field);
end;

ref.Test;
