
type
	TTest = class
		Field : String = 'hello';
		destructor Destroy; override;
	end;

var ref := TTest.Create;

destructor TTest.Destroy;
begin
	ref := nil;
	PrintLn(Field);
end;
