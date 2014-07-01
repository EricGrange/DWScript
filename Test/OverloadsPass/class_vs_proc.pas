type 
	TEvent = procedure;
	
type 
	TTest = class 
		procedure Foo; 
		begin 
			PrintLn('Foo'); 
		end; 
	end;

procedure Test(a : TEvent); overload;
begin
	PrintLn('Event'); a; 
end;

procedure Test(a : TObject); overload;
begin
	PrintLn(a.ClassName);
end;

var o := TTest.Create;

Test(o);
Test(@o.Foo);