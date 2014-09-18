type
	TFoo = class
	private
		procedure Foo(value: integer); overload;
		begin
			PrintLn('Integer Foo');
		end;
	public
		procedure Foo(value: string);  overload;
		begin
			PrintLn('String Foo');
			Foo(123);
		end;
	end;

type
	TBar = class
		public
			procedure Bar(value: integer); overload;
		private
			procedure Bar(value: string);  overload;
			begin
				PrintLn('String Bar');
			end;
	end;

procedure TBar.Bar(value: integer);
begin
	PrintLn('Integer Bar');
	Bar('hello');
end;

var f := new TFoo;

f.Foo('hello');

var b := new TBar;

b.Bar(123);