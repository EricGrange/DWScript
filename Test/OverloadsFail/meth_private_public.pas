type
	TFoo = class
	private
		procedure Foo(value: integer); overload;
		begin
		end;
	public
		procedure Foo(value: string);  overload;
		begin
		end;
	end;

type
	TBar = class
		public
			procedure Bar(value: integer); overload;
			begin
			end;
		private
			procedure Bar(value: string);  overload;
			begin
			end;
	end;

var f := new TFoo;

f.Foo(123);

var b := new TBar;

b.Bar('hello');