type
	TTest = class external
		Field : Integer;
		property Prop : Integer read (2*Field) write (Field:=Value div 2);
		property Yes : Boolean;
	end;