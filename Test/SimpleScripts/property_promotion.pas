type
	TBase = class
		protected
			Field : Integer;
			property Prop : Integer read Field;
	end;
	
type
	TSub = class (TBase)
		public
			property Prop;
	end;
	
PrintLn(TSub.Create.Prop);