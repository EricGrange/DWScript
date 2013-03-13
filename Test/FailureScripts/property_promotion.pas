type
	TBase = class
		public
			Field : Integer;
			property Prop : Integer read Field;
	end;
	
type
	TSub = class (TBase)
		protected
			property Dummy;
			property Field;
			property Prop;
	end;
	
PrintLn(TSub.Create.Prop);