type 
	IMyIntf = interface 
		['whatever'] // GUID isn't checked
	end;

type 
	IMyIntf2 = interface 
		[123] // GUID isn't checked but must be string
	end;
	
type 
	IMyIntf3 = interface 
		['hhhh'
	end;	