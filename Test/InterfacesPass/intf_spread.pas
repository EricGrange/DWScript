type 
	IMy = interface
		procedure Base;
		procedure Child;
	end;

type
	TBase = class
		procedure Base; begin PrintLn('Base') end;
	end;
	
type
	TChild = class (TBase, IMy)
		procedure Child; begin PrintLn('Child') end;
	end;

var i := TChild.Create as IMy;

i.Base;
i.Child;