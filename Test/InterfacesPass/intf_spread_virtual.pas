type 
	IMy = interface
		procedure Base;
		procedure Child;
	end;

type
	TBase = class
		procedure Base; virtual; begin PrintLn('Base') end;
		procedure Child; virtual; abstract;
	end;
	
type
	TChild1 = class (TBase, IMy)
		procedure Child; override; begin PrintLn('Child') end;
	end;

var i1 := TChild1.Create as IMy;

i1.Base;
i1.Child;

type
	TChild2 = class (TChild1, IMy)
		procedure Base; override; begin PrintLn('Base2') end;
	end;

var i2 := TChild2.Create as IMy;

i2.Base;
i2.Child;