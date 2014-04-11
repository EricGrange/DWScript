type
  TClassTest = class
	Test : Integer;
  end;
  
type TMeta = class of TClassTest;
  
var m : TMeta;

m.Test;