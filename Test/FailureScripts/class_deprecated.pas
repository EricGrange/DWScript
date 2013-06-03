type 
	TBase = class (TObject)
    end; deprecated;
	
type
	TOther = class 
	end; deprecated "old stuff";
	
type 
	TTest = class
		FField : TBase;
		function GetOther : TOther;
		property O : TOther read GetOther;
	end;
	
var b : TBase;
b := TBase.Create;

var o := new TOther;

PrintLn(TBase.ClassName);

type TSub = class(TBase);