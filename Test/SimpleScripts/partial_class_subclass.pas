type 
	TBase = partial class
	end;
	
type
	TSub = class(TBase)
		SubField : Integer;
		function SubTest : Integer; virtual; abstract;
	end;
	
type
	TBase = partial class
		BaseField : Integer;
		function BaseTest : Integer; virtual; abstract;
	end;
	
type
	TSubSub = class(TSub)
		function SubTest : Integer; override;
		begin 
			Result:=SubField; 
		end;
		function BaseTest : Integer; override;
		begin 
			Result:=BaseField; 
		end;
	end;
	
var o := new TSubSub;

o.BaseField:=1;	
o.SubField:=2;

PrintLn(o.BaseField);
PrintLn(o.SubField);

PrintLn(o.BaseTest);
PrintLn(o.SubTest);
	