type TTest = class
	private
		FNames : array of String;
	public
		property Names : array of String read FNames write FNames;
	end;

var a : array of String;
var t := new TTest;
t.Names := a;
a.Add('hello');
PrintLn(t.Names[0]);