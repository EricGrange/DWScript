type TBase = class end;
type TChild = class(TBase) end;

var a : array of TBase;
var b : TBase;
var c : TChild;

a.Add(TChild.Create);

for b in a do
	PrintLn(b.ClassName);
	
for c in a do
	PrintLn(b.ClassName);	