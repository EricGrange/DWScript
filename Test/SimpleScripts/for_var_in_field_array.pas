type 
	TList = class
		Field : array of String;
	end;
		
type
	TContainer = class
		A : array of TList;
	end;
	
var c := new TContainer;
var i := new TList;
i.Field.Add('a', 'b');
c.A.Add(i);
i:=new TList;
i.Field.Add('c', 'd', 'e');
c.A.Add(i);

for var list in c.A do
   for var s in list.Field do
	   PrintLn(s);