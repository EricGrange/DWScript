type TTest = (one, two, three);

var a := [one..three, one, three..one];

for var i in a do
	PrintLn(i.Name);