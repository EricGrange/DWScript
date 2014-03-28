var a := JSON.NewArray;

PrintLn(JSON.Stringify(a));

a.Add(1);

PrintLn(JSON.Stringify(a));

a.Add('two');

PrintLn(JSON.Stringify(a));

a.Add(True);
a.Add(3.5);
a.Add(nil);

PrintLn(JSON.Stringify(a));

a := JSON.NewArray;

var b := JSON.NewArray;
b.Add(1);
b.Add(Null);

var c := JSON.NewObject;
c["hello"]:='World';

a.Add(b);
a.Add(c);

PrintLn(JSON.Stringify(a));
