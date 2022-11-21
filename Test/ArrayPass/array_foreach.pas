var a : array of Integer;

var s := 0;
var x :=  0;

procedure SumIt(v : Integer); begin s += v; end;
procedure XorIt(v : Integer); begin x := x xor v; end;

a.ForEach(SumIt);
a.ForEach(XorIt);

PrintLn(s);
PrintLn(x);

a.Add(1, 2, 7);

a.ForEach(SumIt);
a.ForEach(XorIt);

PrintLn(s);
PrintLn(x);

