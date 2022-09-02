var s : array of String;
var f : array of Float;
var i : array of Integer;
var b : array of Boolean;
var v : array of Variant;

s.Add('hello', 'world');
f.Add(1.5, 2.5);
i.Add(1, 2);
b.Add(False, True);

v.Add(s);
v.Add(f);
v.Add(i);
v.Add(b);

for var e in v do PrintLn(e);
