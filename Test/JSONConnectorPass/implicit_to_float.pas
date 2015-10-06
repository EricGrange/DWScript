var j := JSON.Parse('{"Name":1.5}');

procedure Stuff1(a : Float);
begin
   PrintLn(a);
end;

procedure Stuff2(const a : Variant);
begin
   PrintLn(Float(a));
end;

var s : Float;

s := j.Name;

PrintLn(s);

PrintLn(j.Name);
Stuff1(j.Name);
Stuff2(j.Name);
