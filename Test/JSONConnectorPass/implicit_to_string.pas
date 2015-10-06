var j := JSON.Parse('{"Name":"abc"}');

procedure Stuff1(a : Variant);
begin
   PrintLn(a);
end;

procedure Stuff2(const a : Variant);
begin
   PrintLn(a);
end;

var s : String;

s := j.Name;

PrintLn(s);

PrintLn(j.Name);
Stuff1(j.Name);
Stuff2(j.Name);
