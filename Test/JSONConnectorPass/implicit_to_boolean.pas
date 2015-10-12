var j := JSON.Parse('{"Name":true}');

procedure Stuff1(a : Variant);
begin
   PrintLn(a);
end;

procedure Stuff2(const a : Variant);
begin
   PrintLn(a);
end;

var s : Boolean;

s := j.Name;

PrintLn(s);

if j.Name then PrintLn(1) else PrintLn(0);
if not Boolean(j.Name) then PrintLn(1) else PrintLn(0);

PrintLn(j.Name);
Stuff1(j.Name);
Stuff2(j.Name);
