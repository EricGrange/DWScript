{

Special DWSII Syntax:

It's possible to declare variables everywhere in
the script code. The declaration is valid inside the
active block and its sub-blocks.

}

var x: Integer;
var s: Integer;

for x := 0 to 10 do
begin
  var s: string; // overrides previous declaration 
		     // of "s" (only inside this loop)
  s := IntToStr(x);
  PrintLn(s);
end;

for x := 0 to 10 do
begin
  var s: Float; // overrides previous declaration 
                // of "s" (only inside this loop)
  s := Sqrt(x);
  PrintLn(FloatToStr(s, 3));
end;

for x := 0 to 10 do
begin
  s := Round(Sqr(x)); // No redeclaration of "s" so
                // the initial declaration is used
  PrintLn(s);
end;


