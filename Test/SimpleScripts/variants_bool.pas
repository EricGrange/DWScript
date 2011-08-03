const ctrue = True;
const cfalse : Boolean = False;
var vtrue = not cfalse;
var vfalse : Boolean = not vtrue;

var v : Variant;

v:=True;
PrintLn(not v);
v:=not v;
PrintLn(not v);

v:=cfalse;

PrintLn(v or vtrue);
PrintLn(vfalse or v);

if v then PrintLn('Bug 1');

v:=v xor ctrue;

PrintLn(v or vtrue);
PrintLn(vfalse or v);
PrintLn(v and vtrue);
PrintLn(vfalse and v);

if v then else PrintLn('Bug 2');

while v do
   repeat
      Exit;
   until v;
   