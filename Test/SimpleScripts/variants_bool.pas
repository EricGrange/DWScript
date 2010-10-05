const ctrue = True;
const cfalse : Boolean = False;
var vtrue = not cfalse;
var vfalse : Boolean = not vtrue;

var v : Variant;

v:=cfalse;

PrintLn(v or vtrue);
PrintLn(vfalse or v);

v:=v xor ctrue;

PrintLn(v or vtrue);
PrintLn(vfalse or v);
PrintLn(v and vtrue);
PrintLn(vfalse and v);
