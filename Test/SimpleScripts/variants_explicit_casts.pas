var v : Variant;
var s : String;
var i : Integer;
var f : Float;

v := '123';

s := String(v)+String(v);
PrintLn(s);
i := Integer(v)+4;
PrintLn(i);

v := 123;

s := String(v)+String(v);
PrintLn(s);
i := Integer(v)+4;
PrintLn(i);

v := 123.4;
s := String(v)+String(v);
PrintLn(s);
i := Integer(v)+1;
PrintLn(i);
f := Float(v)+1;
PrintLn(f);