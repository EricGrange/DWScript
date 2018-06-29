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

PrintLn('Booleans');

v := True;
PrintLn(Boolean(v));
v := False;
PrintLn(Boolean(v));

PrintLn('Booleans strings');

v := 'True';
PrintLn(Boolean(v));
v := 'true';
PrintLn(Boolean(v));
v := 'False';
PrintLn(Boolean(v));
v := 'false';
PrintLn(Boolean(v));

PrintLn('Booleans integer');

v := 1;
PrintLn(Boolean(v));
v := 0;
PrintLn(Boolean(v));
v := -1;
PrintLn(Boolean(v));