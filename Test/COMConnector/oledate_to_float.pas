var t : Float = 42420.5;

var v : Variant;
v := t;
PrintLn(VarToFloatDef(v, 0));
PrintLn(Float(v));

v := OleDate(t);

PrintLn(VarToFloatDef(v, 0));
PrintLn(Float(v));

v := 'bug';

PrintLn(VarToFloatDef(v, 123));

