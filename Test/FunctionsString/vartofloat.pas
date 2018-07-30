var v : Variant = '1.5';

PrintLn(VarToFloatDef(v, 2.5));

v := Unassigned;
PrintLn(VarToFloatDef(v, 2.5));

v := Null;
PrintLn(VarToFloatDef(v, 2.5));

v := 'bug';
PrintLn(VarToFloatDef(v, 2.5));

v := '3,5';
PrintLn(VarToFloatDef(v, 2.5));

v := 4.5;
PrintLn(VarToFloatDef(v, 2.5));

v := False;
PrintLn(VarToFloatDef(v, 2.5));

v := True;
PrintLn(VarToFloatDef(v, 2.5));