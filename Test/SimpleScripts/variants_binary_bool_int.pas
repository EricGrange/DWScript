var v1, v2 : Variant;

v1:=True;
v2:=False;

Print(v1 or v2);
PrintLn(not (v1 or v2));
Print(v1 and v2);
PrintLn(not (v1 and v2));
Print(v1 xor v2);
PrintLn(not (v1 xor v2));

v1:=7;
v2:=2;

Print(v1 or v2);
PrintLn(not (v1 or v2));
Print(v1 and v2);
PrintLn(not (v1 and v2));
Print(v1 xor v2);
PrintLn(not (v1 xor v2));
