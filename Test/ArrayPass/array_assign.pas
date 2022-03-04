var a, b : array [0..2] of Integer;
b:=[1, 2, 3];
a:=b;

PrintLn(Length(a));
Print(a[0]);
Print(a[1]);
PrintLn(a[2]);

b[1]:=0;

Print(a[0]);
Print(a[1]);
PrintLn(a[2]);

a:=[4, 5, 6];

Print(a[0]);
Print(a[1]);
PrintLn(a[2]);
Print(b[0]);
Print(b[1]);
PrintLn(b[2]);

