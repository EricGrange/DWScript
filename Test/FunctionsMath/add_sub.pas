var i, j : Integer;
j:=0;

i:=j-(-1);
PrintLn(i);
i:=j-(-1)-(-1);
PrintLn(i);
i:=(-1)-j-(-1);
PrintLn(i);

i:=j+(-1);
PrintLn(i);
i:=j+(-1)+(-1);
PrintLn(i);
i:=(-1)+j+(-1);
PrintLn(i);

procedure Stuff;
begin
        var i, j : Integer;
        j:=0;

        i:=j-(-1);
        PrintLn(i);
        i:=j-(-1)-(-1);
        PrintLn(i);
        i:=(-1)-j-(-1);
        PrintLn(i);

        i:=j+(-1);
        PrintLn(i);
        i:=j+(-1)+(-1);
        PrintLn(i);
        i:=(-1)+j+(-1);
        PrintLn(i);
end;

Stuff;