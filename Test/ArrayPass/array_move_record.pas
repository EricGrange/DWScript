type T = record x,y, z : String end;
var a : array of T;

a.SetLength(3);
a[0].x := 'a';   a[0].y := 'A';   a[0].z := '0';
a[1].x := 'b';   a[1].y := 'B';   a[1].z := '1';
a[2].x := 'c';   a[2].y := 'C';   a[2].z := '2';

procedure PrintIt;
begin
    for var i := 0 to a.High do begin
        if i > 0 then Print(',');
        Print(a[i].x+a[i].y+a[i].z);
    end;
    PrintLn('');
end;

PrintIt;  // a,b,c
a.Move(0, 2);
PrintIt;  // b,c,a
a.Move(2, 0);
PrintIt;  // a,b,c
a.Move(1, 2);
PrintIt;  // a,c,b
a.Move(1, 0);
PrintIt;  // c,a,b
a.Move(2, 1);
PrintIt;  // c,b,a


