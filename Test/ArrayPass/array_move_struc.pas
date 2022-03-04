type T = array [0..1] of String;
var a : array of T;

a.Add(['a', 'A'], ['b', 'B'], ['c', 'C']);

procedure PrintIt;
begin
    for var i := 0 to a.High do begin
        if i > 0 then Print(',');
        Print(a[i][0]+a[i][1]);
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


