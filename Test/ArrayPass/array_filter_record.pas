type TRec = record x, y : Integer; end;

var a : array of TRec;

a.SetLength(5);

a[0].x := 1;
a[0].y := 2;

a[1].x := 3;
a[1].y := 4;

a[2].x := 5;
a[2].y := 6;

a[3].x := 7;
a[3].y := 8;

a[4].x := 9;
a[4].y := 10;

PrintLn(a.Filter(lambda (e) 
            Result := (e.x >= 3) and (e.y < 8);
            if Result  then
                PrintLn(e.x.ToString + ',' + e.y.ToString);
        end).Length);
        
PrintLn(a.Filter(lambda(e) => Odd(e.x)).Filter(lambda(e) => not Odd(e.y)).Length);