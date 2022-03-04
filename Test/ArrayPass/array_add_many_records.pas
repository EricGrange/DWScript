type 
    TRec = record
        a, b : Integer;
    end;
    
var a : array of TRec;

a.SetLength(1);
a[0].a := 1;
a[0].b := 2;

a.Add(a[0], a[0]);

a[1].b := 3;
a[2].a := 4;

a.Add(a[0], a[1], a[2]);

var s := [a[2], a[1]];
a.Add(s);

for var r in a do
    PrintLn(r.a.ToString + ',' + r.b.ToString);