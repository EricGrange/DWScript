type
    TRec = record
        a, b : String;
    end;
   
var a : array of TRec;
var r : TRec = (a:'-'; b:'-');

a.Insert(0, r);
r.a := '1';
a.Insert(0, r);
r.b := '2';
a.Insert(1, r);

for var rr in a do
    PrintLn(rr.a + rr.b);