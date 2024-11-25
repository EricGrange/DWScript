type TRec = record F : Integer end;

var a : array of TRec;
a.SetLength(4);

for var i := 0 to a.High do a[i].F := i;

for var i := 0 to a.High do begin
	var r : TRec;
	r.F := i;
	PrintLn(a.IndexOf(r, 2));
end;


type TRec2 = record D, F : Integer end;

var a2 : array of TRec2;
a2.SetLength(4);

for var i := 0 to a2.High do a2[i].F := i;

for var i := 0 to a2.High do begin
	var r2 : TRec2;
	r2.F := i;
	PrintLn(a2.IndexOf(r2, 2));
end;
