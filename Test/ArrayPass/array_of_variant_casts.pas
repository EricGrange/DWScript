type TData1 = array [0..0] of Variant;
type TData2 = array [0..1] of Variant;
var a1 : array of TData1;
var a2 : array of TData2;
var b : array of Variant;

a1.Add([1], [1.5], ['two'], [False], [Null]);
a2.Add([1,2], [1.5,2.5], ['two','three'], [False,Null], [Null,0]);
b.Add(1, 1.5, 'two', False, Null);

for var i := 0 to a1.High do begin
   Print(a1[i][0]);
   Print('; ');
   Print(a2[i][0]);
   Print(',');
   Print(a2[i][1]);
   Print('; ');
   PrintLn(b[i]);
end;

a1.Reverse;
a2.Reverse;
b.Reverse;

for var i := 0 to a1.High do begin
   Print(a1[i][0]);
   Print('; ');
   Print(a2[i][0]);
   Print(',');
   Print(a2[i][1]);
   Print('; ');
   PrintLn(b[i]);
end;