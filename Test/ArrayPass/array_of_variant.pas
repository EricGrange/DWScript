var stat : array [0 .. 3] Of Variant;
var dyn : array of Variant;
var i : Integer;

stat[0]:=1;
stat[1]:='two';
stat[2]:=False;
stat[3]:=3.14;

for i:=stat.Low to stat.High do
   PrintLn(stat[i]);
   
dyn.Add(1);
dyn.SetLength(4);
dyn[1]:='two';
dyn[2]:=False;
dyn[3]:=3.14;

for i:=dyn.Low to dyn.High do
   PrintLn(dyn[i]);
