type TMyArray = array [0..5] of String;

var v : TMyArray;

v[0]:='123';

var i : Integer;
for i:=1 to 5 do
   v[i]:=Chr(i+Ord('A'));

for i:=Low(v) to High(v) do
   PrintLn(IntToStr(i)+': '+v[i]);
