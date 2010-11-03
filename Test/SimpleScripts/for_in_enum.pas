
type TMyEnum = (meA, meB, meC);

var i : TMyEnum;

for i in TMyEnum do
   Print(i);

PrintLn('');

for i:=Low(TMyEnum) to High(TMyEnum) do
   Print(i);
PrintLn('');

for i:=meA to meC do
   Print(i);
PrintLn('');

