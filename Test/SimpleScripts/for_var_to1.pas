for var i:=1 to 3 do
   PrintLn(i);
   
for var i:=10 downto i-2 do
   PrintLn(i);   
   
type
   TEnum = (enumOne, enumeTwo);
  
const
   c : array [TEnum] of String = ['one', 'two'];

var e := enumOne;
   
for var f:=Low(TEnum) to High(TEnum) do
   PrintLn(c[f]);
   
PrintLn(c[e]);
   
  