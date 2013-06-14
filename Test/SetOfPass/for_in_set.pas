type TEnum = (one, two);
type TSet = set of TEnum;

var s : TSet = [one, two];

var e : TEnum;

for e in s do
   PrintLn(e.Name);
   
s.Exclude(one);

for e in s do
   PrintLn(e.Name);
   
s.Exclude(two);

for var i in s do
   PrintLn(e.Name);

s.Include(one);   
   
for var i in s do
   PrintLn(i.Name);
   
s.Include(two);   
   
for var i in s do
   PrintLn(i.Name);
   