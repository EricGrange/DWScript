var numbers: array of integer;
numbers.add(3);
numbers.add(0);
numbers.add(1);
numbers.add(2);
numbers.Delete(numbers.IndexOf(3));

var i : Integer;
for i:=0 to numbers.High do
   PrintLn(numbers[i]);
   
   
type TArray = array [0..1] of String;
var av : array of TArray;
var buf : TArray := ['a', 'alpha'];
av.Add(buf);
buf:=['b', 'beta'];
av.Add(buf);
buf:=['g', 'gamma'];
av.Add(buf);
av.Delete(0);

for i:=0 to av.High do
   PrintLn(av[i][0]+' - '+av[i, 1]);

buf := ['a', 'alpha'];
av.Add(buf);
av.Delete(0, 2);   

for i:=0 to av.High do
   PrintLn(av[i][0]+' - '+av[i, 1]);
