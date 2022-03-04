type TIntegers = array of Integer;

var a1, a2 : TIntegers;
var i : Integer;

a1:=[1,2,3];
for i:=a1.Low to a1.High do
	Print(a1[i]);
PrintLn(a1.Length);

a2:=a1.Copy(1);
for i:=a2.Low to a2.High do
	Print(a2[i]);
PrintLn(a2.Length);

a2:=a1.Copy;
for i:=a2.Low to a2.High do
	Print(a2[i]);
PrintLn(a2.Length);

try
   a2:=a1.Copy(3, 0);
except
   on e: Exception do PrintLn(e.Message);
end;

try
   a2:=a1.Copy(1, -1);
except
   on e: Exception do PrintLn(e.Message);
end;

try
   a2:=a1.Copy(0, 4);
   PrintLn(a2.Map(IntToStr).Join(','));
except
   on e: Exception do PrintLn(e.Message);
end;

try
   a2:=a1.Copy(-1, 2);
except
   on e: Exception do PrintLn(e.Message);
end;
