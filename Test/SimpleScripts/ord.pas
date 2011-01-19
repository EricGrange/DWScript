type TEnum = (eZero, eOne, eTwo);

PrintLn(Ord('A'));
PrintLn(Ord(''));
PrintLn(Ord(123));
PrintLn(Ord(eTwo));
PrintLn(Ord(True));
PrintLn(Ord(False));

var v : Variant;

v:=456;
PrintLn(Ord(v));

v:='a';
PrintLn(Ord(v));
v:='';
PrintLn(Ord(v));

v:=True;
PrintLn(Ord(v));
v:=False;
PrintLn(Ord(v));

v:=PI;
try
   PrintLn(Ord(v));
except
   on e : Exception do PrintLn(e.Message);
end;