var a : array [TObject] of array of Integer;

var o1 := new TObject;
var o2 := new TObject;

var a1 : array of Integer = [1, 2];
var a2 : array of Integer = [1, 2, 3];

a[o1] := a1;
a[o2] := a2;

PrintLn(a.Length);

PrintLn(a[o1].Map(IntToStr).Join(','));
PrintLn(a[o2].Map(IntToStr).Join(','));

a2.Add(4);

PrintLn(a[o1].Map(IntToStr).Join(','));
PrintLn(a[o2].Map(IntToStr).Join(','));

a[o1] := a2;
a[o2] := [];

PrintLn(a[o1].Map(IntToStr).Join(','));
PrintLn(a[o2].Length);
PrintLn(a[nil].Length);
