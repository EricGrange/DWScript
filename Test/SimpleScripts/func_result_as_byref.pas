//
// New Script
//
Function Func(Const AString : String) : Integer;
Begin

 Result := 0;

End;
//


Var i := 0;
Var a : Array [0 .. 255] Of Integer;

a[Func(IntToStr(i))] := 1;

For i := 1 to 1000 do
 Inc(a[Func(IntToStr(i)) And 255]);
 
For i := 0 to 4 do
 PrintLn(a[i]); 