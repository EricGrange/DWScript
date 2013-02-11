const f : Float = 1; // OK

Type
TObj = Class
 f1 : Float := 1;
 f2 := f;
End;

Type
TRec = Record
 f1 : Float := 2;
 f2 := f+f;
End;

PrintLn(FloatToStr(TObj.Create.f1));
PrintLn(FloatToStr(TObj.Create.f2));

var r : TRec;

PrintLn(FloatToStr(r.f1));
PrintLn(FloatToStr(r.f2));
