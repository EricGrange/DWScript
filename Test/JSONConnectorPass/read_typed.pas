var j := JSON.NewObject;

j.f := 1.5;
j.i := 2;

PrintLn(FloatToStr(j['f'], 2));
PrintLn(IntToHex(j['i'], 2));

