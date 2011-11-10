const cMyVector : TVector = (X:1; Y:2; Z:3; W:4);

var v := Vector(5, 7, 11, 0);

PrintLn(VectorToStr(cMyVector));
PrintLn(VectorToStr(v));

v := v + cMyVector;

PrintLn(VectorToStr(v));

v := v - cMyVector;

PrintLn(VectorToStr(v));

v := cMyVector - v;

PrintLn(VectorToStr(v));

