var v: JSONVariant;

function ByParentI: String; begin Result:= IntToStr(v); end;
function ByParentF: String; begin Result:= FloatToStr(v); end;
function ByParentS: Integer; begin Result:= StrToInt(v); end;

v:= 1;
PrintLn(IntToStr(v));
v := 2;
PrintLn(ByParentI);
v := 3;
PrintLn(ByParentS);

v:= 1.5;
PrintLn(FloatToStr(v));
v:= 2.5;
PrintLn(ByParentF);
