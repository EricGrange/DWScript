operator implicit (Integer) : String uses IntToStr;

var s : String;

s := 123;

PrintLn(s);

function MyFloatToStr(f : Float) : String;
begin
   Result := f.ToString(2);
end;

operator implicit (Float) : String uses MyFloatToStr;

s := Pi;

PrintLn(s);

