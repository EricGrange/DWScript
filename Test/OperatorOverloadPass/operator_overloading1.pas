function StrPlusInt(s : String; i : Integer) : String;
begin
   Result:=s+'['+IntToStr(i)+']';
end;

operator + (String, Integer) : String uses StrPlusInt;

PrintLn('abc'+123);

var s := 'abc'+123+456;

PrintLn(Length(s));
PrintLn(s);