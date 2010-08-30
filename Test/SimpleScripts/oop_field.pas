type
   TMyClass = class
      Field : Float;
      constructor Create(var value : Float);
   end;

constructor TMyClass.Create(var value : Float);
begin
   Field:=value;
   value:=value/2;
end;

var f : Float = 1+0.5;
var o : TMyClass = TMyClass.Create(f);
var o2 : TMyClass;

o2:=o;
PrintLn(FloatToStr(o.Field));

o2:=TMyClass.Create(f);

PrintLn(FloatToStr(o2.Field));
PrintLn(FloatToStr(o.Field));

