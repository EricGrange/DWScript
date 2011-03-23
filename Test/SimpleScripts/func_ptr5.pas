type
   TMyFunc = function (i : Integer) : String;

type
   TMyClass = class
      FOffset : Integer;
      class function Conv(i : Integer) : String;
      function DoConv(i : Integer) : String;
   end;

class function TMyClass.Conv(i : Integer) : String;
begin
   Result:='<'+IntToStr(i)+'> '+ClassName;
end;

function TMyClass.DoConv(i : Integer) : String;
begin
   Result:='['+IntToStr(i+FOffset)+']';
end;

function MyIntToStr(i : Integer) : String;
begin
   Result:='*'+IntToStr(i)+'*';
end;

procedure PrintIt(f : TMyFunc; arg : Integer);
begin
   PrintLn(f(arg));
end;

var o := TMyClass.Create;
o.FOffset:=5000;

PrintIt(IntToStr, 123);
PrintIt(MyIntToStr, 123);
PrintIt(TMyClass.Conv, 123);
PrintIt(o.DoConv, 123);

var ff : TMyFunc;

ff:=IntToStr;
PrintIt(ff, 456);
ff:=MyIntToStr;
PrintIt(ff, 456);
ff:=TMyClass.Conv;
PrintIt(ff, 456);
ff:=o.DoConv;
PrintIt(ff, 456);
