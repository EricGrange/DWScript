type
   TMyFunc = function (i : Integer) : String;

procedure Func1(f : TMyFunc);
begin
   PrintLn('Func1');
   PrintLn(f(123));
end;

procedure Func2(f : TMyFunc);
begin
   PrintLn('Func2');
   Func1(f);
end;

Func2(IntToStr);