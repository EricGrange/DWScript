type
  TFoo = record
    X, Y: Integer;
  end;

function FooImplInt(I: Integer): TFoo;
begin
  Result.X := I;
  Result.Y := I+1;
end;

operator implicit (Integer): TFoo uses FooImplInt;

var
  F1: TFoo;

F1 := 10;
PrintLn('F1 X=' + F1.X.ToString + ' Y=' + F1.Y.ToString);

var F2 : TFoo = 20;
PrintLn('F2 X=' + F2.X.ToString + ' Y=' + F2.Y.ToString);  