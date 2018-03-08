type
  TFoo = record
    X, Y: Integer;
  end;

procedure PrintInt(i : Integer);
begin
    PrintLn(i);
end;
  
function OperImpFooInt(aFoo: TFoo): Integer;
begin
  Result := aFoo.X + aFoo.Y;
end;

operator implicit (TFoo): Integer uses OperImpFooInt;

var F: TFoo;
var i: Integer;

F.X := 10;
F.Y := 11;

PrintInt(F);

i := F;

PrintLn(i);


F.Y := 123;

PrintInt(F);

i := F;

PrintLn(i);
