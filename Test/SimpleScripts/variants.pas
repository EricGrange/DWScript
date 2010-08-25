{

Demo: Datatype variant

}

function VariantDemo(v: Variant): Variant;
begin
  case v of
    1: Result := True;
    2: Result := 3.14159265358;
  end;
end;

var v: Variant;

v := VariantDemo (1);
PrintLn(v);

PrintLn(VariantDemo(2));

