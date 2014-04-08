type TRecord = record X : Float; end;

procedure Test(const x: Float; var r: Float); overload;
begin
  r := x;
end;

procedure Test(const x: Float; var r: TRecord); overload;
var
  temp: Float;
begin
  Test(x, temp);
  r.X := temp;
end;

var r : TRecord;
Test(123, r);

PrintLn(r.X);