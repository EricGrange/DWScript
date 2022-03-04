
type
 TTest = class
   FValues: array [0..12] of Integer;
   function GetValue(x: Integer): Integer;
   procedure SetValue(x, y: Integer);
   property Values[x: Integer]: Integer read GetValue write SetValue; default;
 end;
 
function TTest.GetValue(x: Integer): Integer;
begin
  Result := FValues[x];
end;

procedure TTest.SetValue(x, y: Integer);
begin
  FValues[x] := y;
end;

var x: Integer;
var t: TTest = TTest.Create;

for x := 0 to 10 do
begin
  t[x] := x;
  PrintLn(t[x]);
end;
