type
  TRect = record
    Left: Integer;
    Top: Integer;
    Right: Integer;
    Bottom: Integer;
  end;
function Rect(const Left, Top, Right, Bottom: Integer): TRect;
begin
  Result.Left:= Left;
  Result.Top:= Top;
  Result.Right:= Right;
  Result.Bottom:= Bottom;
end;
function RectToStr(const R: TRect): String;
var
  X: Integer;
begin
  X:= R.Left + R.Top + R.Right + R.Bottom;
  Result:= IntToStr(X);
end;
PrintLn(RectToStr(Rect(50,40,30,20)));