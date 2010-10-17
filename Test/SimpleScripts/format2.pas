procedure SetXY(const AX, AY, AValue : Integer);
begin
   PrintLn(Format('X = %d, Y = %d, Value = %d', [AX, AY, AValue]));
end;

SetXY(100, 100, StrToInt('12'));
