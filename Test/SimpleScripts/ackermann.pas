function Ackermann(n, x, y: Integer): Integer;
begin
  if n = 0 then
    Result := x+1
  else if y = 0 then
  begin
    if n = 1 then Result := x
    else if n = 2 then Result := 0
    else if n = 3 then Result := 1
    else if n >= 4 then Result := 2
  end 
  else
    Result := Ackermann(n - 1, Ackermann(n, x, y - 1), x);
end;

function F(Input: Integer): Integer;
begin
  Result := Ackermann(Input, Input, Input);
end;

PrintLn(IntToStr(F(0)));
PrintLn(IntToStr(F(1)));
PrintLn(IntToStr(F(2)));
PrintLn(IntToStr(F(3)));
