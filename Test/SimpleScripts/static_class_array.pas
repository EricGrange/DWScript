type
  StaticClass = static class
    class var Values: array [0..2] of array [0..1] of Integer = [
      [0, 1],
      [2, 3],
      [4, 5]
    ];
    class const CValues: array [0..2] of array [0..1] of Integer = [
      [9, 8],
      [7, 6],
      [5, 4]
    ];
  end;      

for var Index := 0 to High(StaticClass.Values) do begin
  Print(IntToStr(StaticClass.Values[Index][0]));
  PrintLn(IntToStr(StaticClass.Values[Index][1]));
 end;
  
for var Index := 0 to High(StaticClass.CValues) do begin
  Print(IntToStr(StaticClass.CValues[Index][0]));
  PrintLn(IntToStr(StaticClass.CValues[Index][1]));
 end;
