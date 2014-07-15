type
   TTestEnum = (
      teOne,
      teTwo,
      teThree,
      teFour
   );

function Reverse1(const value: TTestEnum): TTestEnum;
const 
   cTestEnumReversed: array [0..3] of TTestEnum =
      (teFour, teThree, teTwo, teOne);
begin
   Result := cTestEnumReversed[Ord(value)];
end;

function Reverse2(const value: TTestEnum): TTestEnum;
const 
   cTestEnumReversed: array [TTestEnum] of TTestEnum =
      (teFour, teThree, teTwo, teOne);
begin
   Result := cTestEnumReversed[value];
end;

PrintLn(Reverse1(teOne) = teFour);
PrintLn(Reverse2(teOne) = teFour);