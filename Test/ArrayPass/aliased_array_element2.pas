type ByteTyp = Integer;

var arr : array of integer;
var arr2DimB : array of array of ByteTyp;
var arr2Dim : array of array of integer;

function GetByteType : ByteTyp;
begin
  Result := 99;
end;

arr := [ GetByteType(), $13BE, $0487, 567];
PrintLn(arr.Map(IntToStr).Join(','));

arr2DimB.SetLength(1);
arr2DimB[0] := [GetByteType(), $13BF, $0488, 566];
PrintLn(arr2DimB[0].Map(IntToStr).Join(','));

arr2Dim.SetLength(1);
arr2Dim[0] := [GetByteType(), $13B, $0489, 565];
PrintLn(arr2Dim[0].Map(IntToStr).Join(','));