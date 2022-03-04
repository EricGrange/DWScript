type TFrame = class end;

type TSubFrame = class (TFrame) end;

type TObjectArr = array of TObject;

procedure TestArr(arr: TObjectArr);
begin
	for var o in arr do
		PrintLn(o.ClassName);
end;

var
  arr: array of TFrame;

arr.Add(TFrame.Create);
arr.Add(TSubFrame.Create);
  
TestArr(arr);
