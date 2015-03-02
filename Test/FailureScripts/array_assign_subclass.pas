type
  TFrame = class
  end;

type
  TFrameArr = array of TFrame;

procedure TestArr(arr: TFrameArr);
begin
end;

var
  arr: array of TObject;

TestArr(arr);
