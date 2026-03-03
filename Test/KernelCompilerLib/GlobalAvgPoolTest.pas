var k := TKernel.Create;
var in1 := k.AddInput('in1');
var pool := k.AddGlobalAvgPool(in1);
k.MarkOutput(pool);

var b1 := TStridedBuffer.Create(TDataType.Float32, [2, 2]);
var b2 := TStridedBuffer.Create(TDataType.Float32, [2, 2]);

var y, x : Integer;
for y := 0 to 1 do
   for x := 0 to 1 do
      b1.SetData([y, x], y * 2 + x); // 0, 1, 2, 3 -> sum is 6, avg is 1.5

TKernelCompiler.Dispatch(k, [b1, b2]);

for y := 0 to 1 do begin
   for x := 0 to 1 do
      Print(FloatToStr(b2.GetData([y, x])) + ' ');
   PrintLn('');
end;
