var k := TKernel.Create;
var in1 := k.AddInput('in1');
var pool := k.AddGlobalAvgPool(in1);
k.MarkOutput(pool);

var b1 := TStridedBuffer.Create(TDataType.Float32, [2, 3]);
var b2 := TStridedBuffer.Create(TDataType.Float32, [2, 3]);

b1.SetData([0, 0], -10); b1.SetData([0, 1], 5); b1.SetData([0, 2], 15);
b1.SetData([1, 0], -5);  b1.SetData([1, 1], 10); b1.SetData([1, 2], 15);

TKernelCompiler.Dispatch(k, [b1, b2]);

for var y := 0 to 1 do begin
   for var x := 0 to 2 do
      Print(FloatToStr(b2.GetData([y, x])) + ' ');
   PrintLn('');
end;
