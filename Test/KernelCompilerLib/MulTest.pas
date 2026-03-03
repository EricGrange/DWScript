var k := TKernel.Create;
var in1 := k.AddInput('in1');
var in2 := k.AddInput('in2');
var res := k.AddMul(in1, in2);
k.MarkOutput(res);

var b1 := TStridedBuffer.Create(TDataType.Float32, [5]);
var b2 := TStridedBuffer.Create(TDataType.Float32, [5]);
var b3 := TStridedBuffer.Create(TDataType.Float32, [5]);

b1.SetData([0], 2.5); b1.SetData([1], -3.5); b1.SetData([2], 0); b1.SetData([3], 100); b1.SetData([4], 0.1);
b2.SetData([0], 4.0); b2.SetData([1], 2.0); b2.SetData([2], 500); b2.SetData([3], -0.5); b2.SetData([4], 10);

TKernelCompiler.Dispatch(k, [b1, b2, b3]);

for var i := 0 to 4 do
   PrintLn(FloatToStr(b3.GetData([i])));
