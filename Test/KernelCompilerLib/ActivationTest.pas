var k := TKernel.Create;
var in1 := k.AddInput('in1');

var relu := k.AddReLU(in1);
var relu6 := k.AddReLU6(in1);
var hs := k.AddHardSwish(in1);

k.MarkOutput(relu);
k.MarkOutput(relu6);
k.MarkOutput(hs);

var b_in := TStridedBuffer.Create(TDataType.Float32, [5]);
var b_relu := TStridedBuffer.Create(TDataType.Float32, [5]);
var b_relu6 := TStridedBuffer.Create(TDataType.Float32, [5]);
var b_hs := TStridedBuffer.Create(TDataType.Float32, [5]);

// Test values: -5.0, -1.5, 0.0, 3.0, 10.0
b_in.SetData([0], -5.0);
b_in.SetData([1], -1.5);
b_in.SetData([2], 0.0);
b_in.SetData([3], 3.0);
b_in.SetData([4], 10.0);

TKernelCompiler.Dispatch(k, [b_in, b_relu, b_relu6, b_hs]);

PrintLn('--- ReLU ---');
for var i := 0 to 4 do PrintLn(FloatToStr(b_relu.GetData([i])));

PrintLn('--- ReLU6 ---');
for var i := 0 to 4 do PrintLn(FloatToStr(b_relu6.GetData([i])));

PrintLn('--- HardSwish ---');
for var i := 0 to 4 do PrintLn(FloatToStr(b_hs.GetData([i])));
