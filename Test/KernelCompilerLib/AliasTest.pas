var k := TKCLKernel.Create;
var in1 := k.AddInput('in1');
var in2 := k.AddInput('in2');
var add := k.AddAdd(in1, in2);
k.MarkOutput(add);

var b1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2]);
var b2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2]);

b1.SetData([0], 10);
b1.SetData([1], 20);

b2.SetData([0], 1);
b2.SetData([1], 2);

TKCLKernelCompiler.Dispatch(k, [b1, b2, b1]);

PrintLn(FloatToStr(b1.GetData([0])));
PrintLn(FloatToStr(b1.GetData([1])));
