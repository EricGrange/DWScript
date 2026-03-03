var k := TKCLKernel.Create;
var in1 := k.AddInput('in1');
var in2 := k.AddInput('in2');
var add := k.AddAdd(in1, in2);
k.MarkOutput(add);

var b1 := TKCLStridedBuffer.Create(TKCLDataType.Int8, [2]);
var b2 := TKCLStridedBuffer.Create(TKCLDataType.Int8, [2]);
var outBuf := TKCLStridedBuffer.Create(TKCLDataType.Float16, [2]);

b1.SetData([0], 10);
b1.SetData([1], -5);

b2.SetData([0], 15);
b2.SetData([1], 10);

TKCLKernelCompiler.Dispatch(k, [b1, b2, outBuf]);

PrintLn(FloatToStr(outBuf.GetData([0])));
PrintLn(FloatToStr(outBuf.GetData([1])));
