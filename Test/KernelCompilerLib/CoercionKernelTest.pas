var k := TKernel.Create;
var in1 := k.AddInput('in1');
var in2 := k.AddInput('in2');
var add := k.AddAdd(in1, in2);
k.MarkOutput(add);

var b1 := TStridedBuffer.Create(TDataType.Int8, [2]);
var b2 := TStridedBuffer.Create(TDataType.Int8, [2]);
var outBuf := TStridedBuffer.Create(TDataType.Float16, [2]);

b1.SetData([0], 10);
b1.SetData([1], -5);

b2.SetData([0], 15);
b2.SetData([1], 10);

TKernelCompiler.Dispatch(k, [b1, b2, outBuf]);

PrintLn(FloatToStr(outBuf.GetData([0])));
PrintLn(FloatToStr(outBuf.GetData([1])));
