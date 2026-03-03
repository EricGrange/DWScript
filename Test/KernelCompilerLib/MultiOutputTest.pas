var k := TKernel.Create;
var in1 := k.AddInput('in1');
var in2 := k.AddInput('in2');
var sum := k.AddAdd(in1, in2);
var mul := k.AddMul(in1, in2);
k.MarkOutput(sum);
k.MarkOutput(mul);

var b1 := TStridedBuffer.Create(TDataType.Float32, [2]);
var b2 := TStridedBuffer.Create(TDataType.Float32, [2]);
var outSum := TStridedBuffer.Create(TDataType.Float32, [2]);
var outMul := TStridedBuffer.Create(TDataType.Float32, [2]);

b1.SetData([0], 2); b1.SetData([1], 3);
b2.SetData([0], 4); b2.SetData([1], 5);

TKernelCompiler.Dispatch(k, [b1, b2, outSum, outMul]);

PrintLn(outSum.GetData([0]));
PrintLn(outSum.GetData([1]));
PrintLn(outMul.GetData([0]));
PrintLn(outMul.GetData([1]));
