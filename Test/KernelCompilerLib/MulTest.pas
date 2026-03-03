var k := TKernel.Create;
var in1 := k.AddInput('in1');
var in2 := k.AddInput('in2');
var res := k.AddMul(in1, in2);
k.MarkOutput(res);

var b1 := TStridedBuffer.Create(TDataType.Float32, [2]);
var b2 := TStridedBuffer.Create(TDataType.Float32, [2]);
var b3 := TStridedBuffer.Create(TDataType.Float32, [2]);

b1.SetData([0], 2); b1.SetData([1], 3);
b2.SetData([0], 4); b2.SetData([1], 5);

TKernelCompiler.Dispatch(k, [b1, b2, b3]);

PrintLn(b3.GetData([0]));
PrintLn(b3.GetData([1]));
