var k := TKernel.Create;
var in1 := k.AddInput('in1');
var in2 := k.AddInput('in2');
var in3 := k.AddInput('in3');
var sum := k.AddAdd(in1, in2);
var res := k.AddMul(sum, in3); // (in1 + in2) * in3
k.MarkOutput(res);

var b1 := TStridedBuffer.Create(TDataType.Float32, [2]);
var b2 := TStridedBuffer.Create(TDataType.Float32, [2]);
var b3 := TStridedBuffer.Create(TDataType.Float32, [2]);
var out := TStridedBuffer.Create(TDataType.Float32, [2]);

b1.SetData([0], 1); b1.SetData([1], 2);
b2.SetData([0], 3); b2.SetData([1], 4);
b3.SetData([0], 10); b3.SetData([1], 100);

TKernelCompiler.Dispatch(k, [b1, b2, b3, out]);

PrintLn(out.GetData([0]));
PrintLn(out.GetData([1]));
