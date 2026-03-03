var k := TKernel.Create;
var in1 := k.AddInput('in1');
var sum := k.AddAdd(in1, in1);
k.MarkOutput(sum);

var b1 := TStridedBuffer.Create(TDataType.Float32, [2, 2]);
var b2 := TStridedBuffer.Create(TDataType.Float32, [2, 2]);

b1.SetData([0, 0], 1);
b1.SetData([0, 1], 2);
b1.SetData([1, 0], 3);
b1.SetData([1, 1], 4);

TKernelCompiler.Dispatch(k, [b1, b2]);

PrintLn(b2.GetData([0, 0]));
PrintLn(b2.GetData([0, 1]));
PrintLn(b2.GetData([1, 0]));
PrintLn(b2.GetData([1, 1]));
