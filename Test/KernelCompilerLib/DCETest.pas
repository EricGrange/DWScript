var k := TKernel.Create;
var in1 := k.AddInput('in1');
var in2 := k.AddInput('in2');

var sum1 := k.AddAdd(in1, in2); // used
var sum2 := k.AddMul(in1, in2); // not used, DCE should prune this

k.MarkOutput(sum1);

var b1 := TStridedBuffer.Create(TDataType.Float32, [2]);
var b2 := TStridedBuffer.Create(TDataType.Float32, [2]);
var out := TStridedBuffer.Create(TDataType.Float32, [2]);

b1.SetData([0], 1); b1.SetData([1], 2);
b2.SetData([0], 3); b2.SetData([1], 4);

TKernelCompiler.Dispatch(k, [b1, b2, out]);

PrintLn(out.GetData([0]));
PrintLn(out.GetData([1]));
