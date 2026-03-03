var k := TKCLKernel.Create;
var in1 := k.AddInput('in1');
var in2 := k.AddInput('in2');
var in3 := k.AddInput('in3');
var sum1 := k.AddAdd(in1, in2);
var prod1 := k.AddMul(sum1, in3); // (in1 + in2) * in3
var sum2 := k.AddAdd(prod1, in1); // ((in1 + in2) * in3) + in1
k.MarkOutput(sum2);

var b1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [3]);
var b2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [3]);
var b3 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [3]);
var out := TKCLStridedBuffer.Create(TKCLDataType.Float32, [3]);

b1.SetData([0], 1); b1.SetData([1], 2); b1.SetData([2], -1);
b2.SetData([0], 3); b2.SetData([1], 4); b2.SetData([2], 5);
b3.SetData([0], 10); b3.SetData([1], -5); b3.SetData([2], 0.5);

TKCLKernelCompiler.Dispatch(k, [b1, b2, b3, out]);

for var i := 0 to 2 do
   PrintLn(out.GetData([i]));
