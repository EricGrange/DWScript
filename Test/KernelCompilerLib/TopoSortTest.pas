var k := TKCLKernel.Create;
var in1 := k.AddInput('in1');
var in2 := k.AddInput('in2');

// Although added in topological order, we can test that evaluation happens in the correct order internally.
var mulNode := k.AddMul(in1, in2);
var addNode := k.AddAdd(mulNode, in1);

k.MarkOutput(addNode);

var b1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2]);
var b2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2]);
var out := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2]);

b1.SetData([0], 2); b1.SetData([1], 3);
b2.SetData([0], 4); b2.SetData([1], 5);

TKCLKernelCompiler.Dispatch(k, [b1, b2, out]);

PrintLn(out.GetData([0]));
PrintLn(out.GetData([1]));
