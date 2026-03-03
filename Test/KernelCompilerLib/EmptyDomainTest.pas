var k := TKCLKernel.Create;
var in1 := k.AddInput('in1');
var in2 := k.AddInput('in2');
var sum := k.AddAdd(in1, in2);
k.MarkOutput(sum);

var b1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 0, 3]);
var b2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 0, 3]);
var b3 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 0, 3]);

// Dispatch with 0 volume to ensure graceful exit without memory violations
TKCLKernelCompiler.Dispatch(k, [b1, b2, b3]);

PrintLn('Success');
