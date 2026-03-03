var k := TKCLKernel.Create;
var in1 := k.AddInput('in1');
var map := k.AddMul(in1, in1); // square it
k.MarkOutput(map);

var b1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 2, 2, 2, 2]); // 5D, 32 elements
var out1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 2, 2, 2, 2]);

b1.SetData([1, 0, 1, 0, 1], 5);

TKCLKernelCompiler.Dispatch(k, [b1, out1]);

PrintLn(FloatToStr(out1.GetData([1, 0, 1, 0, 1]))); // Should be 25
PrintLn(FloatToStr(out1.GetData([0, 0, 0, 0, 0]))); // Should be 0
