var k := TKCLKernel.Create;
var in1 := k.AddInput('in1');
var in2 := k.AddInput('in2');
var in3 := k.AddInput('in3');
var sum1 := k.AddAdd(in1, in2);
var total := k.AddAdd(sum1, in3);
k.MarkOutput(total);

var b1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2]);

b1.SetData([0], 5);
b1.SetData([1], 10);

// Stress test in-place aliasing: passing the exact same buffer 4 times
TKCLKernelCompiler.Dispatch(k, [b1, b1, b1, b1]);

PrintLn(FloatToStr(b1.GetData([0]))); // 5 + 5 + 5 = 15
PrintLn(FloatToStr(b1.GetData([1]))); // 10 + 10 + 10 = 30
