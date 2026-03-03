var k := TKernel.Create;
var in1 := k.AddInput('in1');
var in2 := k.AddInput('in2');
var sum := k.AddAdd(in1, in2);
k.MarkOutput(sum);

// Dispatch 1: 1D array of 3 elements
var b1_1 := TStridedBuffer.Create(TDataType.Float32, [3]);
var b1_2 := TStridedBuffer.Create(TDataType.Float32, [3]);
var out1 := TStridedBuffer.Create(TDataType.Float32, [3]);
b1_1.SetData([0], 1); b1_1.SetData([1], 2); b1_1.SetData([2], 3);
b1_2.SetData([0], 10); b1_2.SetData([1], 20); b1_2.SetData([2], 30);

TKernelCompiler.Dispatch(k, [b1_1, b1_2, out1]);

// Dispatch 2: 2D array of 2x2 elements (Statelessness test)
var b2_1 := TStridedBuffer.Create(TDataType.Float32, [2, 2]);
var b2_2 := TStridedBuffer.Create(TDataType.Float32, [2, 2]);
var out2 := TStridedBuffer.Create(TDataType.Float32, [2, 2]);
b2_1.SetData([0, 0], 100); b2_1.SetData([0, 1], 200);
b2_1.SetData([1, 0], 300); b2_1.SetData([1, 1], 400);

b2_2.SetData([0, 0], 5); b2_2.SetData([0, 1], 5);
b2_2.SetData([1, 0], 5); b2_2.SetData([1, 1], 5);

TKernelCompiler.Dispatch(k, [b2_1, b2_2, out2]);

// Print Dispatch 1
PrintLn(FloatToStr(out1.GetData([0])));
PrintLn(FloatToStr(out1.GetData([1])));
PrintLn(FloatToStr(out1.GetData([2])));

// Print Dispatch 2
PrintLn(FloatToStr(out2.GetData([0, 0])));
PrintLn(FloatToStr(out2.GetData([0, 1])));
PrintLn(FloatToStr(out2.GetData([1, 0])));
PrintLn(FloatToStr(out2.GetData([1, 1])));
