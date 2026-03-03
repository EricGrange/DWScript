var k := TKernel.Create;
var in1 := k.AddInput('in1');

var weights : array of Float;
weights.SetLength(9);
for var i := 0 to 8 do weights[i] := 1.0;

var bias : array of Float;
bias.SetLength(1);
bias[0] := 0.0;

var conv := k.AddConv2D(in1, weights, bias, 3, 1);
k.MarkOutput(conv);

var b1 := TStridedBuffer.Create(TDataType.Float32, [1, 5, 1]);
var b2 := TStridedBuffer.Create(TDataType.Float32, [1, 5, 1]);
b1.SetData([0, 0, 0], 10);
b1.SetData([0, 1, 0], 20);
b1.SetData([0, 2, 0], 30);
b1.SetData([0, 3, 0], 40);
b1.SetData([0, 4, 0], 50);

TKernelCompiler.Dispatch(k, [b1, b2]);

for var i := 0 to 4 do
   Print(FloatToStr(b2.GetData([0, i, 0])) + ' ');
PrintLn('');

var b3 := TStridedBuffer.Create(TDataType.Float32, [5, 1, 1]);
var b4 := TStridedBuffer.Create(TDataType.Float32, [5, 1, 1]);
b3.SetData([0, 0, 0], 10);
b3.SetData([1, 0, 0], 20);
b3.SetData([2, 0, 0], 30);
b3.SetData([3, 0, 0], 40);
b3.SetData([4, 0, 0], 50);

TKernelCompiler.Dispatch(k, [b3, b4]);

for var i := 0 to 4 do
   Print(FloatToStr(b4.GetData([i, 0, 0])) + ' ');
PrintLn('');
