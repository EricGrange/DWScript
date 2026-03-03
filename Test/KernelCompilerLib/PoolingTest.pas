var k := TKCLKernel.Create;
var in1 := k.AddInput('in1'); // [1, 4, 4, 1]

var mp := k.AddMaxPool2D(in1, 2, 2); // [1, 2, 2, 1]
var gap := k.AddGlobalAvgPool(in1); // [1, 1, 1, 1]

k.MarkOutput(mp);
k.MarkOutput(gap);

var b_in := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 4, 4, 1]);
var b_mp := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 2, 2, 1]);
var b_gap := TKCLStridedBuffer.Create(TKCLDataType.Float32, [1, 1, 1, 1]);

// Fill input with 1..16
var x, y : Integer;
for y := 0 to 3 do
   for x := 0 to 3 do
      b_in.SetData([0, y, x, 0], y * 4 + x + 1);

TKCLKernelCompiler.Dispatch(k, [b_in, b_mp, b_gap]);

PrintLn('MaxPool:');
for y := 0 to 1 do
   for x := 0 to 1 do
      PrintLn(FloatToStr(b_mp.GetData([0, y, x, 0])));

PrintLn('GlobalAvgPool:');
PrintLn(FloatToStr(b_gap.GetData([0, 0, 0, 0])));
