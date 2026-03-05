var k := TKCLKernel.Create;
var in1 := k.AddInput('in1');

var weights : array of Float;
weights.SetLength(9);
for var i := 0 to 8 do weights[i] := 1.0;

var bias : array of Float;
bias.SetLength(1);
bias[0] := 0.0;

var conv := k.AddConv2D(in1, weights, bias, 3, 2);
k.MarkOutput(conv);

var b1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4, 1]);
var b2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 2, 1]);

var y, x : Integer;
for y := 0 to 3 do
   for x := 0 to 3 do
      b1.SetData([y, x, 0], 1.0);

TKCLKernelCompiler.Dispatch(k, [b1, b2]);

for y := 0 to 1 do begin
   for x := 0 to 1 do
      Print(FloatToStr(b2.GetData([y, x, 0])) + ' ');
   PrintLn('');
end;
