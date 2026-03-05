var k := TKCLKernel.Create;
var in1 := k.AddInput('in1');

// 2x2 input
// 1 2
// 3 4
var b1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 2, 1]);
var inArr : array of Float;
inArr.SetLength(4);
inArr[0] := 1; inArr[1] := 2;
inArr[2] := 3; inArr[3] := 4;
b1.SetData(inArr);

// 2x2 kernel, stride 2 -> output 4x4
// weights: all 1s
var weights : array of Float;
weights.SetLength(4);
for var i := 0 to 3 do weights[i] := 1.0;

var bias : array of Float;
bias.SetLength(1);
bias[0] := 0.0;

var conv := k.AddConv2DTranspose(in1, weights, bias, 2, 2);
k.MarkOutput(conv);

var b2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4, 1]);

TKCLKernelCompiler.Dispatch(k, [b1, b2]);

var outArr : array of Float;
b2.GetData(outArr);

for var y := 0 to 3 do begin
   for var x := 0 to 3 do
      Print(FloatToStr(outArr[y*4 + x]) + ' ');
   PrintLn('');
end;
