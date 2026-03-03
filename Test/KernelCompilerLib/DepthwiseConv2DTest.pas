var k := TKCLKernel.Create;
var in1 := k.AddInput('in1');

var weights : array of Float;
weights.SetLength(18);
for var dy := 0 to 2 do
   for var dx := 0 to 2 do begin
      weights[(dy * 3 + dx) * 2 + 0] := 1.0;
      weights[(dy * 3 + dx) * 2 + 1] := 2.0;
   end;

var bias : array of Float;
bias.SetLength(2);
bias[0] := 10.0;
bias[1] := 20.0;

var conv := k.AddDepthwiseConv2D(in1, weights, bias, 3, 1);
k.MarkOutput(conv);

var b_in := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4, 2]);
var b_out := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4, 2]);

for var y := 0 to 3 do
   for var x := 0 to 3 do begin
      b_in.SetData([y, x, 0], 1.0);
      b_in.SetData([y, x, 1], 1.0);
   end;

TKCLKernelCompiler.Dispatch(k, [b_in, b_out]);

PrintLn('Ch0 0,0: ' + FloatToStr(b_out.GetData([0, 0, 0])));
PrintLn('Ch1 0,0: ' + FloatToStr(b_out.GetData([0, 0, 1])));
PrintLn('Ch0 0,1: ' + FloatToStr(b_out.GetData([0, 1, 0])));
PrintLn('Ch1 0,1: ' + FloatToStr(b_out.GetData([0, 1, 1])));
PrintLn('Ch0 1,1: ' + FloatToStr(b_out.GetData([1, 1, 0])));
PrintLn('Ch1 1,1: ' + FloatToStr(b_out.GetData([1, 1, 1])));
