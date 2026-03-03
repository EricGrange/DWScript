var k := TKCLKernel.Create;
var in1 := k.AddInput('in1');

var weights : array of Float;
weights.SetLength(4); // 2 in_channels * 2 out_channels (1x1 kernel)
weights[0] := 1.0; // in0->out0
weights[1] := 2.0; // in0->out1
weights[2] := 3.0; // in1->out0
weights[3] := 4.0; // in1->out1

var bias : array of Float;
bias.SetLength(2);
bias[0] := 10.0;
bias[1] := 20.0;

// Stride 2 to downsample!
var conv := k.AddConv2D(in1, weights, bias, 1, 2);
k.MarkOutput(conv);

var b_in := TKCLStridedBuffer.Create(TKCLDataType.Float32, [4, 4, 2]);
var b_out := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 2, 2]); // Output gets downsampled to 2x2

for var y := 0 to 3 do
   for var x := 0 to 3 do begin
      b_in.SetData([y, x, 0], y + x);
      b_in.SetData([y, x, 1], (y + x) * 2);
   end;

TKCLKernelCompiler.Dispatch(k, [b_in, b_out]);

for var y := 0 to 1 do begin
   for var x := 0 to 1 do
      Print(FloatToStr(b_out.GetData([y, x, 0])) + ',' + FloatToStr(b_out.GetData([y, x, 1])) + ' ');
   PrintLn('');
end;
