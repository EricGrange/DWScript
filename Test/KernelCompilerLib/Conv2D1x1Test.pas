var k := TKCLKernel.Create;
var in1 := k.AddInput('in1');

var weights : array of Float;
weights.SetLength(4); // inC=2, outC=2 -> 2*2 = 4
weights[0] := 1.0; weights[1] := 0.5; // k=0 (InC 0)
weights[2] := 0.1; weights[3] := 2.0; // k=1 (InC 1)

var bias : array of Float;
bias.SetLength(2);
bias[0] := 10.0;
bias[1] := 20.0;

// AddConv2D(input, weights, bias, kernelSize, stride)
var conv := k.AddConv2D(in1, weights, bias, 1, 1);
k.MarkOutput(conv);

var b1 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 2, 2]); // 2x2 pixels, 2 channels
var b2 := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 2, 2]);

// Set input data
b1.SetData([0, 0, 0], 1.0);

TKCLKernelCompiler.Dispatch(k, [b1, b2]);

for var y := 0 to 1 do
   for var x := 0 to 1 do
      for var c := 0 to 1 do
         PrintLn('(' + IntToStr(y) + ',' + IntToStr(x) + ',' + IntToStr(c) + ') = ' + FloatToStr(b2.GetData([y, x, c])));
